;;;; fasl-compiler.scm
;;;; June 6th, 2006
;;;; Mike Schaeffer
;;;
;;; A variant of compiler.scm that emits a FASL file, rather than a text file

(define-package "fasl-compiler"
  (:uses "scheme")
  (:exports "compile-file"
            "*verbose*"
            "*output-file-name*"
            "*debug*"
            "*show-meanings*"
            "*show-expansions*"
            "*show-actions*"
            "*files-to-compile*"
            "*initial-package*"
            "*cross-compile*"
            ))


(define *cross-compile* #f)

;;;; Compiler diagnostics

(define *compiler-output-port* (current-error-port))
(define *compiler-error-port* (current-error-port))

(define (trace-message write? . format-args)
  "Write a compiler trace message if <write?> is true. <format-args> are passed
   into format to generate the output text, written to the compiler output port.
   If <write?> is #f, nothing is done."
  (when write?
    (trace-indent *compiler-output-port*)
    (apply format *compiler-output-port* format-args)))

(define (call-with-compiler-tracing trace? label fn . args)
  (let ((trace? (and trace? (list? args))))
    (define (parse-label)
      (cond ((string? label) (values label "TO"))
            ((list? label)
             (case (length label)
               ((0) (values "FROM" "TO"))
               ((1) (values (car label) "TO"))
               (#t (values (car label) (cadr label)))))
            (#t
             (error "Invalid trace label: ~s" label))))
    (define (message prefix label args)
      (when trace?
        (trace-indent *compiler-output-port*)
        (format  *compiler-output-port* "~a ~a:" prefix label)
        (dolist (arg args)
          (format *compiler-output-port* " ~s"arg))
        (newline *compiler-output-port*)))
    (if trace?
        (values-bind (parse-label) (from-label to-label)
          (message ">" from-label args)
          (values-bind (in-trace-level (apply fn args)) results
            (message "<" to-label results)
            (apply values results)))
        (apply fn args))))

;;;; Compiler environment


(define *show-expansions* #f)
(define *show-meanings* #f)
(define *show-actions* #f)
(define *debug* #f)
(define *verbose* #f)
(define *initial-package* "user")


(define (compiler-evaluate form genv)
  "Evaluates <form> in global environment <genv>, signaling a compiler-error in
   the event of a failure."
  (catch 'end-compiler-evaluate
    (trace-message *show-actions* "==> COMPILER-EVALUATE: ~s genv=~@\n" form genv)
    (handler-bind  ((runtime-error
                     (if *debug*
                         handle-runtime-error
                         (lambda args
                           (compile-error form "Runtime error while evaluating toplevel form: ~s" args)
                           (throw 'end-compiler-evaluate)))))
      (eval form () genv))))



(define (symbol-value-with-bindings symbol bindings :optional (unbound-value #f))
  (check symbol? symbol)
  (if (symbol-bound? symbol () bindings)
      (symbol-value symbol () bindings)
      unbound-value))

;;;; Ways to signal compilation events

(define (compile-warning context-form message . args)
  (signal 'compile-warning context-form message args))

(define (compile-error context-form message . message-args)
  (signal 'compile-error context-form #f message message-args))

(define (compile-fatal-error context-form message . message-args)
  (signal 'compile-error context-form #t message message-args))

;;;; The expander
;;;;
;;;; This is the the initial phase of compilation. It takes raw source s-exprs
;;;; and expands any user macros or special forms into the base language.

(define (apply-expander expander form genv at-toplevel?)
  (call-with-compiler-tracing (and *show-expansions* (pair? form)) 
      (if at-toplevel?
          '("EXPAND-TOPLEVEL" "INTO-TOPLEVEL")
          '("EXPAND" "INTO"))
      (lambda (form) (expander form genv at-toplevel?))
    form))

(define (compiler-macroexpand-1 form genv at-toplevel?)
  (aif (and (pair? form)
            (symbol? (car form))
            (macro? (symbol-value-with-bindings (car form) genv)))
       (values #t
               (apply-expander (lambda (form genv)
                                 (let ((transformer (scheme::%macro-transformer it)))
                                   (if genv
                                       (with-global-environment genv
                                         (transformer form ()))
                                       (transformer form ()))))
                               form genv at-toplevel?))
       (values #f form)))

(define (compiler-macroexpand form genv at-toplevel?)
  (values-bind (compiler-macroexpand-1 form genv at-toplevel?) (expanded? expanded-form)
    (if expanded?
        (compiler-macroexpand expanded-form genv at-toplevel?)
        form)))

(define (scheme::macroexpand-1 form)
  "Apply the macro expansion pass of the compiler to <form>, but only for one
   time. Two values are returned: the first is a boolean indicating if a
   a macro was expanded and the second is the result of that expansion."
  (compiler-macroexpand-1 form #f #f))

(define (scheme::macroexpand form)
  "Apply the macro expansion pass of the compiler to <form>, and repeat until
   no more macros apply. The return value is the result of this full expansion."
  (compiler-macroexpand form #f #f))

(define (maybe-expand-user-macro form genv at-toplevel?)
  (catch 'end-compiler-macroexpand
    (handler-bind
        ((runtime-error
          (if *debug*
              handle-runtime-error
              (lambda (message args . rest)
                (compile-error form (format #f "Macro signaled error: ~I" message args) args)
                (throw 'end-compiler-macroexpand (values #f ()))))))
      (compiler-macroexpand-1 form genv at-toplevel?))))

(define (translate-form-sequence forms allow-definitions? genv at-toplevel?)
  "Translates a sequence of forms into another sequence of forms by removing
   any nested begins or defines."
  ;; Note that this would be an expansion step, were it not for the fact
  ;; that this takes a list of forms and produces a list of forms. (Instead
  ;; of form to form.)
  (define (begin-block? form) (and (pair? form) (eq? (car form) 'begin)))
  (define (define? form) (and (pair? form) (eq? (car form) 'scheme::%define)))
  (define (define-binding-pair form) (cons (cadr form) (cddr form)))

  (let expand-next-form ((remaining-forms forms)
                         (local-definitions ())
                         (body-forms ()))

    (let ((next-form (compiler-macroexpand (car remaining-forms) genv at-toplevel?)))
      (cond
       ((begin-block? next-form)
        (expand-next-form (append (cdr next-form) (cdr remaining-forms))
                          local-definitions
                          body-forms))
       ((define? next-form)
        (unless allow-definitions?
         (compile-error next-form "Definitions not allowed here."))

        (cond (at-toplevel?
               ;; We allow definitions anywhere in a toplevel form, because they
               ;; are not transformed into a letrec.
               (expand-next-form (cdr remaining-forms)
                                 local-definitions
                                 (append body-forms (cons next-form))))
              (#t
               (unless (null? body-forms)
                 (compile-error next-form "Local defines must be the first forms in a block."))
               (expand-next-form (cdr remaining-forms)
                                 (cons (define-binding-pair next-form) local-definitions)
                                 body-forms))))

       ((null? remaining-forms)
        (if (null? local-definitions)
            `(,@(map #L(expand-form _ genv at-toplevel?) body-forms))
            (expand-form
             `((letrec ,local-definitions
                 ,@body-forms))
             genv
             at-toplevel?)))
       (#t
        (expand-next-form (cdr remaining-forms)
                          local-definitions
                          (append body-forms (cons next-form))))))))


(define (expand/if form genv at-toplevel?)
  (unless (or (length=3? form) (length=4? form))
    (compile-error form "Invalid if, bad length."))
  (map #L(expand-form _ genv at-toplevel?) form))

(define (expand/begin form genv at-toplevel?)
  `(begin ,@(translate-form-sequence (cdr form) #f genv at-toplevel?)))

(define (valid-lambda-list? lambda-list)
  (or (symbol? lambda-list)
      (null? lambda-list)
      (and (pair? lambda-list)
           (symbol? (car lambda-list))
           (valid-lambda-list? (cdr lambda-list)))))

(define (valid-variable-list? vars)
  (or (null? vars)
      (and (pair? vars)
           (symbol? (car vars))
           (valid-variable-list? (cdr vars)))))

(define (expand/%lambda form genv at-toplevel?)
  (unless (or (list? (cadr form)) (null? (cadr form)))
    (compile-error form "Invalid %lambda, expected property list"))
  (unless (valid-lambda-list? (caddr form))
    (compile-error form "Invalid %lambda, bad lambda list"))
  `(scheme::%lambda ,(cadr form) ,(caddr form)
      ,@(translate-form-sequence (cdddr form) #t genv #f)))

(define (expand/%tlambda form genv at-toplevel?)
  `(scheme::%lambda () () ,@(translate-form-sequence (cdr form) #t genv #t)))

(define (expand/set! form genv at-toplevel?)
  (unless (length=3? form)
    (compile-error "Invalid set!, bad length." form))
  `(set! ,(cadr form) ,(expand-form (caddr form) genv at-toplevel?)))


(define (parse-eval-when form)
  (unless (> (length form) 2)
    (compile-error form "Incomplete eval-when."))
  (let ((situations (cadr form))
        (forms (cddr form)))
    (unless (and (or (null? situations) (list? situations))
                 (every? #L(member _ '(:compile-toplevel :load-toplevel :execute)) situations))
      (compile-error form "Bad situations list, situations must be :compile-toplevel, :load-toplevel, or :execute."))
    (values situations forms)))

(define (expand/eval-when form genv at-toplevel?)
  (values-bind (parse-eval-when form) (situations forms)
    (if (member :load-toplevel situations)
        `(begin ,@(translate-form-sequence forms #t genv at-toplevel?))
        #f)))

(define (expand/case form genv at-toplevel?)
  (unless (>= (length form) 3)
    (compile-error form "Invalid case, bad length."))
  (unless (every? (lambda (case-clause)
                    (or (eq? case-clause #t)
                        (eq? case-clause 'else)
                        (and (list? case-clause) (> (length case-clause) 1))))
                  (cddr form))
    ;; REVISIT: a body-less case clause throws a compile error, and it's inscrutible
    (compile-error form "Invalid case, bad clause"))

  `(case ,(cadr form)
     ,@(map (lambda (case-clause)
              `(,(car case-clause) ,@(map #L(expand-form _ genv at-toplevel?) (cdr case-clause))))
            (cddr form))))

(define (expand/cond form genv at-toplevel?)
  (unless (>= (length form) 2)
    (compile-error form "Invalid cond, bad length."))
  (unless (every? (lambda (cond-clause)
                    (or (eq? cond-clause #t)
                        (eq? cond-clause 'else)
                        (list? cond-clause)))
                  (cdr form))
    (compile-error form "Invalid cond, bad clause."))

  `(cond ,@(map (lambda (cond-clause) (map #L(expand-form _ genv at-toplevel?) cond-clause)) (cdr form))))

(define (expand/logical form genv at-toplevel?)
  `(,(car form) ,@(map #L(expand-form _ genv at-toplevel?) (cdr form))))

(define (form-expander form genv at-toplevel?)
  (cond ((null? form)
         ())
        ((list? form)
         (case (car form)
           ((quote)               form)
           ((or and)              (expand/logical     form genv at-toplevel?))
           ((case)                (expand/case        form genv at-toplevel?))
           ((cond)                (expand/cond        form genv at-toplevel?))
           ((if)                  (expand/if          form genv at-toplevel?))
           ((scheme::%lambda)     (expand/%lambda     form genv at-toplevel?))
           ((scheme::%tlambda)    (expand/%tlambda    form genv at-toplevel?))
           ((set!)                (expand/set!        form genv at-toplevel?))
           ((begin)               (expand/begin       form genv at-toplevel?))
           ((eval-when)           (expand/eval-when   form genv at-toplevel?))
           (#t
            (values-bind (maybe-expand-user-macro form genv at-toplevel?) (expanded? expanded-form)
              (cond (expanded?
                     (expand-form expanded-form genv at-toplevel?))
                    ((atom? expanded-form)
                     (expand-form expanded-form genv at-toplevel?))
                    (#t
                     (map #L(expand-form _ genv at-toplevel?) form)))))))
        ((symbol? form) form)
        ((atom? form)   form)
        (#t             (error "Don't know how to expand this form: ~s" form))))

(define (expand-form form genv at-toplevel?)
  (apply-expander form-expander form genv at-toplevel?))

;;;; Form Semantic Analysis

(define (l-list-vars l-list)
  (cond ((null? l-list) ())
        ((atom? l-list) `(,l-list))
        (#t (cons (car l-list) (l-list-vars (cdr l-list))))))

(define (extend-cenv l-list cenv)
  (cons (l-list-vars l-list) cenv))

(define (bound-in-cenv? var cenv)
  (let loop ((rest cenv))
    (cond ((null? rest) #f)
          ((atom? rest) (error "Malformed cenv: ~s" cenv))
          (#t
           (let ((cenv-frame (car rest)))
             (unless (list? cenv-frame)
               (error "Malformed frame ~s in cenv: ~s" cenv-frame cenv))
             (if (memq var (car rest))
                 #t
                 (loop (cdr rest))))))))

(define (meaning/%macro defn cenv genv at-toplevel?)
  `(scheme::%macrocons ,(form-meaning (second defn) () genv at-toplevel?)))

(define (meaning/%lambda defn cenv genv at-toplevel?)

  (define (code-body-form body-forms)
    "Return a single form that is semantically equivalent to <body-forms>."
    (case (length body-forms)
      ((0) '(values))
      ((1) (first body-forms))
      (#t `(begin ,@body-forms))))

  (unless (and (list? defn) (>= (length defn) 3))
    (error "Invalid function syntax: ~s" defn))
  (dbind (fn-pos p-list l-list . body) defn
    (let ((body-form (form-meaning (code-body-form body)
                                   (extend-cenv l-list cenv)
                                   genv
                                   at-toplevel?)))
      (if (null? cenv)
          (scheme::%closure () (cons l-list body-form) p-list)
          ;; The compiler does not reify closures if they are defined in
          ;; the context of local variables. The interpreter must reify at
          ;; runtime to capture the correct variable bindings in the closure's
          ;; environment.
          `(system::%%lambda ,p-list ,l-list ,body-form)))))

(define (meaning/application form cenv genv at-toplevel?)
  (map #L(form-meaning _ cenv genv at-toplevel?) form))

(define (meaning/begin form cenv genv at-toplevel?)
  `(system::%%begin ,@(map #L(form-meaning _ cenv genv at-toplevel?) (cdr form))))

(define (meaning/or form cenv genv at-toplevel?)
  `(system::%%or ,@(map #L(form-meaning _ cenv genv at-toplevel?) (cdr form))))

(define (meaning/and form cenv genv at-toplevel?)
  `(system::%%and ,@(map #L(form-meaning _ cenv genv at-toplevel?) (cdr form))))

(define (meaning/if form cenv genv at-toplevel?)
  `(system::%%if ,@(map #L(form-meaning _ cenv genv at-toplevel?) (cdr form))))

(define (meaning/set! form cenv genv at-toplevel?)
  (dbind (fn-pos var val-form) form
    (cond ((keyword? var)
           (compile-error form "Cannot rebind a keyword: ~s" var))
          ((bound-in-cenv? var cenv)
           `(system::%%set! ,var ,(form-meaning val-form cenv genv at-toplevel?)))
          (#t
           (scheme::assemble-fast-op :global-set! var (form-meaning val-form cenv genv at-toplevel?))))))

(define (meaning/cond form cenv genv at-toplevel?)
  `(system::%%cond
     ,@(map (lambda (cond-clause)
              `(,(form-meaning (car cond-clause) cenv genv at-toplevel?)
                ,@(map #L(form-meaning _ cenv genv at-toplevel?) (cdr cond-clause))))
            (cdr form))))

(define (meaning/case form cenv genv at-toplevel?)
  `(system::%%case ,(form-meaning (cadr form) cenv genv at-toplevel?)
     ,@(map (lambda (case-clause)
              `(,(car case-clause)
                ,@(map #L(form-meaning _ cenv genv at-toplevel?) (cdr case-clause))))
            (cddr form))))

(define (meaning/%define form cenv genv at-toplevel?)
  (dbind (fn-pos name defn) form
    `(scheme::%define-global ,(scheme::assemble-fast-op :literal name)
                             ,(form-meaning defn cenv genv at-toplevel?)
                             ,(scheme::assemble-fast-op :literal genv))))

(define (meaning/quote form cenv genv at-toplevel?)
  (scheme::assemble-fast-op :literal (cadr form)))

(define (meaning/symbol form cenv genv at-toplevel?)
  (cond ((keyword? form)
         (scheme::assemble-fast-op :literal form))
        ((bound-in-cenv? form cenv)
         form)
        (#t
         (scheme::assemble-fast-op :global-ref form))))

(define (form-meaning form cenv genv at-toplevel?)
  (call-with-compiler-tracing *show-meanings* '("MEANING-OF" "IS")
    (lambda (form)
      (cond ((symbol? form)
             (meaning/symbol form cenv genv at-toplevel?))
            ((atom? form)
             form)
            (#t
             (case (car form)
               ((scheme::%macro)   (meaning/%macro      form cenv genv at-toplevel?))
               ((scheme::%lambda)  (meaning/%lambda     form cenv genv at-toplevel?))
               ((begin)            (meaning/begin       form cenv genv at-toplevel?))
               ((or)               (meaning/or          form cenv genv at-toplevel?))
               ((and)              (meaning/and         form cenv genv at-toplevel?))
               ((if)               (meaning/if          form cenv genv at-toplevel?))
               ((cond)             (meaning/cond        form cenv genv at-toplevel?))
               ((case)             (meaning/case        form cenv genv at-toplevel?))
               ((set!)             (meaning/set!        form cenv genv at-toplevel?))
               ((scheme::%define)  (meaning/%define     form cenv genv at-toplevel?))
               ((quote)            (meaning/quote       form cenv genv at-toplevel?))
               (#t                 (meaning/application form cenv genv at-toplevel?))))))
    form))

;;;; Form Compiler

(define (compile-form form :optional (genv #f))
  (form-meaning (expand-form form genv #f) () genv #f))

(define (compile-toplevel-form form :optional (genv #f))
  (form-meaning (expand-form form genv #t) () genv #t))

;;;; File Compiler

;;; The file reader

(define *compiler-reader* read)
(define *compiler-location-map* #f)


(define (compile-read-error message port port-location)
  (signal 'compile-read-error message port port-location))


(define (port-location-string port :optional (port-location ()))
  (if (null? port-location)
      (format #f "~a(...)" (port-name port))
      (format #f "~a(~a, ~a)" (port-name port) (car port-location) (cdr port-location))))

(define (form-location-string form)
  (aif (and (hash? *compiler-location-map*)
            (hash-ref *compiler-location-map* form #f))
       (port-location-string (car it) (cdr it))
       "(...)"))

(define (compiler-read port genv)
  (let ((loc (begin
               (flush-whitespace port #t)
               (port-location port))))
    (handler-bind ((read-error (lambda (message port loc)
                                 (compile-read-error message port loc))))
      (dynamic-let ((*location-mapping* *compiler-location-map*)
                    (*package* (symbol-value '*package* () genv)))
        (trace-message *show-actions* "* READ in ~s genv=~@\n" *package* genv)
        (*compiler-reader* port #f))))) ; REVISIT #. eval/read forms are not evaluated in genv

;;; The evaluator

(define (compiler-define var val genv)
  (trace-message *show-actions* "==> COMPILER-DEFINE: ~s := ~s genv=~@\n" var val genv)
  (scheme::%define-global var val genv))

;;; The main loop

(define (form-list-reader forms)
  "Makes a reader for the list of forms <forms>. A reader is a closure
   that returns a form on each call and an eof-object when there are no
   more forms to be read."
  (lambda ()
    (if (null? forms)
        (scheme::%make-eof)
        (let ((form (car forms)))
          (set! forms (cdr forms))
          form))))

(define *output-stream* #f)

(define (process-toplevel-eval-when form load-time-eval? compile-time-eval? genv)
  (values-bind (parse-eval-when form) (situations forms)
    (let ((load-time-eval? (and load-time-eval? (member :load-toplevel situations)))
          (compile-time-eval? (or (member :compile-toplevel situations)
                                  (and compile-time-eval?
                                       (member :execute-toplevel situations)))))
      (when (or load-time-eval? compile-time-eval?)
        (process-toplevel-forms (form-list-reader forms) load-time-eval? compile-time-eval? genv)))))




(define (process-toplevel-include form genv)
  (unless (and (list? form) (length=2? form) (string? (second form)))
    (compile-error #f "Invalid include form: ~s" form))
  (let ((file-spec (second form)))
    (define (file-spec-files)
      (if (wild-glob-pattern? file-spec)
          (directory file-spec)
          (list file-spec)))
    (dolist (filename (file-spec-files))
      (call-with-compiler-tracing *show-actions* '("BEGIN-INCLUDE" "END-INCLUDE")
                                  (lambda (filename)
                                    (when (currently-compiling-file? filename)
                                      (compile-fatal-error #f "Recursive include of ~s while compiling ~s"
                                                           filename *files-currently-compiling*))
                                    (compile-file/simple filename genv))
        filename))))

(define (process-toplevel-form form load-time-eval? compile-time-eval? genv)
  (trace-message *show-actions* "* PROCESS-TOPLEVEL-FORM~a~a: ~s\n"
                 (if load-time-eval? " [load-time]" "")
                 (if compile-time-eval? " [compile-time]" "")
                 form)
  (cond ((pair? form)
         (case (car form)
           ((scheme::%define)
            (let ((var (second form))
                  (val (compile-form (third form) genv)))

            ;; error checking here???
            (compiler-define var (compiler-evaluate val genv) genv)
            (emit-definition var val genv)))
           ((begin)
            (process-toplevel-forms (form-list-reader (cdr form)) load-time-eval? compile-time-eval? genv))
           ((include)
            (process-toplevel-include form genv))
           ((eval-when)
            (process-toplevel-eval-when form load-time-eval? compile-time-eval? genv))
           (#t
            (values-bind (maybe-expand-user-macro form genv #t) (expanded? expanded-form)
              (cond (expanded?
                     (process-toplevel-form expanded-form load-time-eval? compile-time-eval? genv))
                    (#t
                     (when compile-time-eval?
                       (compiler-evaluate form genv))
                     (when load-time-eval?
                       (emit-action form *output-stream* genv))))))))))

(define (process-toplevel-forms reader load-time-eval? compile-time-eval? genv)
  (let loop ((next-form (reader)))
    (unless (eof-object? next-form)
      (process-toplevel-form next-form load-time-eval? compile-time-eval? genv)
      (loop (reader)))))

(define (expand-port-forms ip *output-stream* genv)
  (process-toplevel-forms (lambda () (compiler-read ip genv)) #t #f genv))

;;; FASL file generaiton

(define (form->compiled-procedure form genv)
  "Compute a compiled procedure that evaluates <form> and returns the
   result of that evaluation."
  (compile-form `(lambda () ,form) genv))

(define (emit-action form *output-stream* genv)
  (trace-message *show-actions* "==> EMIT-ACTION: ~s\n" form)
  (fasl-write-op scheme::FASL-OP-LOADER-APPLY0 (list (form->compiled-procedure form genv)) *output-stream*))

(define (evaluated-object? obj)
  "Returns true if <obj> is an object that has specific handling in the scheme
   evaluator. If <obj> evaluates to itself, returns #f."
  (or (scheme::fast-op? obj)
      (symbol? obj)
      (pair? obj)))

(define (emit-definition var val genv)
  (trace-message *show-actions*"==> EMIT-DEFINITION: ~s := ~s\n" var val)
  (trace-message *verbose* "; defining ~a\n" var)
  (if (evaluated-object? val)
      (fasl-write-op scheme::FASL-OP-LOADER-DEFINEA0 (list var (form->compiled-procedure val genv)) *output-stream*)
      (fasl-write-op scheme::FASL-OP-LOADER-DEFINEQ (list var val) *output-stream*)))

;;; Error reporting

(define (end-compile-abnormally return-code *output-stream*)
  (abort-fasl-writes *output-stream*)
  (throw 'end-compile-now return-code))

(define (compiler-message context-desc message-type message message-args)
  "Display a compiler message to the compiler error port. <context-desc> is a description
   of the context for the message. <message-type> is the type of message to be written.
   <message> is a format string with the message text and <message-args> is a list of
   arguments to the message format string."
  (format *compiler-error-port* "~a ~a - ~I\n" context-desc message-type message message-args))

(define (compiler-message/form context-form message-type message message-args)
  "Display a compiler message to the compiler error port. <context-form> is a form
   that provides context for the message, if appropriate. <message-type> is the
   type of message to be written. <message> is a format string with the message text
   and <message-args> is a list of arguments to the message format string."
  (compiler-message (form-location-string context-form) message-type message message-args))

;;; Compilation setup


(define *files-currently-compiling* ())

(define (currently-compiling-file? filename)
  (if (any? #L(filename-string=? _ filename) *files-currently-compiling*)
      filename
      #f))

;; TODO: dynamic-let in a specific global environment

(define (compile-file/simple filename genv)
  ;; REVISIT: Logic to restore *package* after compiling a file. Ideally, this should
  ;; match the behavior of scheme::call-as-loader, although it is unclear how this
  ;; relates to the way we do cross-compilation.
  (let ((original-package (symbol-value '*package* () genv)))
    (dynamic-let ((*files-currently-compiling* (cons filename *files-currently-compiling*)))
      (trace-message #t "; Compiling file: ~a\n" filename)
      (with-port input-port (open-input-file filename)
          (fasl-write-op scheme::FASL-OP-BEGIN-LOAD-UNIT (list filename) *output-stream*)
          (expand-port-forms input-port *output-stream* genv)
          (fasl-write-op scheme::FASL-OP-END-LOAD-UNIT (list filename) *output-stream*)))
    (set-symbol-value! '*package* original-package () genv)))

(define (compile-file/checked filename genv)
  (let ((compile-error-count 0))
    (handler-bind ((compile-read-error
                    (lambda (message port port-location)
                      (compiler-message (port-location-string port port-location) :read-error message ())
                      (end-compile-abnormally 1 *output-stream*)))
                   (compile-error
                    (lambda (context-form fatal? message details)
                      (compiler-message/form context-form :error message details)
                      (incr! compile-error-count)
                      (when fatal?
                        (end-compile-abnormally 1 *output-stream*))))
                   (compile-warning
                    (lambda (context-form message args)
                      (compiler-message/form context-form :warning message args))))
      (compile-file/simple filename genv)
      compile-error-count)))

(defmacro (with-compiler-output-stream os filename . code)
  (with-gensyms (output-port-sym)
    `(with-port ,output-port-sym (open-output-file ,filename :binary)
       (with-fasl-stream ,os ,output-port-sym
         (dynamic-let ((*output-stream* ,os))
           ,@code)))))

(define (compile-files filenames output-file-name :optional (genv #f))
  (with-compiler-output-stream output-fasl-stream  output-file-name
     (let next-file ((filenames filenames) (error-count 0))
       (cond ((not (null? filenames))
              (next-file (cdr filenames)
                         (+ error-count (compile-file/checked (car filenames) genv))))
             ((> error-count 0)
              (format *compiler-error-port* "; ~a error(s) detected while compiling.\n" error-count)
              (end-compile-abnormally 2 output-fasl-stream))
             (#t
              ())))))

(define (input-file-name->output-file-name input-filename)
  (cond ((list? input-filename)
         (if (length=1? input-filename)
             (input-file-name->output-file-name (first input-filename))
             "a.scf"))
        ((string? input-filename)
         (string-append (filename-no-extension input-filename) ".scf"))
        (#t
         (error "Invalid input filename: ~s" input-filename))))

;; This is an example of how another form of cross compilation might work
;;
;; (define (setup-cross-compiler/package-renaming)
;;   "Setup for cross compiling using renamed packages."
;;   (format #t "; Configuring for cross compile by renaming packages.\n")
;;   (let ((excluded-packages (map find-package '("system" "keyword"))))
;;     (dolist (p (list-all-packages))
;;       (unless (memq p excluded-packages)
;;         (rename-package! p (string-append "host-" (package-name p))))))
;;   (make-package! "scheme")
;;   (make-package! "user")
;;   (use-package! "system" "scheme")
;;   (use-package! "scheme" "user")
;;   (in-package! "scheme")
;;   (load-internal "s-core")
;;   (set! fasl-compiler::*compiler-reader*
;;         (symbol-value (intern! "read" (find-package "scheme")))))

(define (compile-file filename :optional (output-file-name #f))
  (let ((output-file-name (cond ((string? output-file-name) output-file-name)
                                ((not output-file-name) (input-file-name->output-file-name filename))
                                (#t (error "Invalid output filename: ~s" filename))))
        (filenames (if (list? filename) filename (list filename))))
    (set! *compiler-location-map* (make-hash :eq))
    (catch 'end-compile-now
      (handler-bind ((runtime-error
                      (if *debug*
                          handle-runtime-error
                          (lambda (message args)
                            ;;(show-runtime-error message args)
                            (format *compiler-error-port*
                                    "\n\n\nINTERNAL COMPILER ERROR!, message=~s\n\targs=~s\n"
                                    message args)
                            (throw 'end-compile-now 127)))))

        (let* ((compiler-genv (scheme::%current-global-environment))
               (target-genv (if *cross-compile*
                                (copy-global-environment :compiler-target-bindings)
                                compiler-genv)))

          (trace-message *verbose* "; global-genv=~@, target-genv=~@\n"
                         compiler-genv target-genv)
  
          (compile-files filenames output-file-name target-genv))

        (format *compiler-output-port* "; Compile completed successfully.\n"))
      0)))
