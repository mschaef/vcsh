;;;; compiler.scm
;;;; November 3rd, 2007
;;;; Mike Schaeffer
;;;;
;;;; The bytecode compiler.
;;;;
;;;; The compiler works in several distinct phases:
;;;;
;;;; 1) form-meaning computes the 'meaning' of the form being compiled.
;;;;    Basically, this amounts to expanding all user macros and reducing
;;;;    some of the synactic nicities of the languauge to a more primative
;;;;    form of scheme consisting only of function calls and a few special
;;;;    forms.
;;;;
;;;; 2) The flattener converts this simplified dialect of scheme into
;;;;    symbolic assembly code for the byte code assembler.
;;;;
;;;; 3) The bytecode assembler assembles the symbolic assembly into a sequence
;;;;    of bytes.
;;;;
;;;; Note: Steps 2 and 3 do not yet exist!

(scheme:define-package "compiler"
                       (:uses "scheme")
                       (:exports "compile"))

(define *debug* #f)

;;; Error reporting

(define *compiler-location-map* #f)

(define (compiler-message loc-string message-class message :optional (args ()))
  (format (current-error-port) "\n~a: ~a: ~I" loc-string message-class message args))

(define (problem severity form message . args)
  (signal 'compile-problem severity form message args))

(define (port-location-string port :optional (port-location ()))
  (if (null? port-location)
      (format #f "~a(...)" (port-name port))
      (format #f "~a(~a, ~a)" (port-name port) (car port-location) (cdr port-location))))

(define (form-location-string form)
  (aif (and (hash? *compiler-location-map*)
            (hash-ref *compiler-location-map* form #f))
       (port-location-string (car it) (cdr it))
       "(...)"))


;;; Compiler macro definitions and special forms

(define *special-forms* '(%%lambda %%cond %%extend-env %%sequence %%varset!
                          %%list-let %%and %%or %%funcall %%funcall/tail
                          %%varref %%constant))


(define (special-form? form)
  (and (pair? form)
       (symbol? (car form))
       (member (car form) *special-forms*)))

;;; User macros

(define (user-macro-form? form)
  "Determines if <form> is a form that is to be expanded by a user defined
   macro. If so returns the macro, otherwise returns #f."
  (and (pair? form)
       (symbol? (car form))
       (symbol-bound? (car form))
       (macro? (symbol-value (car form)))))


(define (apply-user-macro macro form)
  "Applies the macro <macro> to <form>, returning the expanded form."
  (check macro? macro)
  (catch 'end-compiler-macroexpand
    (handler-bind
        ((runtime-error
          (if *debug*
              handle-runtime-error
              (lambda (message args . rest)
                (problem :error form
                         (format #f "Macro signaled error: ~I" message args) args)
                (throw 'end-compiler-macroexpand ())))))
      ((%macro-transformer macro) form ()))))

(define (fully-apply-user-macros form)
  "Iteratively transform <form> with user macros only, until there are
   no more such transformations to be made."
  (aif (user-macro-form? form)
       (fully-apply-user-macros (apply-user-macro it form))
       form))

;;; Compiler macro definitions - These define the basic syntax the compiler understands

(define (begin-block? form)
  (and (pair? form) (eq? (car form) 'begin)))

(define (define? form)
  (and (pair? form) (eq? (car form) '%define)))

(define (meaning-if form value? tail?)
  (list-let (test-clause . alternatives) (cdr form)
    (unless (or (length=1? alternatives)
                (length=2? alternatives))
      (problem :error form "Invalid alternatives to if: ~s" alternatives))
    `(%%cond (,(form-meaning test-clause #t #f)
             ,(form-meaning (car alternatives) value? tail?))
             ((%%constant #t)
             ,(form-meaning (cadr alternatives) value? tail?)))))

(define (simplify-begin form value? tail?)
  "Simplify a block of code, flattening nested begins and extracting
   a list of all internal definitions. Returns two values: a list of
   all body forms and a list of all internal definitions."
  (let loop ((internal-defns ()) (body-forms ()) (remaining form))
    (if (null? remaining)
        (values (reverse body-forms) internal-defns)
        (let ((next (fully-apply-user-macros (car remaining))))
          (cond ((define? next)
                 (unless (null? body-forms)
                   (problem :error form "Internal definitions must be at the beginning of a form." form))
                 (loop (cons next internal-defns) body-forms (rest remaining)))
                ((begin-block? next)
                 (loop internal-defns body-forms (append (cdr next) (rest remaining))))
                (#t
                 (loop internal-defns (cons next body-forms) (rest remaining))))))))

(define (sequence-meaning forms value? tail?)
  (let ((meanings (let recur ((forms forms)) ; !! map-pair would make this much simpler
                    (if (null? forms)
                        ()
                        (let* ((last-form? (null? (cdr forms)))
                               (meaning (form-meaning (car forms)
                                                      (and last-form? value?)
                                                      (and last-form? tail?))))
                          (if (null? meaning)
                              (recur (cdr forms))
                              (cons meaning (recur (cdr forms)))))))))
    (case (length meanings)
      ((0) ())
      ((1) (car meanings))
      (#t (cons '%%sequence meanings)))))

(define (meaning-begin form value? tail?)
  (values-bind (simplify-begin (cdr form) value? tail?) (body-forms internal-defns)
    (cond ((not (null? internal-defns))
           (form-meaning `(letrec ,(map cdr internal-defns) ,@body-forms) value? tail?))
          ((length=1? body-forms)
           (form-meaning (first body-forms) value? tail?))
          (#t
           (sequence-meaning body-forms value? tail?)))))



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

(define (meaning-%lambda form value? tail?)
  (list-let (property-list lambda-list . body) (cdr form)
    (unless (list? property-list)
      (problem :error form "Invalid %lambda, expected property list: ~s" property-list))
    (unless (valid-lambda-list? lambda-list)
      (problem :error form "Invalid %lambda, bad lambda list: ~s" lambda-list))
    `(%%lambda ,property-list ,lambda-list
               (%%sequence ,(form-meaning `(begin ,@body) #t #t)))))


(define (meaning-set! form value? tail?)
  (list-let (var value) (cdr form)
    `(%%varset! ,var ,(form-meaning value #t tail?))))

(define (meaning-%extend-env form value? tail?)
  (list-let (actuals formals . body) (cdr form)
    (unless (valid-variable-list? actuals)
      (problem :error form "Invalid %extend-env, bad actuals list: ~s" actuals))
    (unless (= (length actuals) (length formals))
      (problem :error form "Invalid %extend-env, number of actuals ~s  must equal number of binding forms ~s" actuals formals))
    `(%%extend-env ,actuals ,(map #L(form-meaning _ #t #f) formals)
                   ,(form-meaning `(begin ,@body value? tail?)))))

(define (meaning-list-let form value? tail?)
  (list-let (formals list-expr . body) (cdr form)
    (unless (valid-lambda-list? formals)
      (problem :error form "Invalid list-let, bad variable list." actuals))
    `(%%list-let ,formals ,(form-meaning list-expr #t #f)
                 ,(form-meaning `(begin ,@body) value? tail?))))

(define (valid-eval-when-situations-list? situations)
  (and (list? situations)
       (every? #L(member _ '(:compile-toplevel
                                :load-toplevel
                                :execute))
               situations)))

(define (meaning-eval-when form value? tail?)
  (list-let (situations . body) (cdr form)
    (unless (valid-eval-when-situations-list? situations)
      (problem :error form "Bad situations list, each situation must be one of :compile-toplevel, :load-toplevel, or :execute." situations))
    (if (member :load-toplevel situations)
        `(begin ,@body)
        #f)))


(define (valid-case-clause? clause)
  "Determines if <clause> is a valid case clause. Returns :default, if it's
   a default clause, :conditional if it's conditional, and #f if it's not valid."
  (and (list? clause)
       (> (length clause) 1)
       (or (and (member (car clause) '(#t else)) :default)
           (and (list? (car clause)) :conditional))))

(define (meaning-case form value? tail?)
  (list-let (value-form . clauses) (cdr form)
    (with-gensyms (value-sym)
      (form-meaning
       `(let ((,value-sym ,(form-meaning value-form #t #f)))
          (cond ,@(map (lambda (clause)
                         (case (valid-case-clause? clause)
                           ((:conditional)
                            `((member ,value-sym ',(car clause)) ,@(cdr clause)))
                           ((:default)
                            `(#t ,@(cdr clause)))
                           (#t
                            (problem :error form "Bad case clause: ~s" clause))))
                       clauses)))
       value? tail?))))

(define (valid-cond-clause? clause)
  (and (list? clause)
       (> (length clause) 1)))

(define (meaning-cond form value? tail?)
  (let ((clauses (cdr form)))
    (awhen (find (negate valid-cond-clause?) clauses)
           (problem :error form "Bad cond clause: ~s" it))
    `(%%cond ,@(map (lambda (clause)
                      (let ((ctest (car clause))
                            (cbody (cons 'begin (cdr clause))))
                        (list (form-meaning ctest #t #f)
                              (form-meaning cbody value? tail?))))
                    clauses))))

(define (meaning-and forms value? tail?) ; !! should emit tail-recursive code
  `(%%and ,@(map #L(form-meaning _ #t #f) (cdr forms))))

(define (meaning-or forms value? tail?)
  `(%%or ,@(map #L(form-meaning _ #t #f) (cdr forms))))


(define (meaning-quote form value? tail?)
  (if value?
      `(%%constant ,(cadr form))
      ()))

(define (meaning-funcall form value? tail?)
  (list-let (fn-pos . args) form
    (let ((fn-meaning (form-meaning fn-pos #t #f))
          (args-meanings (map #L(form-meaning _ #t #f) args)))
    (if tail?
        `(%%funcall/tail ,fn-meaning ,@args-meanings)
        `(%%funcall ,fn-meaning ,@args-meanings)))))

(define (meaning-const form value? tail?)
  (if value?
      `(%%constant ,form)
      ()))

(define (meaning-varref form value? tail?)
  (if value?
      `(%%varref ,form)
      ()))

(define (form-meaning form value? tail?)
  (acond ((null? form)
          (meaning-const () value? tail?))
         ((special-form? form)
          form)
         ((user-macro-form? form)
          (form-meaning (apply-user-macro it form) value? tail?))
         ((list? form)
          (case (car form)
            ((%lambda) (meaning-%lambda form value? tail?))
            ((set!) (meaning-set! form value? tail?))
            ((%extend-env) (meaning-%extend-env form value? tail?))
            ((list-let) (meaning-list-let form value? tail?))
            ((begin) (meaning-begin form value? tail?))
            ((eval-when) (meaning-eval-when form value? tail?))
            ((if) (meaning-if form value? tail?))
            ((case) (meaning-case form value? tail?))
            ((cond) (meaning-cond form value? tail?))
            ((and) (meaning-and form value? tail?))
            ((or) (meaning-or form value? tail?))
            ((quote) (meaning-quote form value? tail?))
            (#t (meaning-funcall form value? tail?))))
         ((symbol? form)
          (meaning-varref form value? tail?))
         (#t
          (meaning-const form value? tail?))))

;;; The assembler

(define (make-lap)
  (make-queue))

(define (lap-code lap)
  (q-items lap))

(define (label name)
  (gensym (string-append "label-" name)))

(define (instr op . args)
  (cons op args))

(define (emit! lap . instrs)
  (for-each #L(q-enqueue! _ lap) instrs))

;;; The flattener

(define (compiler-env)
  "Create a new compiler environment. A compiler environment is an
   a-list used to maintain state while compiling. Among other things,
   it maintains a list of current lexical variable frames."
  (alist 'lenv ()))

(define (extend-compiler-lenv lexical-vars cenv)
  "Returns compiler environment <cenv> extended to include a new
   lexical frame with the variables in <lexical-vars>."
  (alist-cons 'lenv
              (cons lexical-vars (cdr (assq 'lenv cenv)))
              cenv))

(define (var-index var frame)
  (let loop ((ii 0) (frame frame))
    (cond ((null? frame)
           #f)
          ((symbol? frame)
           (if (eq? var frame)
               ii
               #f))
          ((eq? var (car frame))
           ii)
          (#t
           (loop (+ ii 1) (cdr frame))))))

(define (local-varref? var cenv)
  "Determines if <var> is a local variable reference within the compiler
   environment <cenv>. Local references are returnd as a pair of the
   form (<frame-index> . <var-index>)."
  (let next-frame ((frame-index 0) (frames (cdr (assq 'lenv cenv))))
    (if (null? frames)
        #f
        (aif (var-index var (car frames))
             (cons frame-index it)
             (next-frame (+ 1 frame-index) (cdr frames))))))


(define (flatten-constant code cenv lap)
  (emit! lap (instr 'const (second code))))

(define (flatten-funcall code cenv lap)
  (let ((fn-pos (car code))
        (args (reverse (cdr code))))
    (for-each #L(flatten-code _ cenv lap) args)
    (flatten-code fn-pos cenv lap)
    (emit! lap (instr 'apply (length args)))))

(define (flatten-%%sequence code cenv lap)
  (for-each #L(flatten-code _ cenv lap) (cdr code)))

(define (flatten-%%varref code cenv lap)
  (let ((var (cadr code)))
    (aif (local-varref? var cenv)
         (emit! lap (instr 'local-ref (car it) (cdr it)))
         (emit! lap (instr 'global-ref var)))))


(define (flatten-%%varset! code cenv lap)
  (let ((var (second code))
        (val (third code)))
    (flatten-code val cenv lap)
    (aif (local-varref? var cenv)
         (emit! lap (instr 'local-set! (car it) (cdr it)))
         (emit! lap (instr 'global-set! var)))))

(define (flatten-%%funcall code cenv lap)
  (let ((at-tail? (eq? (car code) '%%funcall/tail))
        (code (cdr code)))
    (when at-tail?
      (emit! lap (instr 'save-continuation)))
    (for-each #L(flatten-code _ cenv lap) (reverse (cdr code)))
    (flatten-code (car code) cenv lap)
    (emit! lap (instr 'call (length (cdr code))))))

(define (flatten-%%cond code cenv lap)
  (let ((end-label (label "cond-end")))
    (let next-clause ((clauses (cdr code))
                      (this-clause-label (label "cond"))
                      (next-clause-label (label "cond")))
      (cond ((null? clauses)
             (emit! lap end-label))
            (#t
             (let ((clause (first clauses)))
               (let ((condition (first clause))
                     (body (second clause)))
                 (emit! lap this-clause-label)
                 (flatten-code condition cenv lap)
                 (emit! lap (instr 'jump-if-false next-clause-label))
                 (flatten-code body cenv lap)
                 (emit! lap (instr 'jump end-label))
                 (next-clause (cdr clauses) next-clause-label (label "cond")))))))))


(define (flatten-embedded-%%lambda code cenv lap)
  (emit! lap (instr 'const (flatten-%%lambda code cenv))))

(define (flatten-code code cenv lap)
  (cond ((list? code)
         (case (car code)
           ((%%lambda) (flatten-embedded-%%lambda code cenv lap))
           ((%%constant) (flatten-constant code cenv lap))
           ((%%varref) (flatten-%%varref code cenv lap))
           ((%%varset!) (flatten-%%varset! code cenv lap))
           ((%%cond) (flatten-%%cond code cenv lap))
           ((%%extend-env) :extend-env)
           ((%%sequence) (flatten-%%sequence code cenv lap))
           ((%%list-let) :list-let)
           ((%%and) :and)
           ((%%or) :or)
           ((%%funcall %%funcall/tail) (flatten-%%funcall code cenv lap))
           (#t
            (error "Do not know how to flatten: ~s" code))))
        (#t
         (error "Do not know how to flatten: ~s" code))))

(define (lambda-calling-protocol lvars)
  "Given a lambda list, determine the implied calling protocol.
   Returns two values, the minimum number of arguments and whether
   or not the function has a rest argument."
  (if (symbol? lvars)
      (values 0 #t)
      (values (length lvars)
              (symbol? (cdr (last-pair lvars))))))

(define (flatten-%%lambda code cenv)
  (let ((lap (make-lap)))
    (let* ((lvars (third code))
           (body (cadddr code))
           (cenv (extend-compiler-lenv lvars cenv)))
      (values-bind (lambda-calling-protocol lvars) (min-args rest?)
        (emit! lap (instr (if rest? 'enter-frame/rest 'enter-frame) min-args))
        (flatten-code body cenv lap)
        (emit! lap (instr 'leave-frame))
        (emit! lap (instr 'resume-at-continuation)))
      (assemble-lap (q-items lap)))))

(define (assemble-lap lap)
  (%compiled-closure () (vector lap) ()))

;;; The main driver function

(define (form->code form)
  (cond ((primitive? form)
         (error "Primitive procedures cannot be compiled. ~s" form))
        ((procedure? form)
         `(lambda ,(car (%closure-code form)) ,(cdr (%closure-code form))))
        (#t
         form)))

(define (compile form)
  "Compiles the form <form>, returning the compiled representation."
  (let ((compile-error-count 0))
    (handler-bind ((compile-problem
                    (lambda (severity form message args)
                      (compiler-message (form-location-string form) severity message args)
                      (when (eq? severity :error)
                          (incr! compile-error-count)))))
      (let ((meaning (form-meaning (form->code form) #t #t)))
        (unless (and (pair? meaning) (eq? (car meaning) '%%lambda))
          (error "Cannot compile: ~s" form))
        (begin-1
         (flatten-%%lambda meaning (compiler-env))
         (when (> compile-error-count 0)
           (format #t "~&\n; ~a errors detected while compiling ~s"
                   compile-error-count form)))))))


(define (show-expansions)
  (trace form-meaning apply-user-macro))