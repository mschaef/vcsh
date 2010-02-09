;;;; compiler-expand.scm
;;;;
;;;; The expander. This expands incoming lisp forms into a dialect of the
;;;; language that only uses core forms.

(define *show-expansions* #f)

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

(define expand-form) ; forward

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

(define (valid-lambda-list? l-list)
  (let valid? ((l-list l-list))
    (or (symbol? l-list)
        (null? l-list)
        (and (pair? l-list)
             (symbol? (car l-list))
             (valid? (cdr l-list))))))

(define (valid-variable-list? vars)
  (let valid? ((vars vars))
    (or (null? vars)
        (and (pair? vars)
             (symbol? (car vars))
             (valid? (cdr vars))))))

(define (expand/%lambda form genv at-toplevel?)
  (unless (or (list? (cadr form)) (null? (cadr form)))
    (compile-error form "Invalid %lambda, expected property list"))
  (unless (valid-lambda-list? (caddr form))
    (compile-error form "Invalid %lambda, bad lambda list"))
  `(scheme::%lambda ,(cadr form) ,(caddr form)
      ,@(translate-form-sequence (cdddr form) #t genv #f)))

(define (expand/%toplevel-lambda form genv at-toplevel?)
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

(define (expand/logical form genv at-toplevel?)
  `(,(car form) ,@(map #L(expand-form _ genv at-toplevel?) (cdr form))))

(define (form-expander form genv at-toplevel?)
  (cond ((null? form)
         ())
        ((list? form)
         (case (car form)
           ((quote)               form)
           ((or and)              (expand/logical     form genv at-toplevel?))
           ((if)                  (expand/if          form genv at-toplevel?))
           ((scheme::%lambda)     (expand/%lambda     form genv at-toplevel?))
           ((%toplevel-lambda)    (expand/%toplevel-lambda    form genv at-toplevel?))
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

