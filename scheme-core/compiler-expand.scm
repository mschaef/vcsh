
;;;; compiler-expand.scm --
;;;;
;;;; The expander. This expands incoming lisp forms into a dialect of the
;;;; language that only uses core forms understood by the semantic
;;;; analysis phase.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define *show-expansions* #f)

(define (apply-expander expander form at-toplevel?)
  (call-with-compiler-tracing (and *show-expansions* (pair? form))
      (if at-toplevel?
          '("EXPAND-TOPLEVEL" "INTO-TOPLEVEL")
          '("EXPAND" "INTO"))
      (lambda (form) (expander form at-toplevel?))
    form))

(define (compiler-macroexpand-1 form at-toplevel?)
  (aif (and (pair? form)
            (symbol? (car form))
            (symbol-bound? (car form))
            (macro? (symbol-value (car form))))
       (values #t
               (apply-expander (lambda (form at-toplevel?)
                                 (let ((transformer (scheme::%macro-transformer it)))
                                   (transformer form ())))
                               form
                               at-toplevel?))
       (values #f form)))

(define compiler-macroexpand)

(define (compiler-macroexpand form at-toplevel?)
  (mvbind (expanded? expanded-form) (compiler-macroexpand-1 form at-toplevel?)
    (if expanded?
        (compiler-macroexpand expanded-form at-toplevel?)
        form)))

(define (scheme::macroexpand-1 form)
  "Apply the macro expansion pass of the compiler to <form>, but only for one
   time. Two values are returned: the first is a boolean indicating if a
   a macro was expanded and the second is the result of that expansion."
  (compiler-macroexpand-1 form #f))

(define (scheme::macroexpand form)
  "Apply the macro expansion pass of the compiler to <form>, and repeat until
   no more macros apply. The return value is the result of this full expansion."
  (compiler-macroexpand form #f #f))

(define (maybe-expand-user-macro form at-toplevel?)
  (catch 'end-compiler-macroexpand
    (handler-bind
        ((runtime-error
          (if *debug*
              handle-runtime-error
              (lambda (message args . rest)
                (compile-error form (format #f "Macro signaled error: ~I" message args) args)
                (throw 'end-compiler-macroexpand (values #f ()))))))
      (compiler-macroexpand-1 form at-toplevel?))))

(define expand-form) ; forward

(define (make-translated-form-sequence body-forms ldefs at-toplevel?)
  (if (null? ldefs)
      (map #L(expand-form _ at-toplevel?) body-forms)
      (expand-form
       `((letrec ,ldefs
           ,@body-forms))
       at-toplevel?)))

(define (translate-form-sequence forms allow-definitions? at-toplevel?)
  "Translates a sequence of forms into another sequence of forms by removing
   any nested begins or defines."
  ;; Note that this would be an expansion step, were it not for the fact
  ;; that this takes a list of forms and produces a list of forms. (Instead
  ;; of form to form.)
  (define (begin-block? form)        (and (pair? form) (eq? (car form) 'begin)))
  (define (define? form)             (and (pair? form) (eq? (car form) 'scheme::%define)))
  (define (define-binding-pair form) (cons (cadr form) (cddr form)))

  (let loop ((forms forms) (ldefs ()) (body-forms ()))

    (let ((current-form (compiler-macroexpand (car forms) at-toplevel?)))
      (cond
       ((begin-block? current-form)
        (loop (append (cdr current-form) (cdr forms)) ldefs body-forms))

       ((define? current-form)
        (unless allow-definitions?
         (compile-error current-form "Definitions not allowed here."))
        (cond (at-toplevel?
               (loop (cdr forms) ldefs (append body-forms (cons current-form))))
              ((null? body-forms)
               (loop (cdr forms) (cons (define-binding-pair current-form) ldefs) body-forms))
              (#t
               (compile-error current-form "Local defines must be the first forms in a block."))))

       ((null? forms)
        (make-translated-form-sequence body-forms ldefs at-toplevel?))

       (#t
        (loop (cdr forms) ldefs (append body-forms (cons current-form))))))))


(define (primitive-definition-form? form)
  "Return <form> if it is a primitive definition form, #f otherwise."
  (if (and (pair? form)
           (eq? (car form) 'scheme::%define))
      form
      #f))

(define (disallow-definitions forms)
  "Scan the list of <forms> for primitive defintions. Signal a compile
   error if any are found."
  (awhen (any? primitive-definition-form? forms)
    (compile-error it "Definition not allowed"))
  forms)

(define (extract-form-sequence-definitions forms)
  "Split up <forms> into a list of definitions and a list of executable
   forms. If any definitions follow executable forms, a compile error is signaled."
  (let loop ((remaining forms) (definitions ()) (output ()))
    (if (null? remaining)
        (values (reverse definitions)
                (reverse output))
        (let ((next-form (car remaining)))
          (if (primitive-definition-form? next-form)
              (begin
                (unless (null? output)
                  (compile-error (car forms) "Definitions must be the first form in a block."))
                (loop (cdr remaining) (cons next-form definitions) output))
              (loop (cdr remaining) definitions (cons next-form output)))))))

(define (flatten-form-sequence forms at-toplevel?)
  "Translates a sequence of forms into another sequence of forms by removing
   expanding all macros and removing any nested begins."
  ;; Note that this would be an expansion step, were it not for the fact
  ;; that this takes a list of forms and produces a list of forms. (Instead
  ;; of form to form.)
  (define (begin-block? form)
    (and (pair? form)
         (eq? (car form) 'begin)))

  (let loop ((remaining forms) (flattened ()))
    (let ((current-form (compiler-macroexpand (car remaining) at-toplevel?)))
      (cond
       ((begin-block? current-form)
        (loop (append (cdr current-form) (cdr remaining)) flattened))
       ((null? remaining)
        flattened)
       (#t
        (loop (cdr remaining) (append flattened (cons (expand-form current-form at-toplevel?)))))))))

(define (expand/if form at-toplevel?)
  (unless (or (length=3? form) (length=4? form))
    (compile-error form "Invalid if, bad length."))
  (map #L(expand-form _ at-toplevel?) form))

(define (expand/begin form at-toplevel?)
  `(begin ,@(flatten-form-sequence (cdr form) at-toplevel?)))

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

(define (expand/%lambda form at-toplevel?)
  (unless (or (list? (cadr form)) (null? (cadr form)))
    (compile-error form "Invalid %lambda, expected property list"))
  (unless (valid-lambda-list? (caddr form))
    (compile-error form "Invalid %lambda, bad lambda list"))
  `(scheme::%lambda ,(cadr form) ,(caddr form)
      ,@(translate-form-sequence (cdddr form) #t #f)))

(define (expand/%toplevel-lambda form at-toplevel?)
  `(scheme::%lambda () () ,@(translate-form-sequence (cdr form) #t #t)))

(define (expand/set! form at-toplevel?)
  (unless (length=3? form)
    (compile-error "Invalid set!, bad length." form))
  `(set! ,(cadr form) ,(expand-form (caddr form) at-toplevel?)))

(define (expand/logical form at-toplevel?)
  `(,(car form) ,@(map #L(expand-form _ at-toplevel?) (cdr form))))

(define (form-expander form at-toplevel?)
  (cond ((null? form)
         ())
        ((list? form)
         (case (car form)
           ((quote)               form)
           ((or and)              (expand/logical          form at-toplevel?))
           ((if)                  (expand/if               form at-toplevel?))
           ((scheme::%lambda)     (expand/%lambda          form at-toplevel?))
           ((%toplevel-lambda)    (expand/%toplevel-lambda form at-toplevel?))
           ((set!)                (expand/set!             form at-toplevel?))
           ((begin)               (expand/begin            form at-toplevel?))
           (#t
            (mvbind (expanded? expanded-form) (maybe-expand-user-macro form at-toplevel?)
              (cond (expanded?
                     (expand-form expanded-form at-toplevel?))
                    ((atom? expanded-form)
                     (expand-form expanded-form at-toplevel?))
                    (#t
                     (map #L(expand-form _ at-toplevel?) form)))))))
        ((symbol? form) form)
        ((atom? form)   form)
        (#t             (error "Don't know how to expand this form: ~s" form))))

(define (expand-form form at-toplevel?)
  (apply-expander form-expander form at-toplevel?))

(define (cpass/expand form)
  (expand-form form #t))