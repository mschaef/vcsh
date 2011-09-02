
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

(define (apply-expander expander form)
  (call-with-compiler-tracing (and *show-expansions* (pair? form))
      '("EXPAND" "INTO")
      (lambda (form) (expander form))
    form))

(define (compiler-macroexpand-1 form)
  (aif (and (pair? form)
            (symbol? (car form))
            (symbol-bound? (car form))
            (macro? (symbol-value (car form))))
       (values #t
               (apply-expander (lambda (form)
                                 (let ((transformer (scheme::%macro-transformer it)))
                                   (transformer form ())))
                               form))
       (values #f form)))

(define compiler-macroexpand)

(define (compiler-macroexpand form)
  (mvbind (expanded? expanded-form) (compiler-macroexpand-1 form)
    (if expanded?
        (compiler-macroexpand expanded-form)
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

(define (maybe-expand-user-macro form)
  (catch 'end-compiler-macroexpand
    (handler-bind
        ((runtime-error
          (if *debug*
              handle-runtime-error
              (lambda (message args . rest)
                (compile-error form (format #f "Macro signaled error: ~I" message args) args)
                (throw 'end-compiler-macroexpand (values #f ()))))))
      (compiler-macroexpand-1 form))))

(define expand-form) ; forward

(define (make-translated-form-sequence expanded-body-forms local-definitions)
  (define (define->let-binding def)
    (dbind (ignored sym defn) def
      `(,sym ,defn)))

  (if (null? local-definitions)
      expanded-body-forms
      (list (compiler-macroexpand ; Reuse the letrec macro for local defines.
             `(letrec ,(map define->let-binding local-definitions)
                ,@expanded-body-forms)))))

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

(define (expand-lambda-sequence forms)
  (mvbind (definitions forms) (extract-form-sequence-definitions
                               (flatten-form-sequence forms))
    (make-translated-form-sequence forms definitions)))

(define (flatten-form-sequence forms)
  "Translates a sequence of forms into another sequence of forms by removing
   expanding all macros and removing any nested begins."
  ;; Note that this would be an expansion step, were it not for the fact
  ;; that this takes a list of forms and produces a list of forms. (Instead
  ;; of form to form.)
  (define (begin-block? form)
    (and (pair? form)
         (eq? (car form) 'begin)))

  (let loop ((remaining forms) (flattened ()))
    (let ((current-form (compiler-macroexpand (car remaining))))
      (cond
       ((begin-block? current-form)
        (loop (append (cdr current-form) (cdr remaining)) flattened))
       ((null? remaining)
        flattened)
       (#t
        (loop (cdr remaining) (append flattened (cons (expand-form current-form)))))))))

(define (expand/if form)
  (unless (or (length=3? form) (length=4? form))
    (compile-error form "Invalid if, bad length."))
  (map expand-form form))

(define (expand/begin form)
  `(begin ,@(flatten-form-sequence (cdr form))))

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

(define (expand/%lambda form)
  (unless (or (list? (cadr form)) (null? (cadr form)))
    (compile-error form "Invalid %lambda, expected property list"))
  (unless (valid-lambda-list? (caddr form))
    (compile-error form "Invalid %lambda, bad lambda list"))
  `(scheme::%lambda ,(cadr form) ,(caddr form)
      ,@(expand-lambda-sequence (cdddr form) #f)))

(define (expand/%toplevel-lambda form)
  `(scheme::%lambda () () ,@(flatten-form-sequence (cdr form) #t)))

(define (expand/set! form)
  (unless (length=3? form)
    (compile-error "Invalid set!, bad length." form))
  `(set! ,(cadr form) ,(expand-form (caddr form))))

(define (expand/logical form)
  `(,(car form) ,@(map expand-form (cdr form))))

(define (form-expander form)
  (cond ((null? form)
         ())
        ((list? form)
         (case (car form)
           ((quote)               form)
           ((or and)              (expand/logical          form))
           ((if)                  (expand/if               form))
           ((scheme::%lambda)     (expand/%lambda          form))
           ((%toplevel-lambda)    (expand/%toplevel-lambda form))
           ((set!)                (expand/set!             form))
           ((begin)               (expand/begin            form))
           (#t
            (mvbind (expanded? expanded-form) (maybe-expand-user-macro form)
              (cond (expanded?
                     (expand-form expanded-form))
                    ((atom? expanded-form)
                     (expand-form expanded-form))
                    (#t
                     (map expand-form form)))))))
        ((symbol? form) form)
        ((atom? form)   form)
        (#t             (error "Don't know how to expand this form: ~s" form))))

(define (expand-form form)
  (apply-expander form-expander form))

(define (cpass/expand form)
  (expand-form form))