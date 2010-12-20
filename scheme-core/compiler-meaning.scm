;;;; compiler-meaning.scm
;;;;
;;;; The semantic analysis phase. This maps basic lisp into an
;;;; s-expression tree of fast-op assembler instructinos.

(define *show-meanings* #f)

(define (l-list-vars l-list)
  (let retry ((l-list l-list))
    (cond ((null? l-list)
           ())
          ((atom? l-list)
           `(,l-list))
          (#t
           (cons (car l-list) (retry (cdr l-list)))))))

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

(define expanded-form-meaning) ; forward decl

(define (warn-if-global-unbound var genv)
  (unless (symbol-bound? var () genv)
    (compile-warning var "Global variable unbound: ~s" var)))

(define (meaning/application form cenv genv at-toplevel?)
  `(:apply ,(expanded-form-meaning (car form) cenv genv at-toplevel?)
           ,@(map #L(expanded-form-meaning _ cenv genv at-toplevel?) (cdr form))))

;; REVISIT: meaning/begin, /or, and /and all have the same basic form, poss. refactor.
;; REVISIT: meaning/begin, /or, and /and are all non-tail recursive

(define (meaning/symbol form cenv genv at-toplevel?)
  (cond ((keyword? form)
         `(:literal ,form))
        ((bound-in-cenv? form cenv)
         `(:local-ref ,form))
        (#t
         (warn-if-global-unbound form genv)
         `(:global-ref ,form))))

(define *special-form-handlers* #h(:eq))

(defmacro (define-special-form pattern . code)
  (check pair? pattern)
  (check symbol? (car pattern))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (hash-push! *special-form-handlers* ',(car pattern)
                 (cons (lambda (form)
                         (and (pair? form)
                              (dbind-matches? ,(cdr pattern) (cdr form))))
                       (lambda (form cenv genv at-toplevel?)
                         (dbind ,(cdr pattern) (cdr form)
                           ,@code))))))


(define (meaning/%macro defn cenv genv at-toplevel?)
  `(:macro ,(expanded-form-meaning (second defn) () genv at-toplevel?)))

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
    (let ((body-form (expanded-form-meaning (code-body-form body)
                                            (extend-cenv l-list cenv)
                                            genv
                                            at-toplevel?)))
      `(:close-env ,l-list ,body-form ,p-list))))


(define (meaning/begin form cenv genv at-toplevel?)
  (let recur ((args (cdr form)))
    (cond ((null? args)     `(:literal ()))
          ((length=1? args) (expanded-form-meaning (car args) cenv genv at-toplevel?))
          (#t `(:sequence ,(expanded-form-meaning (car args) cenv genv at-toplevel?)
                          ,(recur (cdr args)))))))

(define (meaning/or form cenv genv at-toplevel?)
  (let recur ((args (cdr form)))
    (cond ((null? args)     `(:literal #f))
          ((length=1? args) (expanded-form-meaning (car args) cenv genv at-toplevel?))
          (#t `(:or/2 ,(expanded-form-meaning (car args) cenv genv at-toplevel?)
                      ,(recur (cdr args)))))))


(define (meaning/and form cenv genv at-toplevel?)
  (let recur ((args (cdr form)))
    (cond ((null? args)     `(:literal #t))
          ((length=1? args) (expanded-form-meaning (car args) cenv genv at-toplevel?))
          (#t `(:and/2 ,(expanded-form-meaning (car args) cenv genv at-toplevel?)
                       ,(recur (cdr args)))))))

(define (meaning/if form cenv genv at-toplevel?)
  `(:if-true ,(expanded-form-meaning (second form) cenv genv at-toplevel?)
             ,(expanded-form-meaning (third form) cenv genv at-toplevel?)
             ,(expanded-form-meaning (fourth form) cenv genv at-toplevel?)))

(define (meaning/set! form cenv genv at-toplevel?)
  (dbind (fn-pos var val-form) form
    (cond ((keyword? var)
           (compile-error form "Cannot rebind a keyword: ~s" var))
          ((bound-in-cenv? var cenv)
           `(:local-set! ,var ,(expanded-form-meaning val-form cenv genv at-toplevel?)))
          (#t
           (warn-if-global-unbound var genv)
           `(:global-set! ,var ,(expanded-form-meaning val-form cenv genv at-toplevel?))))))

(define-special-form (scheme::%define name defn)
  `(:global-def ,name
                ,((toplevel-form->thunk defn genv))
                ,genv))

(define-special-form (quote value)
  `(:literal ,value))

(define-special-form (the-environment)
  `(:get-env))

(define (meaning/%mark-stack form cenv genv at-toplevel?)
  `(:mark-stack
    ,(expanded-form-meaning (second form) cenv genv at-toplevel?)
    ,(expanded-form-meaning (third form) cenv genv at-toplevel?)))

(define (meaning/%set-genv form cenv genv at-toplevel?)
  `(:set-genv
    ,(expanded-form-meaning (second form) cenv genv at-toplevel?)))

(define (meaning/%%catch form cenv genv at-toplevel?)
  `(:catch
    ,(expanded-form-meaning (second form) cenv genv at-toplevel?)
    ,(expanded-form-meaning (third form) cenv genv at-toplevel?)))

(define (meaning/%%throw form cenv genv at-toplevel?)
  `(:throw
    ,(expanded-form-meaning (second form) cenv genv at-toplevel?)
    ,(expanded-form-meaning (third form) cenv genv at-toplevel?)))

(define (meaning/%%with-unwind-fn form cenv genv at-toplevel?)
  `(:with-unwind-fn
    ,(expanded-form-meaning (second form) cenv genv at-toplevel?)
    ,(expanded-form-meaning (third form) cenv genv at-toplevel?)))

(define (expanded-form-meaning form cenv genv at-toplevel?)
  (call-with-compiler-tracing *show-meanings* '("MEANING-OF" "IS")
    (lambda (form)
      (cond ((symbol? form)
             (meaning/symbol form cenv genv at-toplevel?))
            ((scheme::fast-op? form)
             form)
            ((atom? form)
             `(:literal ,form))
            ((hash-has? *special-form-handlers* (car form))
             (aif (find #L((car _) form) (hash-ref *special-form-handlers* (car form)))
                  ((cdr it) form cenv genv at-toplevel?)
                  (error "Invalid syntax for ~a: ~s" (car form) form)))
            (#t
             (case (car form)
               ((scheme::%macro)            (meaning/%macro           form cenv genv at-toplevel?))
               ((scheme::%lambda)           (meaning/%lambda          form cenv genv at-toplevel?))
               ((begin)                     (meaning/begin            form cenv genv at-toplevel?))
               ((or)                        (meaning/or               form cenv genv at-toplevel?))
               ((and)                       (meaning/and              form cenv genv at-toplevel?))
               ((if)                        (meaning/if               form cenv genv at-toplevel?))
               ((set!)                      (meaning/set!             form cenv genv at-toplevel?))
               ((scheme::%set-genv)         (meaning/%set-genv        form cenv genv at-toplevel?))
               ((scheme::%mark-stack)       (meaning/%mark-stack      form cenv genv at-toplevel?))
               ((scheme::%%throw)           (meaning/%%throw          form cenv genv at-toplevel?))
               ((scheme::%%catch)           (meaning/%%catch          form cenv genv at-toplevel?))
               ((scheme::%%with-unwind-fn)  (meaning/%%with-unwind-fn form cenv genv at-toplevel?))
               (#t                          (meaning/application      form cenv genv at-toplevel?))))))
    form))

