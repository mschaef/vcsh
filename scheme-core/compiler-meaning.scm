
;;;; compiler-meaning.scm --
;;;;
;;;; The semantic analysis phase. This maps basic lisp into an
;;;; s-expression tree of fast-op assembler instructinos.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define *show-meanings* #f)

(define (lambda-list-variables lambda-list)
  "Given a lambda list, return a list of descriptive tuples of each
variable bound by that lambda list. Each tuple contains the bound
symbol, the type of binding (:var or :rest), and the index of the binding
within the environment."
  (let loop ((binding-index 0)
              (lambda-list lambda-list))
    (cond
     ((null? lambda-list)
      ())
     ((atom? lambda-list)
      `((,lambda-list :rest ,binding-index)))
     (#t
      (cons `(,(car lambda-list) :var ,binding-index)
            (loop (+ binding-index 1) (cdr lambda-list)))))))

(define (extend-cenv lambda-list cenv)
  (cons (lambda-list-variables lambda-list) cenv))

(define (bound-in-cenv-frame? var cenv-frame cenv)
  (unless (list? cenv-frame)
    (error "Malformed frame ~s in cenv: ~s" cenv-frame cenv))
  (assoc var cenv-frame))

(define (bound-in-cenv? var cenv)
  "Determine whether or not a variable is bound in the given cenv. Returns a
description of the binding coordinates: (frame-index var-name binding-type binding-index)"
  (let loop ((frame-index 0)
             (remaining-frames cenv))
    (cond
     ((null? remaining-frames)
      #f)
     ((atom? remaining-frames)
      (error "Malformed cenv: ~s" cenv))
     (#t
      (aif (bound-in-cenv-frame? var (car remaining-frames) cenv)
           (cons frame-index it)
           (loop (+ frame-index 1)
                 (cdr remaining-frames)))))))

(forward expanded-form-meaning)

(define (symbol-binding-type-of sym)
  (if (symbol-bound? sym)
      (type-of (symbol-value sym))
      '#f))

(define (bound-global var)
  (unless (symbol-bound? var)
    (compile-warning var "Global variable unbound: ~s" var))
  var)

(define (meaning/application form cenv)
  `(:apply ,(expanded-form-meaning (car form) cenv)
           ,(map #L(expanded-form-meaning _ cenv) (cdr form))))

(define (meaning/symbol form cenv)
  (cond ((keyword? form)
         `(:literal ,form))
        ((bound-in-cenv? form cenv)
         `(:local-ref ,form))
        (#t
         `(:global-ref ,(bound-global form)))))

(define *special-form-handlers* (make-identity-hash))

(define (special-form-symbols)
  ;; REVISIT: Currently unioning in toplevel special forms and
  ;; type names... need to do something better
  (hash-keys *special-form-handlers*))

(defmacro (define-special-form pattern . code)
  (runtime-check pair? pattern)
  (runtime-check symbol? (car pattern))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (hash-push! *special-form-handlers* ',(car pattern)
                 (cons (lambda (form)
                         (and (pair? form)
                              (dbind-matches? ,(cdr pattern) (cdr form))))
                       (lambda (form cenv)
                         (dbind ,(cdr pattern) (cdr form)
                           ,@code))))))

(define-special-form (scheme::%lambda p-list l-list . body)
  (define (code-body-form body-forms)
    "Return a single form that is semantically equivalent to <body-forms>."
    (case (length body-forms)
      ((0) '(values))
      ((1) (first body-forms))
      (#t `(begin ,@body-forms))))
  `(:closure (,l-list . ,p-list)
               ,(expanded-form-meaning (code-body-form body)
                                       (extend-cenv l-list cenv))))


(define-special-form (begin . args)
  (let recur ((args args))
    (cond ((null? args)
           `(:literal ()))
          ((length=1? args)
           (expanded-form-meaning (car args) cenv))
          (#t
           `(:sequence
             ,(expanded-form-meaning (car args) cenv)
             ,(recur (cdr args)))))))

(define-special-form (scheme::block . args)
  (cond ((null? args)
         `(:literal ()))
        (#t
         `(:block
           ,@(map #L(expanded-form-meaning _ cenv) args)))))

(define-special-form (or . args)
  (let recur ((args args))
    (cond ((null? args)
           `(:literal #f))
          ((length=1? args)
           (expanded-form-meaning (car args) cenv))
          (#t
           `(:sequence
             ,(expanded-form-meaning (car args) cenv)
             (:if-true
              (:retval)
              ,(recur (cdr args))))))))

(define-special-form (and . args)
  (let recur ((args args))
    (cond ((null? args)
           `(:literal #t))
          ((length=1? args)
           (expanded-form-meaning (car args) cenv))
          (#t
           `(:sequence
             ,(expanded-form-meaning (car args) cenv)
             (:if-true
              ,(recur (cdr args))
              (:literal #f)))))))

(define-special-form (if cond-form then-form)
  `(:sequence
    ,(expanded-form-meaning cond-form cenv)
    (:if-true
     ,(expanded-form-meaning then-form cenv)
     (:literal ()))))

(define-special-form (if cond-form then-form else-form)
  `(:sequence
    ,(expanded-form-meaning cond-form cenv)
    (:if-true
     ,(expanded-form-meaning then-form cenv)
     ,(expanded-form-meaning else-form cenv))))

(define-special-form (set! var val-form)
  (when (keyword? var)
    (compile-error form "Cannot rebind a keyword: ~s" var))
  (aif (bound-in-cenv? var cenv)
       (begin
         (when (eq? :rest (third it))
           (compile-error form "Cannot rebind a rest binding: ~s" var))
         `(:sequence
           ,(expanded-form-meaning val-form cenv)
           (:local-set! ,var)))
       `(:sequence
         ,(expanded-form-meaning val-form cenv)
         (:global-set! ,(bound-global var)))))

(define compile)

(define-special-form (scheme::%define name defn)
  `(:global-def ,name
                ,((compile defn))))

(define-special-form (quote value)
  `(:literal ,value))

(define-special-form (the-environment)
  `(:get-env))

(define-special-form (scheme::%preserve-initial-frame global-var body-form)
  `(:global-preserve-frame
    ,(bound-global global-var)
    ,(expanded-form-meaning body-form cenv)))

(define-special-form (scheme::%with-stack-boundary tag-form body-form)
  `(:stack-boundary
    ,(expanded-form-meaning tag-form cenv)
    ,(expanded-form-meaning body-form cenv)))

(define-special-form (scheme::%%catch tag-form body-form)
  `(:catch
    ,(expanded-form-meaning tag-form cenv)
    ,(expanded-form-meaning body-form cenv)))

(define-special-form (scheme::%%throw tag-form value-form)
  `(:throw
    ,(expanded-form-meaning tag-form cenv)
    ,(expanded-form-meaning value-form cenv)))

(define-special-form (scheme::%%with-unwind-fn after-fn-form body-form)
  `(:with-unwind-fn
    ,(expanded-form-meaning after-fn-form cenv)
    ,(expanded-form-meaning body-form cenv)))

(define-special-form (scheme::%%get-fsp)
  `(:get-fsp))

(define-special-form (scheme::%%get-frame)
  `(:get-frame))

(define-special-form (scheme::%%get-hframes)
  `(:get-hframes))

(define-special-form (scheme::%%set-hframes new-hframes)
  `(:set-hframes 
    ,(expanded-form-meaning new-hframes cenv)))

(define-special-form (scheme::%%fast-enqueue-cell cell-form queue-form)
  `(:fast-enqueue-cell
    ,(expanded-form-meaning cell-form cenv)
    ,(expanded-form-meaning queue-form cenv)))

(define-special-form (scheme::%%while-true cond-form body-form)
  `(:while-true
    ,(expanded-form-meaning cond-form cenv)
    ,(expanded-form-meaning body-form cenv)))

(define (expanded-form-meaning form cenv)
  (call-with-compiler-tracing *show-meanings* '("MEANING-OF" "IS")
    (lambda (form)
      (cond ((symbol? form)
             (meaning/symbol form cenv))
            ((fast-op? form)
             form)
            ((atom? form)
             `(:literal ,form))
            ((hash-has? *special-form-handlers* (car form))
             (aif (find #L((car _) form) (hash-ref *special-form-handlers* (car form)))
                  ((cdr it) form cenv)
                  (error "Invalid syntax for ~a: ~s" (car form) form)))
            (#t
             (meaning/application form cenv))))
    form))

(define (cpass/meaning form)
  (expanded-form-meaning form ()))
