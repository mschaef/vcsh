
;;;; control-flow.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;;
;;;; Some control flow facilities

(define (throw tag :optional value)
  (%%throw tag value))

(define (unwind-protect thunk after)
  (check procedure? thunk)
  (check procedure? after)
  (%%unwind-protect thunk after))

(define (%catch-apply0 tag fn)
  (check procedure? fn)
  (%%catch-apply0 tag fn))

(defmacro (catch tag-form . body)
  `(%catch-apply0 ,tag-form (lambda () ,@body)))

(defmacro (block . forms)
  `(catch '%return-target ,@forms))

(defmacro (return value)
  `(throw '%return-target ,value))

(defmacro (catch-all . forms)
  `(catch #t ,@forms))

(defmacro (when condition . forms)
  `(if ,condition (begin ,@forms)))

(defmacro (unless condition . forms)
  `(when (not ,condition) ,@forms))

(defmacro (assert condition)
  `(unless ,condition (error "Assertation Failure: ~a" ',condition)))

(defmacro (begin-1 . code)
  (case (length code)
    ((0) ())
    ((1) `(begin ,@code))
    (#t  (with-gensyms (return-value-sym)
           `(let ((,return-value-sym ,(car code)))
              ,@(cdr code)
              ,return-value-sym)))))

(defmacro (begin-2 . code)
  (case (length code)
    ((0) ())
    ((1) `(begin ,@code ()))
    ((2) `(begin ,@code))
    (#t  (with-gensyms (return-value-sym)
           `(begin
              ,(car code)
              `(let ((,return-value-sym ,(cadr code)))
                 ,@(cddr code)
                 ,return-value-sym))))))


(defmacro (do init-exprs test-exprs . body)
  (unless (and (list? init-exprs)
               (every? (lambda (init-expr)
                         (or (length=2? init-expr)
                             (length=3? init-expr)))
                       init-exprs))
    (error "do init-expr's must be of the form ((<var> <init> [<step>]) ...): ~s"
           init-exprs))
  (unless (list? test-exprs)
    (error "do test-expr's must be of the form (<expr> ...): ~s"
           test-exprs))
  (let ((loop-name (gensym "do-loop")))
    `(let ,loop-name ,(map (lambda (init-expr)
                             `(,(first init-expr) ,(second init-expr)))
                           init-exprs)
          (if ,(first test-exprs)
              (begin ,@(rest test-exprs))
              (begin
                ,@body
                (,loop-name ,@(map (lambda (init-expr)
                                     (if (length=3? init-expr)
                                         (third init-expr)
                                         (first init-expr)))
                                   init-exprs)))))))

(defmacro (dotimes head . body)
  (unless (list? head)
    (error "dotimes requires a list of the form (<var> <list-form>) for its <head>" head))
  (let ((var (car head))
        (count-form (cadr head))
        (result-form (caddr head)))
    (unless (symbol? var)
      (error "dotimes requires a symbol for a variable binding" var))
    `(let ((,var 0))
       (repeat ,count-form ,@body (incr! ,var))
       ,result-form)))


(defmacro (dolist head . body)
  (unless (list? head)
    (error "dolist requires a list for <head>" head))
  (let ((var (car head))
        (list-form (cadr head))
        (result-form (caddr head)))
    (unless (symbol? var)
      (error "dolist requires a symbol for a variable binding" var))
    `(begin
       (for-each (lambda (,var) ,@body) ,list-form)
       ,result-form)))

(defmacro (dovec head . body)
  (unless (list? head)
    (error "dovec requires a list for <head>" head))
  (let ((var (car head))
        (list-form (cadr head))
        (result-form (caddr head)))
    (unless (symbol? var)
      (error "dovec requires a symbol for a variable binding" var))
    `(begin
       (vector-for-each (lambda (,var) ,@body) ,list-form)
       ,result-form)))

(define (provably-always-true? form)
  "Determine if <form> can be proved to be always true when evaluated."
  (or (and (atom? form)
           (not (eq? form #f))
           (not (symbol? form)))
      (and (list? form)
           (eq? (car form) 'quote)
           (not (eq? (cadr form) #f)))))

(define (provably-always-false? form)
  "Determine if <form> can be proved to be always false when evaluated. This
   is only the case for #f."
  (eq? form #f))

(defmacro (cond . clauses)

  (define (parse-cond-clause clause)
    (check list? clause "Invalid cond clause.")
    (values (car clause) (cdr clause)))

  (define (clause-body-statement code)
    (if (length=1? code) (car code) `(begin ,@code)))

  (if (null? clauses)
      '(values)
      (mvbind (guard body) (parse-cond-clause (car clauses))
        (cond ((provably-always-true? guard)
               ;; In this case, clauses after an 'always' clause will
               ;; always get ignored. Does this warrant a warning?
               (clause-body-statement body))
              ((provably-always-false? guard)
               ;; In this case, this clause is never evaluated
               `(cond ,@(cdr clauses)))
              (#t
               `(if ,guard
                    ,(clause-body-statement body)
                    (cond ,@(cdr clauses))))))))



(defmacro (case value-form . clauses)

  (define (check-case-clause clause)
    (check list? clause "Invalid case clause.")
    (check (or (eq? #t) list?) (first clause) "Invalid case guard."))

  (define (clause-guard-predicate-form val-sym clause-guard)
    (cond ((eq? clause-guard #t)
           #t)
          ((length=1? clause-guard)
           `(eq? ,val-sym ',(car clause-guard)))
          (#t
           `(or ,@(map (lambda (guard-val)`(eq? ,val-sym ',guard-val)) clause-guard)))))

  (define (find-clause-guard-values clauses)
    (fold (lambda (guard-value all-guard-values)
            (when (memq guard-value all-guard-values)
              (error "Duplicate case guard value: ~s" guard-value))
            (cons guard-value all-guard-values))
          ()
          (append-map (lambda (clause)
                        (check-case-clause clauses)
                        (if (pair? (car clause))
                            (car clause)
                            ()))
                      clauses)))
  ;; Check validity of all case clauses
  ;; (TODO: the return value is ignored, but it could be used as an input to optimizations.)
  (find-clause-guard-values clauses)

  ;; Verify that any default clause is the last clause
  (let ((default-tail (find-tail #L(eq? (car _) #t) clauses)))
    (unless (or (null? default-tail) (null? (cdr default-tail)))
      (error "The default clause of a case must be the last clause.")))

  ;; Emit a let/cond form that provides the case semantics
  (with-gensyms (val-sym)
    `(let ((,val-sym ,value-form))
       (cond ,@(map (lambda (clause)
                      `(,(clause-guard-predicate-form val-sym (car clause)) ,@(cdr clause)))
                    clauses)))))

(defmacro (while cond-form . body)
  (with-gensyms (while-loop-sym)
    `(begin
       (let ,while-loop-sym ()
            (when ,cond-form
              ,@body
              (,while-loop-sym)))
       (values))))

(defmacro (repeat limit-form . body)
  (with-gensyms (ii-sym limit-value-sym)
    `(let ((,ii-sym 0)
           (,limit-value-sym ,limit-form))
       (while (< ,ii-sym ,limit-value-sym)
         ,@body
         (incr! ,ii-sym)))))

;;; Anaphoric macros

(defmacro (awhen condition . code)
  `(let ((it ,condition))
     (when it
       ,@ code)))

(defmacro (aif condition then-clause else-clause)
  `(let ((it ,condition))
     (if it
         ,then-clause
         ,else-clause)))

(defmacro (awhile test . body)
  `(let ((it #t))
     (while it
            (set! it ,test)
            (when it ,@body))))

(defmacro (until test . body)
  `(while (not ,test) ,@body))

(defmacro (aand . conditions)
  (cond ((null? conditions)
         #t)
        ((null? (cdr conditions))
         (car conditions))
        (#t
         `(let ((it ,(car conditions)))
            (if it (aand ,@(cdr conditions)) #f)))))

(defmacro (acond . conditions)
  (cond ((null? conditions)
         ())
        ((list? (car conditions))
         `(aif ,(caar conditions)
               (begin ,@(cdar conditions))
               (acond ,@(cdr conditions))))))

;;; Random-case

(defmacro (random-case . cases)
  "Randomly evaluates one of its cases, with the specified probabilities.
   Each case takes the form ( <probability> . <code> ) and is evaluated
   with the probability <probability> / (sum <all probabilities>). "

  ;;  Validate the code being expanded
  (let loop ((remaining cases))
    (unless (null? remaining)
      (let ((current-case (car remaining)))
        (unless (list? current-case)
          (error "random-case cases must be lists" current-case))
        (unless (number? (car current-case))
          (error "random-case case probabilities must be numbers" (car current-case)))
        (unless (>= (car current-case) 0)
          (error "random-case case probabilities must be non-negative" (car current-case)))
        (loop (cdr remaining)))))

  ;; Expand it
  (let ((total-probabilities (apply + (map car cases)))
        (cumulative-probability-so-far 0))
    (with-gensyms (random-value-sym)
      `(let ((,random-value-sym (random ,total-probabilities)))
         (cond
          ,@(map (lambda (current-case)
                   (incr! cumulative-probability-so-far (car current-case))
                   `((< ,random-value-sym ,cumulative-probability-so-far) ,@(cdr current-case)))
                 cases))))))

;; !! with-output-to-string doubles line feeds

;;; This is the interpreter eval-when. It only allows :execute forms to execute. The
;;; compiler handles this differently.
(defmacro (eval-when situations . forms)
  (unless (and (or (null? situations) (list? situations))
               (every? #L(member _ '(:compile-toplevel :load-toplevel :execute)) situations))
    (error "Bad situations list: ~a" situations))
  (cond ((not (member :execute situations)) ())
        ((length=1? forms)                  (car forms)) ; No begin block for short lists of forms
        (#t                                 `(begin ,@forms))))



;;; Global bindings maninpulation

(define (copy-global-environment :optional (bindings-name #f))
  "Returns a unique copy of the current global bindings."
  (let ((new-global-bindings (%copy-global-environment (%current-global-environment))))
    (unless bindings-name
      (set! bindings-name (gensym "global-bindings")))
;    (vector-set! new-global-bindings 0 bindings-name)
    new-global-bindings))

(define (apply-with-genv fn genv)
  (check procedure? fn)
  (check %global-environment? genv)
  (let ((old-genv (%current-global-environment)))
    (%set-genv genv)
    (%%unwind-protect fn
                     (lambda () (%set-genv old-genv)))))

(defmacro (with-global-environment bindings . code)
  "Executes <code> with the global bindings in <bindings>. The previous
   global bindings are restored after the code returns."
  `(apply-with-genv (lambda () ,@code)
                    ,bindings))

(define (capture-global-environment fn)
  "Returns a closure of <fn> over the current global environment."
  (let ((apply apply) ;; locally capture apply so that the below function will work, even if genv has a corrupt apply
        (genv (%current-global-environment)))
    (lambda args
      (apply-with-genv (lambda () (apply fn args))
                       genv))))

(defmacro (defalias alias procedure)
  "Defines an alias for <procedure> named <alias>. The alias
   delegates all calls to the procedure named by <procedure>,
   with no alterations."
  (with-gensyms (args)
   `(define (,alias . ,args)
      (apply ,procedure ,args))))

;;;; The ever-popular typecase and etypecase

(defmacro (typecase obj-expr . clauses)
  (dolist (clause clauses)
          (unless (list? clause)
                  (error "typecase clauses must be lists: ~s" clause))
          (unless (or (and (list? (car clause))
                           (every? symbol? (car clause)))
                      (eq? #t (car clause)))
                  (error "typecase clause guards must either lists of symbols or #t: ~s" (car clause))))
  `(case (type-of ,obj-expr) ,@clauses))


(defmacro (etypecase obj-expr . clauses)
  (dolist (clause clauses)
          (unless (list? clause)
                  (error "typecase clauses must be lists: ~s" clause))
          (when (eq? (car clause) #t)
                (error "Explicit else clauses not allowed in etypecase, consider typecase instead: ~s" clause))
          (unless (and (list? (car clause))
                       (every? symbol? (car clause)))
                  (error "etypecase clause guards must be lists of symbols: ~s" clause)))
  (with-gensyms (obj-type-sym)
                `(let ((,obj-type-sym (type-of ,obj-expr)))
                   (case ,obj-type-sym ,@clauses
                         (#t
                          (error "etypecase for ~s not handled, must be one of ~s"
                                 ,obj-type-sym
                                 ',(apply set-union/eq (map car clauses))))))))

;;; Fully evaluating and/or



(define (and* . args)
  "Computes the logical AND of <args>, WITHOUT short-circuit evaluation
   of <args>. All <args> are always evaluated. This function always
   returns either #t or #f."
  (let loop ((args args))
    (cond ((null? args) #t)
          ((car args) (loop (cdr args)))
          ;; We actually can avoid traversing all of <args>, because the
          ;; evaluation rule for functions has already guaranteed that
          ;; all of the forms have been evaluated.
          (#t #f))))

(define (or* . args)
  "Computes the logical OR of <args>, WITHOUT short-circuit evaluation
   of <args>. All <args> are always evaluated.  This function always
   returns either #t or #f."
  (let loop ((args args))
    (cond ((null? args) #f)
          ;; We actually can avoid traversing all of <args>, because the
          ;; evaluation rule for functions has already guaranteed that
          ;; all of the forms have been evaluated.
          ((car args) #t)
          (#t (loop (cdr args))))))

(defmacro (UNIMPLEMENTED proc-name . impl-notes)
  "Define a placeholder procedure for the unimplemented procedure <proc-name>.
   The placeholder procedure will just throw an error signaling that it is
   unimplemented. <impl-notes> is a placeholder for notes tied to the unimplemented
   function that might describe how it will eventually be implemented."
  `(define (,proc-name . args)
     (error "~s unimplemented." ',proc-name)))
