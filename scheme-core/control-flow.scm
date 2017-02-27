
;;;; control-flow.scm --
;;;;
;;;; Some control flow facilities
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(defmacro (forward sym)
  `(define ,sym))

(define (throw tag :optional value)
  (%%throw tag value))

(define (unwind-protect thunk after)
  (runtime-check procedure? thunk)
  (runtime-check procedure? after)
  (%%with-unwind-fn after (thunk)))

(defmacro (catch tag-form . body)
  `(%%catch ,tag-form (begin ,@body)))

(defmacro (catch-all . forms)
  `(catch () ,@forms))

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
  (dbind (var list-form result-form) head
    (unless (symbol? var)
      (error "dolist requires a symbol for a variable binding" var))
    `(begin
       (for-each-1 (lambda (,var) ,@body) ,list-form)
       ,result-form)))

(define (vector-for-each fn . xss)
  (case (length xss)
    ((0) #f)
    ((1)
     (let ((xs (car xss)))
       (runtime-check vector? xs)
       (let loop ((ii 0))
         (when (< ii (length xs))
           (fn (vector-ref xs ii))
           (loop (+ ii 1))))))
    (#t
     (unless (every? vector? xss)
       (error "Bad argument ~s, vector required." xss))
     (let ((common-length (apply min (map length xss))))
       (let loop ((ii 0))
         (when (< ii common-length)
           (apply fn (map #L(vector-ref _ ii) xss))
           (loop (+ ii 1))))))))

(defmacro (dovec head . body)
  (unless (list? head)
    (error "dovec requires a list for <head>" head))
  (dbind (var list-form result-form) head
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

(define (parse-cond-clause clause)
  (runtime-check list? clause "Invalid cond clause.")
  (values (car clause) (cdr clause)))

(define (clause-body-statement code)
  (if (length=1? code) (car code) `(begin ,@code)))

(defmacro (cond . clauses)
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

(define (check-case-clause clause)
  (runtime-check list? clause "Invalid case clause.")
  (runtime-check (or (eq? #t) list?) (first clause) "Invalid case guard."))

(define (case-clause-guard-predicate-form val-sym clause-guard)
  (cond ((eq? clause-guard #t)
         #t)
        ((length=1? clause-guard)
         `(eq? ,val-sym ',(car clause-guard)))
        (#t
         `(or ,@(map (lambda (guard-val)`(eq? ,val-sym ',guard-val)) clause-guard)))))

(defmacro (case value-form . clauses)
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
  ;; (REVISIT: the return value is ignored, but it could be used as an input to optimizations.)
  (find-clause-guard-values clauses)

  ;; Verify that any default clause is the last clause
  (let ((default-tail (find-tail #L(eq? (car _) #t) clauses)))
    (unless (or (null? default-tail) (null? (cdr default-tail)))
      (error "The default clause of a case must be the last clause.")))

  ;; Emit a let/cond form that provides the case semantics
  (with-gensyms (val-sym)
    `(let ((,val-sym ,value-form))
       (cond ,@(map (lambda (clause)
                      `(,(case-clause-guard-predicate-form val-sym (car clause)) ,@(cdr clause)))
                    clauses)))))

;; TODO: Add ecase

(defmacro (while cond-form . body)
  `(%%while-true ,cond-form (begin ,@body)))

(defmacro (until test . body)
  `(while (not ,test) ,@body))

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


;;; This is the interpreter eval-when. It only allows :execute forms to execute. The
;;; compiler handles this differently.
(defmacro (eval-when situations . forms)
  (unless (and (or (null? situations) (list? situations))
               (every? #L(member _ '(:compile-toplevel :load-toplevel :execute)) situations))
    (error "Bad situations list: ~a" situations))
  (cond ((not (member :execute situations)) ())
        ((length=1? forms)                  (car forms)) ; No begin block for short lists of forms
        (#t                                 `(begin ,@forms))))




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

(define (always x)
  "Returns a function  that always returns <x>."
  (lambda () x))

(define (negate pred?)
  "Returns a one argument predicate that has the opposite sense
   as <pred?>"
  (lambda (x) (not (pred? x))))

(forward compose)

(define (compose . fns)
  "Returns an arity-1 function that evaluates the composition of the
   functions in <fns>."
  (if (null? fns)
      identity
      (lambda (x)
        ((car fns) ((apply compose (cdr fns)) x)))))

(forward rcompose)

(define (rcompose . fns)
  "Returns an arity-1 function that evaluates the reverse composition of
   the functions in <fns>."
  (if (null? fns)
      identity
      (lambda (x)
        ((apply rcompose (cdr fns)) ((car fns) x)))))

(define (identity x)
  "The identity function. Returns <x>, unchanged."
  x)
