
;;;; compiler-foptimize.scm --
;;;;
;;;; The FOP tree optimizer.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define *optimize* #t)

(define (fop-id x) x)

(define map-fop-assembly) ;; forward

(define (map-fop-assembly fn fasm)

  (define (map-fop-args fop-formals fop-actuals)
    (define (map-fop-arg formal actual)
      (cond ((eq? formal :fast-op)
             (map-fop-assembly fn actual))
            ((pair? formal)
             (map #L(map-fop-arg (car formal) _) actual))
            (#t
             actual)))
    (map map-fop-arg fop-formals fop-actuals))

  (let ((fasm (fn fasm)))
    (dbind (fop-name . fop-actuals) fasm
        (let ((fop-formals (fop-name->formals fop-name)))
          (if fop-formals
              `(,fop-name ,@(map-fop-args fop-formals fop-actuals))
              (error "Invalid FOP transformation result: ~s" fasm))))))

;;;; Global application optimization

(define (xform-global-apply fop)
  (bind-if-match (:apply (:global-ref ?sym) ?args) fop
     `(:apply-global ,?sym ,?args)
     fop))

;;;; Primitive function integration

(define *integrations* (make-hash))

(define (register-integration-expander! fn-sym arity fn)
  (hash-set! *integrations* `(,fn-sym ,arity) fn))

(defmacro (define-integration app-form fop-form)
  (dbind (fn-sym . args) app-form
    (check valid-lambda-list? args)
    (mvbind (arity rest?) (lambda-list-arity args)
      (check not rest?)
      `(eval-when (:load-toplevel :compile-toplevel :execute)
         (register-integration-expander! ,fn-sym ,arity
                                         (lambda ,args ,fop-form))))))

(define (find-integration-expander fn-sym arity)
  (hash-ref *integrations* `(,fn-sym ,arity) #f))

(define (find-integrated-form fn-sym args)
  (aif (and (symbol-bound? fn-sym)
            (find-integration-expander (symbol-value fn-sym)
                                       (length args)))
       (apply it args)
       #f))

(define (xform-integrate fop)
  (bind-if-match (:apply-global ?fn-sym ?args) fop
    (or (find-integrated-form ?fn-sym ?args)
        fop)
    fop))

(define-integration (car x) `(:car ,x))
(define-integration (cdr x) `(:cdr ,x))

;;;; The toplevel optimizer

(define (optimize-pass/integrate-subrs fasm)
  (map-fop-assembly xform-integrate fasm))

(define (optimize-pass/global-applications fasm)
  (map-fop-assembly xform-global-apply fasm))

(define (optimize-fop-assembly fasm)
  (if *optimize*
      (optimize-pass/integrate-subrs
       (optimize-pass/global-applications fasm))
      fasm))
