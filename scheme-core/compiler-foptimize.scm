
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

(define *optimize/integrate-subrs* #t)

(define (fop-id x) x)

(forward map-fop-assembly)

(define (map-fop-assembly fn fasm)

  (define (map-fop-args fop-formals fop-actuals)
    (define (map-fop-arg formal actual)
      (cond ((eq? formal :fast-op)
             (map-fop-assembly fn actual))
            ((eq? formal :fast-ops)
             (map #L(map-fop-arg :fast-op _) actual))
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

(define (optimize-pass/global-applications fasm)
  (map-fop-assembly xform-global-apply fasm))

;;;; Primitive function integration

(define *integrations* (make-hash))

(define (register-integration-expander! fn-sym arity fn)
  (hash-set! *integrations* `(,fn-sym ,arity) fn))

(defmacro (define-integration app-form fop-form)
  (dbind (fn-sym . args) app-form
    (runtime-check valid-lambda-list? args)
    (mvbind (arity rest?) (lambda-list-arity args)
      (runtime-check not rest?)
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

(define-integration (car x)
  `(:sequence
    ,x
    (:car)))

(define-integration (cdr x)
  `(:sequence
    ,x
    (:cdr)))

(define-integration (not x)
  `(:sequence
    ,x
    (:not)))

(define-integration (null? x)
  `(:sequence
    ,x
    (:nullp)))

(define (optimize-pass/integrate-subrs fasm)
  (map-fop-assembly xform-integrate fasm))

;;;; The toplevel optimizer

(define (opt-pass enabled? pass-fn)
  (if enabled? pass-fn identity))

(define (optimize-pass/full fop)
  ((rcompose optimize-pass/global-applications
             (opt-pass *optimize/integrate-subrs* optimize-pass/integrate-subrs))
   fop))

(define (optimize-fop-assembly fasm)
  ((opt-pass *optimize* optimize-pass/full) fasm))

(define (cpass/fasm-optimize fasm)
  (optimize-fop-assembly fasm))
