
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

(define (xform-global-apply fop)
  (bind-if-match (:apply (:global-ref ?sym) ?args) fop
     `(:apply-global ,?sym ,?args)
     fop))

(define (optimize-fop-assembly fasm)
  (if *optimize*
      (map-fop-assembly xform-global-apply fasm)
      fasm))
