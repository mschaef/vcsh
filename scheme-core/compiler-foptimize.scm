;;;; compiler-foptimize.scm
;;;;
;;;; The compiler's FOP tree optimizer

(define *optimize* #f)

(define (fop-id x) x)

(define (map-fop-assembly fn fasm)
  (define (map-fop-args fop-formals fop-actuals)
    (map (lambda (formal actual)
           (cond ((eq? formal :fast-op)
                  (map-fop-assembly fn actual))
                 ((pair? formal)
                  (map-fop-args formal actual))
                 (#t
                  actual)))
         fop-formals
         fop-actuals))
  (let ((fasm (fn fasm)))
    (dbind (fop-name . fop-actuals) fasm
      (watch-environment)
        (let ((fop-formals (fop-name->formals fop-name)))
          (if fop-formals
              `(,fop-name ,@(map-fop-args fop-formals fop-actuals))
              (error "Invalid FOP transformation result: ~s" fasm))))))

(define (optimize-fop-assembly fasm)
  fasm)
