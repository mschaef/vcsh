(use-package! "unit-test")

(define-generic-function (tgf-rv x)
  1)

(define-method (tgf-rv (x number))
  (+ 3 (call-next-method)))

(define-method (tgf-rv (x fixnum))
  (+ 5 (call-next-method)))

(define-method (tgf-rv (x flonum))
  (+ 7 (call-next-method)))

(define-method (tgf-rv (x complex))
  (+ 13 (call-next-method)))

(define-test generic-function-return-value
  (test-case (= (tgf-rv #\f) 1))
  (test-case (= (tgf-rv 1) 9))
  (test-case (= (tgf-rv 1.0) 11))
  (test-case (= (tgf-rv 1i) 24)))

