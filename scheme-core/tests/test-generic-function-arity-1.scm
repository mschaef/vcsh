(use-package! "unit-test")


(define-generic-function (tgf-1 x)
  :default-handling)

(define-method (tgf-1 (x fixnum)) :fixnum)
(define-method (tgf-1 (x flonum)) :flonum)
(define-method (tgf-1 (x complex)) :complex)
(define-method (tgf-1 (x character)) :character)
(define-method (tgf-1 (x string)) :streng) ; overwritten
(define-method (tgf-1 (x string)) :string)
(define-method (tgf-1 (x number)) :number)


(define-test generic-function-arity-1
  (test-case (eq? (tgf-1 3) :fixnum))
  (test-case (eq? (tgf-1 3.0) :flonum))
  (test-case (eq? (tgf-1 3i) :complex))
  (test-case (eq? (tgf-1 #\3) :character))
  (test-case (eq? (tgf-1 "3") :string))
  (test-case (eq? (tgf-1 #(3)) :default-handling))
  (test-case (eq? (tgf-1 '(3)) :default-handling))
  )


