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
  (check (eq? (tgf-1 3) :fixnum))
  (check (eq? (tgf-1 3.0) :flonum))
  (check (eq? (tgf-1 3i) :complex))
  (check (eq? (tgf-1 #\3) :character))
  (check (eq? (tgf-1 "3") :string))
  (check (eq? (tgf-1 [3]) :default-handling))
  (check (eq? (tgf-1 '(3)) :default-handling)))


