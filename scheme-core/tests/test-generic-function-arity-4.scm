(use-package! "unit-test")

(define-generic-function (tgf-4 w x y z)
  :default-handling)

(define-method (tgf-4 (w fixnum) (x fixnum) (y fixnum) (z fixnum))
  :fixnum)

(define-method (tgf-4 (w number) (x number) (y number) (z number))
  :number)

(define-method (tgf-4 (w flonum) (x flonum) (y flonum) (z flonum))
  :flonum)

(define-test generic-function-arity-4
  (test-case (eq? (tgf-4 #\1 2 3 4) :default-handling))
  (test-case (eq? (tgf-4 1 #\2 3 4) :default-handling))
  (test-case (eq? (tgf-4 1 2 #\3 4) :default-handling))
  (test-case (eq? (tgf-4 1 2 3 #\4) :default-handling))
  (test-case (eq? (tgf-4 #\1 #\2 #\3 #\4) :default-handling))

  (test-case (eq? (tgf-4 1 2 3 4) :fixnum))

  (test-case (eq? (tgf-4 1.0 2 3 4) :number))
  (test-case (eq? (tgf-4 1 2.0 3 4) :number))
  (test-case (eq? (tgf-4 1 2 3.0 4) :number))
  (test-case (eq? (tgf-4 1 2 3 4.0) :number))

  (test-case (eq? (tgf-4 1.0 2.0 3.0 4.0) :flonum))
  (test-case (eq? (tgf-4 1i 2i 3i 4i) :flonum)))


