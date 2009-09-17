(use-package! "unit-test")

(define-test numeric-equality
  (test-case (= 1))
  (test-case (= 1.0))

  (test-case (= 1 1))
  (test-case (not (= 1 2)))
  (test-case (= 1.0 1.0))
  (test-case (not (= 1.0 2.0)))
  (test-case (= 1 1.0))
  (test-case (not (= 1 2.0)))
  (test-case (= 1.0 1))
  (test-case (not (= 1.0 2)))

  (test-case (= 1 1 1))
  (test-case (not (= 2 1 1)))
  (test-case (not (= 1 2 1)))
  (test-case (not (= 1 1 2)))
  (test-case (= 1.0 1.0 1.0))
  (test-case (not (= 2.0 1.0 1.0)))
  (test-case (not (= 1.0 2.0 1.0)))
  (test-case (not (= 1.0 1.0 2.0)))
  (test-case (= 1.0 1 1.0))
  (test-case (not (= 2.0 1 1.0)))
  (test-case (not (= 1.0 2 1.0)))
  (test-case (not (= 1.0 1 2.0)))
  (test-case (= 1 1.0 1.0))
  (test-case (not (= 2 1.0 1.0)))
  (test-case (not (= 1 2.0 1.0)))
  (test-case (not (= 1 1.0 2.0)))

  (test-case (= 1 1 1 1))
  (test-case (not (= 2 1 1 1)))
  (test-case (not (= 1 2 1 1)))
  (test-case (not (= 1 1 2 1)))
  (test-case (not (= 1 1 2 2)))

  (test-case (eqv? 1 1))
  (test-case (eqv? 1.0 1.0))
  (test-case (not (eqv? 1 2)))
  (test-case (not (eqv? 1.0 2.0)))
  (test-case (not (eqv? 1.0 1)))
  (test-case (not (eqv? 1 1.0)))
  (test-case (not (eqv? 1.0 2)))
  (test-case (not (eqv? 1 2.0)))

  (test-case (equal? 1 1))
  (test-case (equal? 1.0 1.0))
  (test-case (not (equal? 1 2)))
  (test-case (not (equal? 2 1)))
  (test-case (not (equal? 1.0 2.0)))
  (test-case (not (equal? 2.0 1.0)))
  (test-case (not (equal? 1 1.0)))
  (test-case (not (equal? 1.0 1)))
  (test-case (not (equal? 2 1.0)))
  (test-case (not (equal? 1.0 2)))

  (test-case (= #iposinf #iposinf))
  (test-case (= #ineginf #ineginf))

  (test-case (not (= #inan #inan)))
  (test-case (not (= #iposinf #ineginf)))
  (test-case (not (= #ineginf #iposinf)))
  (test-case (not (= #iposinf #inan)))
  (test-case (not (= #ineginf #inan)))
  (test-case (not (= #inan #ineginf)))
  (test-case (not (= #inan #iposinf)))

  (test-case (equal? #iposinf #iposinf))
  (test-case (equal? #ineginf #ineginf))
  (test-case (equal? #inan #inan))
  (test-case (not (equal? #iposinf #ineginf)))
  (test-case (not (equal? #ineginf #iposinf)))

  (test-case (not (equal? #iposinf #inan)))
  (test-case (not (equal? #ineginf #inan)))
  (test-case (not (equal? #inan #ineginf)))
  (test-case (not (equal? #inan #iposinf)))
  )
