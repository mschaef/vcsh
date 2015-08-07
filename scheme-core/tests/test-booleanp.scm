(use-package! "unit-test")

(define-test boolean?
  (test-case (boolean? #f))
  (test-case (boolean? #t))
  (test-case (not (boolean? 1)))
  (test-case (not (boolean? 'symbol)))
  (test-case (not (boolean? #\a)))
  (test-case (not (boolean? [1 2 3 4 5]))))
