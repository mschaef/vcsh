(use-package! "unit-test")

(define-test vector
  (let ((a (vector)))
    (test-case (vector? a))
    (test-case (= 0 (length a))))
  (let ((a (vector 1 2 3)))
    (test-case (vector? a))
    (test-case (= 3 (length a)))
    (test-case (= (vector-ref a 0) 1))
    (test-case (= (vector-ref a 1) 2))
    (test-case (= (vector-ref a 2) 3))))
