(use-package! "unit-test")

(define-test vector-copy
  (test-case (runtime-error? (vector-copy 'not-a-vector)))

  (let* ((a (vector))
	 (b (vector-copy a)))
    (test-case (not (eq? a b)))
    (test-case (equal? a b))
    (test-case (= 0 (length b)))
    (test-case (equal? #() b)))

  (let* ((a (vector 1))
	 (b (vector-copy a)))
    (test-case (not (eq? a b)))
    (test-case (equal? a b))
    (test-case (= 1 (length b)))
    (test-case (equal? #(1) b)))

  (let* ((a (vector 1 2 3))
	 (b (vector-copy a)))
    (test-case (not (eq? a b)))
    (test-case (equal? a b))
    (test-case (= 3 (length b)))
    (test-case (equal? #(1 2 3) b))))
    