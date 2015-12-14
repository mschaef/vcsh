(use-package! "unit-test")

(define-test vector-copy
  (check (runtime-error? (vector-copy 'not-a-vector)))

  (let* ((a (vector))
	 (b (vector-copy a)))
    (check (not (eq? a b)))
    (check (equal? a b))
    (check (= 0 (length b)))
    (check (equal? [] b)))

  (let* ((a (vector 1))
	 (b (vector-copy a)))
    (check (not (eq? a b)))
    (check (equal? a b))
    (check (= 1 (length b)))
    (check (equal? [1] b)))

  (let* ((a (vector 1 2 3))
	 (b (vector-copy a)))
    (check (not (eq? a b)))
    (check (equal? a b))
    (check (= 3 (length b)))
    (check (equal? [1 2 3] b))))
    
