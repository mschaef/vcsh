(use-package! "unit-test")

(define-test vector-fill!
  (test-case (runtime-error? (vector-fill! 'not-a-vector 12)))
  (test-case (not (runtime-error? (vector-fill! [] #f))))
  (test-case (not (runtime-error? (vector-fill! [0] #f))))
  (test-case (not (runtime-error? (vector-fill! [0 1 2 3] #f))))

  (let* ((a (vector))
	 (b (vector-fill! a :test)))
    (test-case (vector? b))
    (test-case (= (length b) 0))
    (test-case (eq? a b))
    (test-case (equal? [] b)))

  (let* ((a (vector 1))
	 (b (vector-fill! a :test)))
    (test-case (vector? b))
    (test-case (= (length b) 1))
    (test-case (eq? a b))
    (test-case (equal? [:test] b)))

  (let* ((a (vector 1 2 3))
	 (b (vector-fill! a :test)))
    (test-case (vector? b))
    (test-case (= (length b) 3))
    (test-case (eq? a b))
    (test-case (equal? [:test :test :test] b))))

	 
    
