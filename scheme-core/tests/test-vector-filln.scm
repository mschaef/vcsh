(use-package! "unit-test")

(define-test vector-fill!
  (check (runtime-error? (vector-fill! 'not-a-vector 12)))
  (check (not (runtime-error? (vector-fill! [] #f))))
  (check (not (runtime-error? (vector-fill! [0] #f))))
  (check (not (runtime-error? (vector-fill! [0 1 2 3] #f))))

  (let* ((a (vector))
	 (b (vector-fill! a :test)))
    (check (vector? b))
    (check (= (length b) 0))
    (check (eq? a b))
    (check (equal? [] b)))

  (let* ((a (vector 1))
	 (b (vector-fill! a :test)))
    (check (vector? b))
    (check (= (length b) 1))
    (check (eq? a b))
    (check (equal? [:test] b)))

  (let* ((a (vector 1 2 3))
	 (b (vector-fill! a :test)))
    (check (vector? b))
    (check (= (length b) 3))
    (check (eq? a b))
    (check (equal? [:test :test :test] b))))

	 
    
