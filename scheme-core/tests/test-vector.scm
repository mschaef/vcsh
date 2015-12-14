(use-package! "unit-test")

(define-test vector
  (let ((a (vector)))
    (check (vector? a))
    (check (= 0 (length a))))
  (let ((a (vector 1 2 3)))
    (check (vector? a))
    (check (= 3 (length a)))
    (check (= (vector-ref a 0) 1))
    (check (= (vector-ref a 1) 2))
    (check (= (vector-ref a 2) 3))))

(define-test vector-literal
  (let ((value 42))
    (check (equal? [42 42] [value value])))

  (let ((create-vector-fn (lambda () [])))
    (check (not (eq? (create-vector-fn) (create-vector-fn))))))
