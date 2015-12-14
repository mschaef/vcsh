(use-package! "unit-test")

(define-test make-vector
  (check (runtime-error? (make-vector 'a '())))
  (check (runtime-error? (make-vector -1 '())))
  (check (not (runtime-error? (make-vector 0 '()))))
  (check (not (runtime-error? (make-vector 1 '()))))
  (check (not (runtime-error? (make-vector 10 '()))))
  (let ((a (make-vector 3 :test-sym)))
    (check (vector? a))
    (check (= 3 (length a)))
    (check (and (eq? (vector-ref a 0) :test-sym)
		 (eq? (vector-ref a 1) :test-sym)
		 (eq? (vector-ref a 2) :test-sym)))))
