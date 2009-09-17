(use-package! "unit-test")

(define-test make-vector
  (test-case (runtime-error? (make-vector 'a '())))
  (test-case (runtime-error? (make-vector -1 '())))
  (test-case (not (runtime-error? (make-vector 0 '()))))
  (test-case (not (runtime-error? (make-vector 1 '()))))
  (test-case (not (runtime-error? (make-vector 10 '()))))
  (let ((a (make-vector 3 :test-sym)))
    (test-case (vector? a))
    (test-case (= 3 (length a)))
    (test-case (and (eq? (vector-ref a 0) :test-sym)
		 (eq? (vector-ref a 1) :test-sym)
		 (eq? (vector-ref a 2) :test-sym)))))
