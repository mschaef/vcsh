(use-package! "unit-test")

(define-test vector?
  (test-case (vector? [1 2 3]))
  (test-case (vector? (vector 1 2 3)))
  (test-case (vector? (vector-set! [1 2 3] 1 1)))
  (test-case (vector? (make-vector 10)))
  (test-case (not (vector? 1)))
  (test-case (not (vector? (lambda (x) (+ x 1)))))
  (test-case (not (vector? 'symbol)))
  (test-case (not (vector? #t)))
  (test-case (not (vector? #\t)))
  (test-case (not (vector? "String")))
  (test-case (not (vector? '(1 2 3)))))
