(use-package! "unit-test")

(define-test vector?
  (check (vector? [1 2 3]))
  (check (vector? (vector 1 2 3)))
  (check (vector? (vector-set! [1 2 3] 1 1)))
  (check (vector? (make-vector 10)))
  (check (not (vector? 1)))
  (check (not (vector? (lambda (x) (+ x 1)))))
  (check (not (vector? 'symbol)))
  (check (not (vector? #t)))
  (check (not (vector? #\t)))
  (check (not (vector? "String")))
  (check (not (vector? '(1 2 3)))))
