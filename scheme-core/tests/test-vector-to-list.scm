(use-package! "unit-test")

(define-test vector->list
  (test-case (runtime-error? (vector->list 'not-a-vector)))
  (test-case (not (runtime-error? (vector->list []))))
  (test-case (not (runtime-error? (vector->list [0]))))
  (test-case (not (runtime-error? (vector->list [0 1 2]))))
  (let ((a (vector->list [])))
    (test-case (null? a)))

  (let ((a (vector->list [0])))
    (test-case (list? a))
    (test-case (= 1 (length a)))
    (test-case (equal? '(0) a)))

  (let ((a (vector->list [0 1 2])))
    (test-case (list? a))
    (test-case (= 3 (length a)))
    (test-case (equal? '(0 1 2) a))))
