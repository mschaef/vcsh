(use-package! "unit-test")

(define-test vector->list
  (check (runtime-error? (vector->list 'not-a-vector)))
  (check (not (runtime-error? (vector->list []))))
  (check (not (runtime-error? (vector->list [0]))))
  (check (not (runtime-error? (vector->list [0 1 2]))))
  (let ((a (vector->list [])))
    (check (null? a)))

  (let ((a (vector->list [0])))
    (check (list? a))
    (check (= 1 (length a)))
    (check (equal? '(0) a)))

  (let ((a (vector->list [0 1 2])))
    (check (list? a))
    (check (= 3 (length a)))
    (check (equal? '(0 1 2) a))))
