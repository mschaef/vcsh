(use-package! "unit-test")

(define-test list->vector
  (check (runtime-error? (list->vector 'foo)))
  (check (runtime-error? (list->vector '(1 2 . improper-list))))
  (check (not (runtime-error? (list->vector '()))))
  (check (not (runtime-error? (list->vector '(1)))))
  (check (not (runtime-error? (list->vector '(1 2 3)))))

  (let ((a (list->vector '())))
    (check (vector? a))
    (check (= 0 (length a))))

  (let ((a (list->vector '(1))))
    (check (vector? a))
    (check (= 1 (length a)))
    (check (equal? [1] a)))

  (let ((a (list->vector '(1 2 3))))
    (check (vector? a))
    (check (= 3 (length a)))
    (check (equal? [1 2 3] a))))
