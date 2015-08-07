(use-package! "unit-test")

(define-test list->vector
  (test-case (runtime-error? (list->vector 'foo)))
  (test-case (runtime-error? (list->vector '(1 2 . improper-list))))
  (test-case (not (runtime-error? (list->vector '()))))
  (test-case (not (runtime-error? (list->vector '(1)))))
  (test-case (not (runtime-error? (list->vector '(1 2 3)))))

  (let ((a (list->vector '())))
    (test-case (vector? a))
    (test-case (= 0 (length a))))

  (let ((a (list->vector '(1))))
    (test-case (vector? a))
    (test-case (= 1 (length a)))
    (test-case (equal? [1] a)))

  (let ((a (list->vector '(1 2 3))))
    (test-case (vector? a))
    (test-case (= 3 (length a)))
    (test-case (equal? [1 2 3] a))))
