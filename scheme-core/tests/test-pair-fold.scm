(use-package! "unit-test")

(define-test pair-fold
  (test-case (equal? (pair-fold + '() '()) '()))
  (test-case (equal? (pair-fold cons '() '(1 2 3 4 5)) '((5) (4 5) (3 4 5) (2 3 4 5) (1 2 3 4 5))))
  (test-case (equal? (pair-fold + :foo '()) :foo)))
