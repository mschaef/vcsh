(use-package! "unit-test")

(define-test pair-fold
  (check (equal? (pair-fold + '() '()) '()))
  (check (equal? (pair-fold cons '() '(1 2 3 4 5)) '((5) (4 5) (3 4 5) (2 3 4 5) (1 2 3 4 5))))
  (check (equal? (pair-fold + :foo '()) :foo)))
