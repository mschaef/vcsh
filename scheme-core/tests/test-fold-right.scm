(use-package! "unit-test")

(define-test fold-right
  (check (equal? (fold-right + '() '()) '()))
  (check (equal? (fold-right + 0 '(1 2 3)) 6))
  (check (equal? (fold-right cons '() '(1 2 3 4 5)) '(1 2 3 4 5)))
  (check (equal? (fold-right + :foo '()) :foo))
  (check (equal? (fold-right list '() '(1 2 3 4)) '(1 (2 (3 (4 ())))))))
