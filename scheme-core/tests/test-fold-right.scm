(use-package! "unit-test")

(define-test fold-right
  (test-case (equal? (fold-right + '() '()) '()))
  (test-case (equal? (fold-right + 0 '(1 2 3)) 6))
  (test-case (equal? (fold-right cons '() '(1 2 3 4 5)) '(1 2 3 4 5)))
  (test-case (equal? (fold-right + :foo '()) :foo))
  (test-case (equal? (fold-right list '() '(1 2 3 4)) '(1 (2 (3 (4 ())))))))
