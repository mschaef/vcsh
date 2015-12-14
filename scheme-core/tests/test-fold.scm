(use-package! "unit-test")

(define-test fold
  (check (runtime-error? (fold + :foo '(1 2 3))))
  (check (runtime-error? (fold + 0 '(:foo 1 2))))
  (check (runtime-error? (fold + 0 '(1 2 :foo))))

  (check (equal? (fold + '() '()) '()))
  (check (equal? (fold + 0 '(1 2 3)) 6))
  (check (equal? (fold cons '() '(1 2 3 4 5)) '(5 4 3 2 1)))
  (check (equal? (fold + :foo '()) :foo))
  (check (equal? (fold list '() '(1 2 3 4 5)) '(5 (4 (3 (2 (1 ()))))))))
