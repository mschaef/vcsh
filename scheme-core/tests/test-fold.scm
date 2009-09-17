(use-package! "unit-test")

(define-test fold
  (test-case (runtime-error? (fold + :foo '(1 2 3))))
  (test-case (runtime-error? (fold + 0 '(:foo 1 2))))
  (test-case (runtime-error? (fold + 0 '(1 2 :foo))))

  (test-case (equal? (fold + '() '()) '()))
  (test-case (equal? (fold + 0 '(1 2 3)) 6))
  (test-case (equal? (fold cons '() '(1 2 3 4 5)) '(5 4 3 2 1)))
  (test-case (equal? (fold + :foo '()) :foo))
  (test-case (equal? (fold list '() '(1 2 3 4 5)) '(5 (4 (3 (2 (1 ()))))))))
