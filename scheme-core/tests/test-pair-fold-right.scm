(use-package! "unit-test")

(define-test pair-fold-right
  (check (equal? (pair-fold-right + :foo '()) :foo))
  (check (equal? (pair-fold-right cons '() '(a b c)) '((a b c) (b c) (c)))))
