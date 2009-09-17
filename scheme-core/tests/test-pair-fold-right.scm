(use-package! "unit-test")

(define-test pair-fold-right
  (test-case (equal? (pair-fold-right + :foo '()) :foo))
  (test-case (equal? (pair-fold-right cons '() '(a b c)) '((a b c) (b c) (c)))))
