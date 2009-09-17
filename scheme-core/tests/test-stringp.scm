(use-package! "unit-test")

(define ts10a "0123456789")

(define-test string?
  (test-case (equal? ts10a (string? ts10a)))
  (test-case (not (string? 12))))
