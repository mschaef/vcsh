(use-package! "unit-test")

(define ts10a "0123456789")

(define-test string?
  (check (equal? ts10a (string? ts10a)))
  (check (not (string? 12))))
