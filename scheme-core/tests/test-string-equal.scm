(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-equal
  (check (equal? ts10a ts10a))
  (check (equal? ts20a ts20a))
  (check (equal? ts40a ts40a))
  (check (equal? ts10a ts10b))
  (check (equal? ts20a ts20b))
  (check (equal? ts40a ts40b)))
