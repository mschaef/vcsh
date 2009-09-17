(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-equal
  (test-case (equal? ts10a ts10a))
  (test-case (equal? ts20a ts20a))
  (test-case (equal? ts40a ts40a))
  (test-case (equal? ts10a ts10b))
  (test-case (equal? ts20a ts20b))
  (test-case (equal? ts40a ts40b)))
