(use-package! "unit-test")


(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")


;; TESTTHIS: Test substring with null end offset

(define-test substring
  (check (runtime-error? (substring 123 2 3)))
  (check (runtime-error? (substring ts10a 'non-number 3)))
  (check (runtime-error? (substring ts10a 3 'non-number)))
  (check (runtime-error? (substring ts10a -1 5)))
  (check (runtime-error? (substring ts10a  6 5)))
  (check (runtime-error? (substring ts10a  6 -1)))
  (check (runtime-error? (substring ts10a  6 11)))

  (check (equal? ts10a (substring ts10a 0 10)))
  (check (equal? "" (substring ts10a 0 0)))
  (check (equal? "01" (substring ts10a 0 2)))
  (check (equal? "" (substring ts10a 2 2)))
  (check (equal? "234" (substring ts10a 2 5)))

  (check (equal? ts10a (substring ts40a 0 10)))
  (check (equal? "" (substring ts40a 0 0)))
  (check (equal? "01" (substring ts40a 0 2)))
  (check (equal? "" (substring ts40a 2 2)))
  (check (equal? "234" (substring ts40a 2 5))))
