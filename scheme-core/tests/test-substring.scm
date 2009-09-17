(use-package! "unit-test")


(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test substring
  (test-case (runtime-error? (substring 123 2 3)))
  (test-case (runtime-error? (substring ts10a 'non-number 3)))
  (test-case (runtime-error? (substring ts10a 3 'non-number)))
  (test-case (runtime-error? (substring ts10a -1 5)))
  (test-case (runtime-error? (substring ts10a  6 5)))
  (test-case (runtime-error? (substring ts10a  6 -1)))
  (test-case (runtime-error? (substring ts10a  6 11)))

  (test-case (equal? ts10a (substring ts10a 0 10)))
  (test-case (equal? "" (substring ts10a 0 0)))
  (test-case (equal? "01" (substring ts10a 0 2)))
  (test-case (equal? "" (substring ts10a 2 2)))
  (test-case (equal? "234" (substring ts10a 2 5)))

  (test-case (equal? ts10a (substring ts40a 0 10)))
  (test-case (equal? "" (substring ts40a 0 0)))
  (test-case (equal? "01" (substring ts40a 0 2)))
  (test-case (equal? "" (substring ts40a 2 2)))
  (test-case (equal? "234" (substring ts40a 2 5)) )

  ;; TESTTHIS: Test substring with null end offset  
  )
