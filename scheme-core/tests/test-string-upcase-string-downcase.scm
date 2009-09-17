(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-upcase-string-downcase
  (test-case (runtime-error? (string-upcase 123)))
  (test-case (runtime-error? (string-downcase 123)))
   
  (test-case (equal? ts10a (string-upcase ts10a))     )
  (test-case (equal? ts10a (string-downcase ts10a)))
   
  (test-case (equal? "UPPERCASE" (string-upcase "UPPERCASE")))
  (test-case (equal? "UPPERCASE" (string-upcase "UpPerCasE")))
  (test-case (equal? "test" (string-downcase "test")))
  (test-case (equal? "test" (string-downcase "TEsT")))
  
  (let ((ts-a "abcde")
	(ts-b "ABCDE"))
    (string-upcase ts-a)
    (string-upcase ts-b)
    
    (test-case (equal? ts-a "abcde"))
    (test-case (equal? ts-b "ABCDE"))))
