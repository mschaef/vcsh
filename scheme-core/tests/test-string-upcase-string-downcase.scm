(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-upcase-string-downcase
  (check (runtime-error? (string-upcase 123)))
  (check (runtime-error? (string-downcase 123)))
   
  (check (equal? ts10a (string-upcase ts10a)))
  (check (equal? ts10a (string-downcase ts10a)))
   
  (check (equal? "UPPERCASE" (string-upcase "UPPERCASE")))
  (check (equal? "UPPERCASE" (string-upcase "UpPerCasE")))
  (check (equal? "test" (string-downcase "test")))
  (check (equal? "test" (string-downcase "TEsT")))
  
  (let ((ts-a "abcde")
	(ts-b "ABCDE"))
    (string-upcase ts-a)
    (string-upcase ts-b)
    
    (check (equal? ts-a "abcde"))
    (check (equal? ts-b "ABCDE"))))
