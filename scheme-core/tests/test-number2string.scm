(use-package! "unit-test")

(define-test number->string
  (test-case (runtime-error? (number->string -1.2 8)))

  (test-case (equal? "0" (number->string 0 10)))
  (test-case (equal? "12" (number->string 12 10)))
  
  (test-case (equal? "-12" (number->string -12 10 #t)))
  (test-case (equal? "18446744073709551604" (number->string -12 10 #f)))
  (test-case (equal? "18446744073709551604" (number->string -12 10)))
  
  (test-case (equal? "0" (number->string 0 16)))
  (test-case (equal? "12" (number->string 18 16)))

  (test-case (equal? "-12" (number->string -18 16 #t)))
  (test-case (equal?  "ffffffffffffffee" (number->string -18 16 #f)))
  (test-case (equal?  "ffffffffffffffee" (number->string -18 16)))
    
  (test-case (equal? "cafe" (number->string 51966 16)))

  (test-case (equal? "-cafe" (number->string -51966 16 #t)))
  (test-case (equal? "ffffffffffff3502" (number->string -51966 16 #f)))
  (test-case (equal? "ffffffffffff3502" (number->string -51966 16)))

  (test-case (equal? "0" (number->string 0 8)))
  (test-case (equal? "12" (number->string 10 8)))

  (test-case (equal? "-12" (number->string -10 8 #t)))
  (test-case (equal? "1777777777777777777766" (number->string -10 8 #f)))
  (test-case (equal? "1777777777777777777766" (number->string -10 8)))

  (test-case (runtime-error? (number->string 12 2)))
  (test-case (runtime-error? (number->string 12 2.2)))
  (test-case (runtime-error? (number->string 12 #\a)))
  (test-case (runtime-error? (number->string 12 #f)))
  )
