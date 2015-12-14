(use-package! "unit-test")

(define-test number->string
  (check (equal? "0" (number->string 0 10)))
  (check (equal? "12" (number->string 12 10)))
  
  (check (equal? "-12" (number->string -12 10 #t)))
  (check (equal? "18446744073709551604" (number->string -12 10 #f)))
  (check (equal? "18446744073709551604" (number->string -12 10)))
  
  (check (equal? "0" (number->string 0 16)))
  (check (equal? "12" (number->string 18 16)))

  (check (equal? "-12" (number->string -18 16 #t)))
  (check (equal?  "ffffffffffffffee" (number->string -18 16 #f)))
  (check (equal?  "ffffffffffffffee" (number->string -18 16)))
    
  (check (equal? "cafe" (number->string 51966 16)))

  (check (equal? "-cafe" (number->string -51966 16 #t)))
  (check (equal? "ffffffffffff3502" (number->string -51966 16 #f)))
  (check (equal? "ffffffffffff3502" (number->string -51966 16)))

  (check (equal? "0" (number->string 0 8)))
  (check (equal? "12" (number->string 10 8)))

  (check (equal? "-12" (number->string -10 8 #t)))
  (check (equal? "1777777777777777777766" (number->string -10 8 #f)))
  (check (equal? "1777777777777777777766" (number->string -10 8)))

  (check (runtime-error? (number->string 12 2)))
  (check (runtime-error? (number->string 12 2.2)))
  (check (runtime-error? (number->string 12 #\a)))
  (check (runtime-error? (number->string 12 #f))))
