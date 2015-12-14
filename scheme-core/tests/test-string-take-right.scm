(use-package! "unit-test")

(define-test string-take-right
  (check (runtime-error? (string-take-right :symbol 0)))
  (check (runtime-error? (string-take-right 1234 0)))
  (check (runtime-error? (string-take-right "foo" :symbol)))
  (check (runtime-error? (string-take-right "foo" "bar")))

  (check (equal? (string-take-right "" 0) ""))
  (check (runtime-error? (string-take-right "" 1)))
  (check (runtime-error? (string-take-right "" 2)))
  
  (check (equal? (string-take-right "12345" 0) ""))
  (check (equal? (string-take-right "12345" 1) "5"))
  (check (equal? (string-take-right "12345" 2) "45"))
  (check (equal? (string-take-right "12345" 3) "345"))
  (check (equal? (string-take-right "12345" 4) "2345"))
  (check (equal? (string-take-right "12345" 5) "12345"))
  (check (runtime-error? (string-take-right "12345" 6))))
