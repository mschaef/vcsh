(use-package! "unit-test")

(define-test string-take
  (check (runtime-error? (string-take :symbol 0)))
  (check (runtime-error? (string-take 1234 0)))
  (check (runtime-error? (string-take "foo" :symbol)))
  (check (runtime-error? (string-take "foo" "bar")))

  (check (equal? (string-take "" 0) ""))
  (check (runtime-error? (string-take "" 1)))
  (check (runtime-error? (string-take "" 2)))
  
  (check (equal? (string-take "12345" 0) ""))
  (check (equal? (string-take "12345" 1) "1"))
  (check (equal? (string-take "12345" 2) "12"))
  (check (equal? (string-take "12345" 3) "123"))
  (check (equal? (string-take "12345" 4) "1234"))
  (check (equal? (string-take "12345" 5) "12345"))
  (check (runtime-error? (string-take "12345" 6))))
