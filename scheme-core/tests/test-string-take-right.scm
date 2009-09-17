(use-package! "unit-test")

(define-test string-take-right
  (test-case (runtime-error? (string-take-right :symbol 0)))
  (test-case (runtime-error? (string-take-right 1234 0)))
  (test-case (runtime-error? (string-take-right "foo" :symbol)))
  (test-case (runtime-error? (string-take-right "foo" "bar")))

  (test-case (equal? (string-take-right "" 0) ""))
  (test-case (runtime-error? (string-take-right "" 1)))
  (test-case (runtime-error? (string-take-right "" 2)))
  
  (test-case (equal? (string-take-right "12345" 0) ""))
  (test-case (equal? (string-take-right "12345" 1) "5"))
  (test-case (equal? (string-take-right "12345" 2) "45"))
  (test-case (equal? (string-take-right "12345" 3) "345"))
  (test-case (equal? (string-take-right "12345" 4) "2345"))
  (test-case (equal? (string-take-right "12345" 5) "12345"))
  (test-case (runtime-error? (string-take-right "12345" 6)))
  )
