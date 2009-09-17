(use-package! "unit-test")

(define-test string-take
  (test-case (runtime-error? (string-take :symbol 0)))
  (test-case (runtime-error? (string-take 1234 0)))
  (test-case (runtime-error? (string-take "foo" :symbol)))
  (test-case (runtime-error? (string-take "foo" "bar")))

  (test-case (equal? (string-take "" 0) ""))
  (test-case (runtime-error? (string-take "" 1)))
  (test-case (runtime-error? (string-take "" 2)))
  
  (test-case (equal? (string-take "12345" 0) ""))
  (test-case (equal? (string-take "12345" 1) "1"))
  (test-case (equal? (string-take "12345" 2) "12"))
  (test-case (equal? (string-take "12345" 3) "123"))
  (test-case (equal? (string-take "12345" 4) "1234"))
  (test-case (equal? (string-take "12345" 5) "12345"))
  (test-case (runtime-error? (string-take "12345" 6)))
  )

