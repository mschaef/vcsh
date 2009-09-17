(use-package! "unit-test")

(define-test string-drop
  (test-case (runtime-error? (string-drop :symbol 0)))
  (test-case (runtime-error? (string-drop 1234 0)))
  (test-case (runtime-error? (string-drop "foo" :symbol)))
  (test-case (runtime-error? (string-drop "foo" "bar")))

  (test-case (equal? (string-drop "" 0) ""))
  (test-case (runtime-error? (string-drop "" 1)))
  (test-case (runtime-error? (string-drop "" 2)))
  
  (test-case (equal? (string-drop "12345" 0) "12345"))
  (test-case (equal? (string-drop "12345" 1) "2345"))
  (test-case (equal? (string-drop "12345" 2) "345"))
  (test-case (equal? (string-drop "12345" 3) "45"))
  (test-case (equal? (string-drop "12345" 4) "5"))
  (test-case (equal? (string-drop "12345" 5) ""))
  (test-case (runtime-error? (string-drop "12345" 6)))
  )

