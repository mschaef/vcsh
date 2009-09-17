(use-package! "unit-test")

(define-test string-drop-right
  (test-case (runtime-error? (string-drop-right :symbol 0)))
  (test-case (runtime-error? (string-drop-right 1234 0)))
  (test-case (runtime-error? (string-drop-right "foo" :symbol)))
  (test-case (runtime-error? (string-drop-right "foo" "bar")))

  (test-case (equal? (string-drop-right "" 0) ""))
  (test-case (runtime-error? (string-drop-right "" 1)))
  (test-case (runtime-error? (string-drop-right "" 2)))
  
  (test-case (equal? (string-drop-right "12345" 0) "12345"))
  (test-case (equal? (string-drop-right "12345" 1) "1234"))
  (test-case (equal? (string-drop-right "12345" 2) "123"))
  (test-case (equal? (string-drop-right "12345" 3) "12"))
  (test-case (equal? (string-drop-right "12345" 4) "1"))
  (test-case (equal? (string-drop-right "12345" 5) ""))
  (test-case (runtime-error? (string-drop-right "12345" 6)))
  )
