(use-package! "unit-test")

(define-test string-drop
  (check (runtime-error? (string-drop :symbol 0)))
  (check (runtime-error? (string-drop 1234 0)))
  (check (runtime-error? (string-drop "foo" :symbol)))
  (check (runtime-error? (string-drop "foo" "bar")))

  (check (equal? (string-drop "" 0) ""))
  (check (runtime-error? (string-drop "" 1)))
  (check (runtime-error? (string-drop "" 2)))
  
  (check (equal? (string-drop "12345" 0) "12345"))
  (check (equal? (string-drop "12345" 1) "2345"))
  (check (equal? (string-drop "12345" 2) "345"))
  (check (equal? (string-drop "12345" 3) "45"))
  (check (equal? (string-drop "12345" 4) "5"))
  (check (equal? (string-drop "12345" 5) ""))
  (check (runtime-error? (string-drop "12345" 6))))

