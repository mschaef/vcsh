(use-package! "unit-test")

(define-test string-drop-right
  (check (runtime-error? (string-drop-right :symbol 0)))
  (check (runtime-error? (string-drop-right 1234 0)))
  (check (runtime-error? (string-drop-right "foo" :symbol)))
  (check (runtime-error? (string-drop-right "foo" "bar")))

  (check (equal? (string-drop-right "" 0) ""))
  (check (runtime-error? (string-drop-right "" 1)))
  (check (runtime-error? (string-drop-right "" 2)))
  
  (check (equal? (string-drop-right "12345" 0) "12345"))
  (check (equal? (string-drop-right "12345" 1) "1234"))
  (check (equal? (string-drop-right "12345" 2) "123"))
  (check (equal? (string-drop-right "12345" 3) "12"))
  (check (equal? (string-drop-right "12345" 4) "1"))
  (check (equal? (string-drop-right "12345" 5) ""))
  (check (runtime-error? (string-drop-right "12345" 6))))
