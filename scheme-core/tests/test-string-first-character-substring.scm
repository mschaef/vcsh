(use-package! "unit-test")

(define-test string-first-character-substring
  ;; Test error hangling
  (check (runtime-error? (string-first-character 42 (charset-vector " \t\n"))))
  (check (runtime-error? (string-first-character "hello"  42)))
  (check (runtime-error? (string-first-substring 42 (charset-vector " \t\n"))))
  (check (runtime-error? (string-first-substring "hello" 42)))

  (check (runtime-error? (string-first-character "hello" " \t\n" :not-a-number)))
  (check (runtime-error? (string-first-substring "hello" " \t\n" :not-a-number)))

  ;; Character not found.
  (check (not (string-first-character "hello" (charset-vector " \t\n"))))
  (check (not (string-first-substring "hello" (charset-vector " \t\n"))))
  (check (not (string-first-character "hello" (charset-vector " \t\n") 0)))
  (check (not (string-first-substring "hello" (charset-vector " \t\n") 0)))

  ;; iniitial offset out of range
  (check (runtime-error? (string-first-character "hello" (charset-vector "h") -1)))
  (check (runtime-error? (string-first-substring "hello" (charset-vector "h") -1)))
  (check (not (string-first-character "hello" (charset-vector "h") 10)))
  (check (not (string-first-substring "hello" (charset-vector "h") 10)))

  ;; Character in first location
  (check (eq? 0 (string-first-character " \nhello" (charset-vector " \t\n"))))
  (check (eq? 0 (string-first-character "hello 123" (charset-vector "abcdefghijklmnopqrstuvwxyz"))))

  (check (eq? 0 (string-first-character " \nhello" (charset-vector " \t\n") 0)))
  (check (eq? 0 (string-first-character "hello 123" (charset-vector "abcdefghijklmnopqrstuvwxyz") 0)))

  (check (eq? 2 (string-first-substring " \nhello" (charset-vector " \t\n"))))
  (check (eq? 5 (string-first-substring "hello 123" (charset-vector "abcdefghijklmnopqrstuvwxyz"))))

  (check (not (string-first-substring " \nhello" (charset-vector " \t\n") 2)))
  (check (eq? 5 (string-first-substring "hello 123" (charset-vector "abcdefghijklmnopqrstuvwxyz") 2))))


