(use-package! "unit-test")

(define-test string-first-character-substring
  ;; Test error hangling
  (test-case (runtime-error? (string-first-character 42 " \t\n")))
  (test-case (runtime-error? (string-first-character "hello"  42)))
  (test-case (runtime-error? (string-first-substring 42 " \t\n")))
  (test-case (runtime-error? (string-first-substring "hello" 42)))

  (test-case (runtime-error? (string-first-character "hello" " \t\n" :not-a-number)))
  (test-case (runtime-error? (string-first-substring "hello" " \t\n" :not-a-number)))

  ;; Character not found.
  (test-case (not (string-first-character "hello" " \t\n")))
  (test-case (not (string-first-substring "hello" " \t\n")))
  (test-case (not (string-first-character "hello" " \t\n" 0)))
  (test-case (not (string-first-substring "hello" " \t\n" 0)))

  ;; iniitial offset out of range
  (test-case (runtime-error? (string-first-character "hello" "h" -1)))
  (test-case (runtime-error? (string-first-substring "hello" "h" -1)))
  (test-case (not (string-first-character "hello" "h" 10)))
  (test-case (not (string-first-substring "hello" "h" 10)))

  ;; Character in first location
  (test-case (eq? 0 (string-first-character " \nhello" " \t\n")))
  (test-case (eq? 0 (string-first-character "hello 123" "abcdefghijklmnopqrstuvwxyz")))

  (test-case (eq? 0 (string-first-character " \nhello" " \t\n" 0)))
  (test-case (eq? 0 (string-first-character "hello 123" "abcdefghijklmnopqrstuvwxyz" 0)))

  (test-case (eq? 2 (string-first-substring " \nhello" " \t\n")))
  (test-case (eq? 5 (string-first-substring "hello 123" "abcdefghijklmnopqrstuvwxyz")))

  (test-case (not (string-first-substring " \nhello" " \t\n" 2)))
  (test-case (eq? 5 (string-first-substring "hello 123" "abcdefghijklmnopqrstuvwxyz" 2)))
  )


