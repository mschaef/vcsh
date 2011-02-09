(use-package! "unit-test")

(define-test string-first-character-substring
  ;; Test error hangling
  (test-case (runtime-error? (string-first-character 42 #.(charset-vector " \t\n"))))
  (test-case (runtime-error? (string-first-character "hello"  42)))
  (test-case (runtime-error? (string-first-substring 42 #.(charset-vector " \t\n"))))
  (test-case (runtime-error? (string-first-substring "hello" 42)))

  (test-case (runtime-error? (string-first-character "hello" " \t\n" :not-a-number)))
  (test-case (runtime-error? (string-first-substring "hello" " \t\n" :not-a-number)))

  ;; Character not found.
  (test-case (not (string-first-character "hello" #.(charset-vector " \t\n"))))
  (test-case (not (string-first-substring "hello" #.(charset-vector " \t\n"))))
  (test-case (not (string-first-character "hello" #.(charset-vector " \t\n") 0)))
  (test-case (not (string-first-substring "hello" #.(charset-vector " \t\n") 0)))

  ;; iniitial offset out of range
  (test-case (runtime-error? (string-first-character "hello" #.(charset-vector "h") -1)))
  (test-case (runtime-error? (string-first-substring "hello" #.(charset-vector "h") -1)))
  (test-case (not (string-first-character "hello" #.(charset-vector "h") 10)))
  (test-case (not (string-first-substring "hello" #.(charset-vector "h") 10)))

  ;; Character in first location
  (test-case (eq? 0 (string-first-character " \nhello" #.(charset-vector " \t\n"))))
  (test-case (eq? 0 (string-first-character "hello 123" #.(charset-vector "abcdefghijklmnopqrstuvwxyz"))))

  (test-case (eq? 0 (string-first-character " \nhello" #.(charset-vector " \t\n") 0)))
  (test-case (eq? 0 (string-first-character "hello 123" #.(charset-vector "abcdefghijklmnopqrstuvwxyz") 0)))

  (test-case (eq? 2 (string-first-substring " \nhello" #.(charset-vector " \t\n"))))
  (test-case (eq? 5 (string-first-substring "hello 123" #.(charset-vector "abcdefghijklmnopqrstuvwxyz"))))

  (test-case (not (string-first-substring " \nhello" #.(charset-vector " \t\n") 2)))
  (test-case (eq? 5 (string-first-substring "hello 123" #.(charset-vector "abcdefghijklmnopqrstuvwxyz") 2))))


