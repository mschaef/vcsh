(use-package! "unit-test")

(define-test write-strings
  (test-case (runtime-error? (write-strings)))
  (test-case (runtime-error? (write-strings :not-a-port)))
  (test-case (runtime-error? (write-strings (open-null-port) :not-a-string)))
  (test-case (runtime-error? (write-strings (open-null-port) "string" "yup, another" :not-a-string)))

  (let ((os (open-output-string)))
    (write-strings os "test")
    (test-case (equal? "test" (get-output-string os)))
    (write-strings os "case" #\1 "2" "3" "4" "5")
    (test-case (equal? "testcase12345" (get-output-string os)))

    (test-case (eq? os (write-strings os)))

    (test-case (equal? "testcase12345" (get-output-string os)))))


