(use-package! "unit-test")

(define-test write-strings
  (check (runtime-error? (write-strings)))
  (check (runtime-error? (write-strings :not-a-port)))
  (check (runtime-error? (write-strings (open-null-output-port) :not-a-string)))
  (check (runtime-error? (write-strings (open-null-output-port) "string" "yup, another" :not-a-string)))

  (let ((os (open-output-string)))
    (write-strings os "test")
    (check (equal? "test" (get-output-string os)))
    (write-strings os "case" #\1 "2" "3" "4" "5")
    (check (equal? "testcase12345" (get-output-string os)))

    (check (eq? os (write-strings os)))

    (check (equal? "testcase12345" (get-output-string os)))))


