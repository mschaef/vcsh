(define-package "test-csv"
  (:requires "csv")
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"
         "csv"))

(define-test csv-line-endings
  (check (equal? (csv-string->list "1,2,\"a\"\n3,4,\"b\"\n")
                 '((1 2 "a") (3 4 "b"))))
  ;; CR+LF line endings (as in RFC 4180) read the same as LF.
  (check (equal? (csv-string->list "1,2,\"a\"\r\n3,4,\"b\"\r\n")
                 '((1 2 "a") (3 4 "b"))))
  (check (equal? (csv-string->list "1,2\r\n3,4")
                 '((1 2) (3 4)))))
