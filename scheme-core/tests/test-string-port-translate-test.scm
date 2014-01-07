(use-package! "unit-test")

(defmacro (output-string port-var . code)
  `(let ((,port-var (open-output-string)))
     ,@code
     (get-output-string ,port-var)))


(define-test string-port-translate-test
  (test-case (equal? (output-string p 
                                    (display "hello" p))
                     "hello"))
  (test-case (equal? (output-string p 
                                    (display "hello\r" p))
                     "hello\r"))
  (test-case (equal? (output-string p 
                                    (display "hello\n" p))
                     "hello\n"))
  (test-case (equal? (output-string p 
                                    (display "hello\r\n" p))
                     "hello\r\n"))
  (test-case (equal? (output-string p 
                                    (display "\rhello" p))
                     "\rhello"))
  (test-case (equal? (output-string p 
                                    (display "\nhello" p))
                     "\nhello"))
  (test-case (equal? (output-string p 
                                    (display "\r\nhello" p))
                     "\r\nhello"))
  (test-case (equal? (output-string p 
                                    (display "hello\rworld" p))
                     "hello\rworld"))
  (test-case (equal? (output-string p 
                                    (display "hello\nworld" p))
                     "hello\nworld"))
  (test-case (equal? (output-string p 
                                    (display "hello\r\nworld" p))
                     "hello\r\nworld"))
  (test-case (equal? (output-string p 
                                    (display "hello" p)
                                    (display "world" p))
                     "helloworld"))
  (test-case (equal? (output-string p 
                                    (display "hello\r" p)
                                    (display "world" p))
                     "hello\rworld"))
  (test-case (equal? (output-string p 
                                    (display "hello\n" p)
                                    (display "world" p))
                     "hello\nworld"))
  (test-case (equal? (output-string p 
                                    (display "hello\r\n" p)
                                    (display "world" p))
                     "hello\r\nworld"))
  (test-case (equal? (output-string p 
                                    (display "hello\r" p)
                                    (display "\nworld" p))
                     "hello\r\nworld")))


