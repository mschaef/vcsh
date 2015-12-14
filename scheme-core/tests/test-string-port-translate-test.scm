(use-package! "unit-test")

(defmacro (output-string port-var . code)
  `(let ((,port-var (open-output-string)))
     ,@code
     (get-output-string ,port-var)))


(define-test string-port-translate-test
  (check (equal? (output-string p 
                                    (display "hello" p))
                     "hello"))
  (check (equal? (output-string p 
                                    (display "hello\r" p))
                     "hello\r"))
  (check (equal? (output-string p 
                                    (display "hello\n" p))
                     "hello\n"))
  (check (equal? (output-string p 
                                    (display "hello\r\n" p))
                     "hello\r\n"))
  (check (equal? (output-string p 
                                    (display "\rhello" p))
                     "\rhello"))
  (check (equal? (output-string p 
                                    (display "\nhello" p))
                     "\nhello"))
  (check (equal? (output-string p 
                                    (display "\r\nhello" p))
                     "\r\nhello"))
  (check (equal? (output-string p 
                                    (display "hello\rworld" p))
                     "hello\rworld"))
  (check (equal? (output-string p 
                                    (display "hello\nworld" p))
                     "hello\nworld"))
  (check (equal? (output-string p 
                                    (display "hello\r\nworld" p))
                     "hello\r\nworld"))
  (check (equal? (output-string p 
                                    (display "hello" p)
                                    (display "world" p))
                     "helloworld"))
  (check (equal? (output-string p 
                                    (display "hello\r" p)
                                    (display "world" p))
                     "hello\rworld"))
  (check (equal? (output-string p 
                                    (display "hello\n" p)
                                    (display "world" p))
                     "hello\nworld"))
  (check (equal? (output-string p 
                                    (display "hello\r\n" p)
                                    (display "world" p))
                     "hello\r\nworld"))
  (check (equal? (output-string p 
                                    (display "hello\r" p)
                                    (display "\nworld" p))
                     "hello\r\nworld")))


