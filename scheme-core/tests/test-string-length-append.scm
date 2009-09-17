(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-length-append
  (test-case (eq? 10 (string-length ts10a)))
  (test-case (eq? 20 (string-length ts20a)))
  (test-case (eq? 40 (string-length ts40a)))
   
  (test-case (equal? (string-append ts10a ts10a) ts20a))
  (test-case (eq? 20 (string-length (string-append ts10a ts10a))))
  (test-case (equal? (string-append "" ts10a) ts10a))
  (test-case (eq? 10 (string-length (string-append "" ts10a))))
  (test-case (equal? (string-append ts10a "") ts10a))
  (test-case (eq? 10 (string-length (string-append ts10a ""))))
   
  (test-case (equal? (string-append ts10a ts10a ts10a ts10a) ts40a))
  (test-case (eq? 40 (string-length (string-append ts10a ts10a ts10a ts10a))))
   
  (test-case (equal? (string-append "" "" "" ts10a) ts10a))
  (test-case (eq? 10 (string-length (string-append "" "" "" ts10a))))
  (test-case (equal? (string-append ts10a "" "" "") ts10a))
  (test-case (eq? 10 (string-length (string-append ts10a "" "" ""))))
  (test-case (equal? (string-append "" ts10a "" "") ts10a))
  (test-case (eq? 10 (string-length (string-append "" ts10a "" ""))))

  (test-case (equal? "foobar" (string-append :foo :bar)))
  (test-case (equal? "foobar" (string-append :fo :ob :ar)))
  (test-case (equal? "foobar" (string-append :foobar)))

  (test-case (equal? "foobar" (string-append "foo" "bar")))
  (test-case (equal? "foobar" (string-append "fo" "ob" "ar")))
  (test-case (equal? "foobar" (string-append "foobar")))
  
  (test-case (equal? "foobar" (string-append #\f "oo" #\b "a" #\r)))
  (test-case (equal? "foobar" (string-append #\f #\o #\o #\b #\a #\r)))
  (test-case (equal? "foobar" (string-append 102 111 111 98 97 114)))
  (test-case (equal? "foobar" (string-append #\f 111 #\o 98 97 #\r)))

  (test-case (runtime-error? (string-append -1 "foobar")))
  (test-case (runtime-error? (string-append "foo" -1 "bar")))
  (test-case (runtime-error? (string-append "foobar" -1)))
  
  (test-case (runtime-error? (string-append 256 "foobar")))
  (test-case (runtime-error? (string-append "foo" 256 "bar")))
  (test-case (runtime-error? (string-append "foobar" 256)))

  (test-case (runtime-error? (string-append 65.0 "foobar")))
  (test-case (runtime-error? (string-append "foo" 65.0 "bar")))
  (test-case (runtime-error? (string-append "foobar" 65.0)))

  (test-case (runtime-error? (string-append #t)))
  (test-case (runtime-error? (string-append '(no lists allowed))))
  (test-case (runtime-error? (string-append #(no vectors either))))
  )
