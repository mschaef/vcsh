(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-length-append
  (check (eq? 10 (string-length ts10a)))
  (check (eq? 20 (string-length ts20a)))
  (check (eq? 40 (string-length ts40a)))
   
  (check (equal? (string-append ts10a ts10a) ts20a))
  (check (eq? 20 (string-length (string-append ts10a ts10a))))
  (check (equal? (string-append "" ts10a) ts10a))
  (check (eq? 10 (string-length (string-append "" ts10a))))
  (check (equal? (string-append ts10a "") ts10a))
  (check (eq? 10 (string-length (string-append ts10a ""))))
   
  (check (equal? (string-append ts10a ts10a ts10a ts10a) ts40a))
  (check (eq? 40 (string-length (string-append ts10a ts10a ts10a ts10a))))
   
  (check (equal? (string-append "" "" "" ts10a) ts10a))
  (check (eq? 10 (string-length (string-append "" "" "" ts10a))))
  (check (equal? (string-append ts10a "" "" "") ts10a))
  (check (eq? 10 (string-length (string-append ts10a "" "" ""))))
  (check (equal? (string-append "" ts10a "" "") ts10a))
  (check (eq? 10 (string-length (string-append "" ts10a "" ""))))

  (check (equal? "foobar" (string-append :foo :bar)))
  (check (equal? "foobar" (string-append :fo :ob :ar)))
  (check (equal? "foobar" (string-append :foobar)))

  (check (equal? "foobar" (string-append "foo" "bar")))
  (check (equal? "foobar" (string-append "fo" "ob" "ar")))
  (check (equal? "foobar" (string-append "foobar")))
  
  (check (equal? "foobar" (string-append #\f "oo" #\b "a" #\r)))
  (check (equal? "foobar" (string-append #\f #\o #\o #\b #\a #\r)))
  (check (equal? "foobar" (string-append 102 111 111 98 97 114)))
  (check (equal? "foobar" (string-append #\f 111 #\o 98 97 #\r)))

  (check (runtime-error? (string-append -1 "foobar")))
  (check (runtime-error? (string-append "foo" -1 "bar")))
  (check (runtime-error? (string-append "foobar" -1)))
  
  (check (runtime-error? (string-append 256 "foobar")))
  (check (runtime-error? (string-append "foo" 256 "bar")))
  (check (runtime-error? (string-append "foobar" 256)))

  (check (runtime-error? (string-append 65.0 "foobar")))
  (check (runtime-error? (string-append "foo" 65.0 "bar")))
  (check (runtime-error? (string-append "foobar" 65.0)))

  (check (runtime-error? (string-append #t)))
  (check (runtime-error? (string-append '(no lists allowed))))
  (check (runtime-error? (string-append '[no vectors either]))))
