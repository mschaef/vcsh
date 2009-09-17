(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-trim
  (test-case (equal? ts10a (string-trim ts10a)))
  (test-case (equal? ts10a (string-trim (string-append " " ts10a))))
  (test-case (equal? ts10a (string-trim (string-append " " ts10a " "))))
  (test-case (equal? ts10a (string-trim (string-append ts10a " "))))
  (test-case (equal? ts10a (string-trim (string-append "\t" ts10a))))
  (test-case (equal? ts10a (string-trim (string-append "\t" ts10a "\t"))))
  (test-case (equal? ts10a (string-trim (string-append ts10a "\t"))))
  (test-case (equal? ts10a (string-trim (string-append "\n" ts10a))))
  (test-case (equal? ts10a (string-trim (string-append "\n" ts10a "\n"))))
  (test-case (equal? ts10a (string-trim (string-append ts10a "\n"))))
  (test-case (equal? ts10a (string-trim (string-append "\t    \r \n" ts10a))))
  (test-case (equal? ts10a (string-trim (string-append "\t  \r  \n" ts10a "\n  \t  \t"))))
  (test-case (equal? ts10a (string-trim (string-append ts10a "\r  \n    \t"))))

  (let ((ts10ars (string-append ts10a "\t\r\n  "))
	(ts10als (string-append  "\t\r\n  " ts10a)))

    (test-case (runtime-error? (string-trim-left 123)))
    (test-case (runtime-error? (string-trim-right 123)))

    (test-case (equal? ts10a   (string-trim-right ts10ars)))
    (test-case (equal? ts10ars (string-trim-left  ts10ars)))
    (test-case (equal? ts10als (string-trim-right ts10als)))
    (test-case (equal? ts10a   (string-trim-left  ts10als)))
     
    (test-case (equal? ts10ars (string-trim-left  (string-append "   \t\r\n " ts10ars))))
    (test-case (equal? ts10als (string-trim-right (string-append ts10als "   \t\r\n "))))
    )

  (let ((ts-tl "  abcde")
	(ts-tr "abcde  ")
	(ts-tb " abcde "))
      
    (test-case (equal? "abcde" (string-trim-left  ts-tl)))
    (test-case (equal? "abcde" (string-trim-right ts-tr)))
    (test-case (equal? "abcde" (string-trim ts-tb)))
    
   ; Verify that string-trim is non-desctructive      
    (test-case (equal? ts-tl "  abcde"))
    (test-case (equal? ts-tr "abcde  "))
    (test-case (equal? ts-tb " abcde ")))

    ; Check custom trim characters
  (test-case (runtime-error? (string-trim "hello" 12)))

  (test-case (equal? " 123 @@@" (string-trim "??? 123 @@@" "?")))
  (test-case (equal? "??? 123 " (string-trim "??? 123 @@@" "@")))
  (test-case (equal? " 123 " (string-trim "??? 123 @@@" "?@")))
  (test-case (equal? "123" (string-trim "??? 123 @@@" "?@ ")))
  (test-case (equal? "123" (string-trim "??? 123 @@@" " ?@")))
  
  (test-case (equal? " 123 @@@" (string-trim-left "??? 123 @@@" "?")))
  (test-case (equal? "??? 123 @@@" (string-trim-left "??? 123 @@@" "@")))
  (test-case (equal? " 123 @@@" (string-trim-left "??? 123 @@@" "?@")))
  (test-case (equal? "123 @@@" (string-trim-left "??? 123 @@@" "?@ ")))
  (test-case (equal? "123 @@@" (string-trim-left "??? 123 @@@" " ?@")))
  
  (test-case (equal? "??? 123 @@@" (string-trim-right "??? 123 @@@" "?")))
  (test-case (equal? "??? 123 " (string-trim-right "??? 123 @@@" "@")))
  (test-case (equal? "??? 123 " (string-trim-right "??? 123 @@@" "?@")))
  (test-case (equal? "??? 123" (string-trim-right "??? 123 @@@" "?@ ")))
  (test-case (equal? "??? 123" (string-trim-right "??? 123 @@@" " ?@"))))

