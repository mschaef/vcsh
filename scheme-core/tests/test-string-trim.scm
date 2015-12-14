(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-trim
  (check (equal? ts10a (string-trim ts10a)))
  (check (equal? ts10a (string-trim (string-append " " ts10a))))
  (check (equal? ts10a (string-trim (string-append " " ts10a " "))))
  (check (equal? ts10a (string-trim (string-append ts10a " "))))
  (check (equal? ts10a (string-trim (string-append "\t" ts10a))))
  (check (equal? ts10a (string-trim (string-append "\t" ts10a "\t"))))
  (check (equal? ts10a (string-trim (string-append ts10a "\t"))))
  (check (equal? ts10a (string-trim (string-append "\n" ts10a))))
  (check (equal? ts10a (string-trim (string-append "\n" ts10a "\n"))))
  (check (equal? ts10a (string-trim (string-append ts10a "\n"))))
  (check (equal? ts10a (string-trim (string-append "\t    \r \n" ts10a))))
  (check (equal? ts10a (string-trim (string-append "\t  \r  \n" ts10a "\n  \t  \t"))))
  (check (equal? ts10a (string-trim (string-append ts10a "\r  \n    \t"))))

  (let ((ts10ars (string-append ts10a "\t\r\n  "))
	(ts10als (string-append  "\t\r\n  " ts10a)))

    (check (runtime-error? (string-trim-left 123)))
    (check (runtime-error? (string-trim-right 123)))

    (check (equal? ts10a   (string-trim-right ts10ars)))
    (check (equal? ts10ars (string-trim-left  ts10ars)))
    (check (equal? ts10als (string-trim-right ts10als)))
    (check (equal? ts10a   (string-trim-left  ts10als)))
     
    (check (equal? ts10ars (string-trim-left  (string-append "   \t\r\n " ts10ars))))
    (check (equal? ts10als (string-trim-right (string-append ts10als "   \t\r\n ")))))

  (let ((ts-tl "  abcde")
	(ts-tr "abcde  ")
	(ts-tb " abcde "))
      
    (check (equal? "abcde" (string-trim-left  ts-tl)))
    (check (equal? "abcde" (string-trim-right ts-tr)))
    (check (equal? "abcde" (string-trim ts-tb)))
    
   ; Verify that string-trim is non-desctructive      
    (check (equal? ts-tl "  abcde"))
    (check (equal? ts-tr "abcde  "))
    (check (equal? ts-tb " abcde ")))

    ; Check custom trim characters
  (check (runtime-error? (string-trim "hello" 12)))

  (check (equal? " 123 @@@" (string-trim "??? 123 @@@" "?")))
  (check (equal? "??? 123 " (string-trim "??? 123 @@@" "@")))
  (check (equal? " 123 " (string-trim "??? 123 @@@" "?@")))
  (check (equal? "123" (string-trim "??? 123 @@@" "?@ ")))
  (check (equal? "123" (string-trim "??? 123 @@@" " ?@")))
  
  (check (equal? " 123 @@@" (string-trim-left "??? 123 @@@" "?")))
  (check (equal? "??? 123 @@@" (string-trim-left "??? 123 @@@" "@")))
  (check (equal? " 123 @@@" (string-trim-left "??? 123 @@@" "?@")))
  (check (equal? "123 @@@" (string-trim-left "??? 123 @@@" "?@ ")))
  (check (equal? "123 @@@" (string-trim-left "??? 123 @@@" " ?@")))
  
  (check (equal? "??? 123 @@@" (string-trim-right "??? 123 @@@" "?")))
  (check (equal? "??? 123 " (string-trim-right "??? 123 @@@" "@")))
  (check (equal? "??? 123 " (string-trim-right "??? 123 @@@" "?@")))
  (check (equal? "??? 123" (string-trim-right "??? 123 @@@" "?@ ")))
  (check (equal? "??? 123" (string-trim-right "??? 123 @@@" " ?@"))))
