(define-package "test-string"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test string-comparison-ci/bad-arguments
  (check (runtime-error? (string>-ci 42 "string"))) 
  (check (runtime-error? (string>-ci "string" 42))) 
  (check (runtime-error? (string>-ci 21 42))) 

  (check (runtime-error? (string<-ci 42 "string"))) 
  (check (runtime-error? (string<-ci "string" 42))) 
  (check (runtime-error? (string<-ci 21 42))) 

  (check (runtime-error? (string>=-ci 42 "string")))
  (check (runtime-error? (string>=-ci "string" 42)))
  (check (runtime-error? (string>=-ci 21 42)))

  (check (runtime-error? (string<=-ci 42 "string")))
  (check (runtime-error? (string<=-ci "string" 42)))
  (check (runtime-error? (string<=-ci 21 42)))

  (check (runtime-error? (string=-ci 42 "string")))   
  (check (runtime-error? (string=-ci "string" 42)))   
  (check (runtime-error? (string=-ci 21 42)))   

  (check (runtime-error? (string!=-ci 42 "string")))
  (check (runtime-error? (string!=-ci "string" 42)))
  (check (runtime-error? (string!=-ci 21 42))))

(define-test string-comparison-ci
  (check (string<-ci "alpha" "omega"))
  (check (not (string<-ci "alpha" "alpha")))
  (check (not (string<-ci "omega" "alpha")))

  (check (string<=-ci "alpha" "omega"))
  (check (string<=-ci "alpha" "alpha"))
  (check (not (string<=-ci "omega" "alpha")))

  (check (not (string=-ci "alpha" "omega")))
  (check (string=-ci "alpha" "alpha"))
  (check (not (string=-ci "omega" "alpha")))

  (check (not (string>-ci "alpha" "omega")))
  (check (not (string>-ci "alpha" "alpha")))
  (check (string>-ci "omega" "alpha"))

  (check (not (string>=-ci "alpha" "omega")))
  (check (string>=-ci "alpha" "alpha"))
  (check (string>=-ci "omega" "alpha"))

  (check (string!=-ci "alpha" "omega"))
  (check (not (string!=-ci "alpha" "alpha")))
  (check (string!=-ci "omega" "alpha"))

  (check (string<-ci "ALPHA" "OMEGA"))
  (check (not (string<-ci "ALPHA" "ALPHA")))
  (check (not (string<-ci "OMEGA" "ALPHA")))

  (check (string<=-ci "ALPHA" "OMEGA"))
  (check (string<=-ci "ALPHA" "ALPHA"))
  (check (not (string<=-ci "OMEGA" "ALPHA")))

  (check (not (string=-ci "ALPHA" "OMEGA")))
  (check (string=-ci "ALPHA" "ALPHA"))
  (check (not (string=-ci "OMEGA" "ALPHA")))

  (check (not (string>-ci "ALPHA" "OMEGA")))
  (check (not (string>-ci "ALPHA" "ALPHA")))
  (check (string>-ci "OMEGA" "ALPHA"))

  (check (not (string>=-ci "ALPHA" "OMEGA")))
  (check (string>=-ci "ALPHA" "ALPHA"))
  (check (string>=-ci "OMEGA" "ALPHA"))

  (check (string!=-ci "ALPHA" "OMEGA"))
  (check (not (string!=-ci "ALPHA" "ALPHA")))
  (check (string!=-ci "OMEGA" "ALPHA"))

  (check (string<-ci "alpha" "OMEGA"))
  (check (not (string<-ci "alpha" "ALPHA")))
  (check (not (string<-ci "omega" "ALPHA")))

  (check (string<=-ci "alpha" "OMEGA"))
  (check (string<=-ci "alpha" "ALPHA"))
  (check (not (string<=-ci "omega" "ALPHA")))

  (check (not (string=-ci "alpha" "OMEGA")))
  (check (string=-ci "alpha" "ALPHA"))
  (check (not (string=-ci "omega" "ALPHA")))

  (check (not (string>-ci "alpha" "OMEGA")))
  (check (not (string>-ci "alpha" "ALPHA")))
  (check (string>-ci "omega" "ALPHA"))

  (check (not (string>=-ci "alpha" "OMEGA")))
  (check (string>=-ci "alpha" "ALPHA"))
  (check (string>=-ci "omega" "ALPHA"))

  (check (string!=-ci "alpha" "OMEGA"))
  (check (not (string!=-ci "alpha" "ALPHA")))
  (check (string!=-ci "omega" "ALPHA"))

  (check (string<-ci "ALPHA" "omega"))
  (check (not (string<-ci "ALPHA" "alpha")))
  (check (not (string<-ci "OMEGA" "alpha")))

  (check (string<=-ci "ALPHA" "omega"))
  (check (string<=-ci "ALPHA" "alpha"))
  (check (not (string<=-ci "OMEGA" "alpha")))

  (check (not (string=-ci "ALPHA" "omega")))
  (check (string=-ci "ALPHA" "alpha"))
  (check (not (string=-ci "OMEGA" "alpha")))

  (check (not (string>-ci "ALPHA" "omega")))
  (check (not (string>-ci "ALPHA" "alpha")))
  (check (string>-ci "OMEGA" "alpha"))

  (check (not (string>=-ci "ALPHA" "omega")))
  (check (string>=-ci "ALPHA" "alpha"))
  (check (string>=-ci "OMEGA" "alpha"))

  (check (string!=-ci "ALPHA" "omega"))
  (check (not (string!=-ci "ALPHA" "alpha")))
  (check (string!=-ci "OMEGA" "alpha")))


(define-test string-comparison/bad-arguments
  (check (runtime-error? (string> 42 "string"))) 
  (check (runtime-error? (string> "string" 42))) 
  (check (runtime-error? (string> 21 42))) 

  (check (runtime-error? (string< 42 "string"))) 
  (check (runtime-error? (string< "string" 42))) 
  (check (runtime-error? (string< 21 42))) 

  (check (runtime-error? (string>= 42 "string")))
  (check (runtime-error? (string>= "string" 42)))
  (check (runtime-error? (string>= 21 42)))

  (check (runtime-error? (string<= 42 "string")))
  (check (runtime-error? (string<= "string" 42)))
  (check (runtime-error? (string<= 21 42)))

  (check (runtime-error? (string= 42 "string")))   
  (check (runtime-error? (string= "string" 42)))   
  (check (runtime-error? (string= 21 42)))   

  (check (runtime-error? (string!= 42 "string")))
  (check (runtime-error? (string!= "string" 42)))
  (check (runtime-error? (string!= 21 42))))

(define-test string-comparison ; TODO: need string-comparison tests for case-sensitivity

  (check (string< "alpha" "omega"))
  (check (not (string< "alpha" "alpha")))
  (check (not (string< "omega" "alpha")))

  (check (string<= "alpha" "omega"))
  (check (string<= "alpha" "alpha"))
  (check (not (string<= "omega" "alpha")))

  (check (not (string= "alpha" "omega")))
  (check (string= "alpha" "alpha"))
  (check (not (string= "omega" "alpha")))

  (check (not (string> "alpha" "omega")))
  (check (not (string> "alpha" "alpha")))
  (check (string> "omega" "alpha"))

  (check (not (string>= "alpha" "omega")))
  (check (string>= "alpha" "alpha"))
  (check (string>= "omega" "alpha"))

  (check (string!= "alpha" "omega"))
  (check (not (string!= "alpha" "alpha")))
  (check (string!= "omega" "alpha")))


(define-test string-drop-right/bad-arguments
  (check (runtime-error? (string-drop-right :symbol 0)))
  (check (runtime-error? (string-drop-right 1234 0)))
  (check (runtime-error? (string-drop-right "foo" :symbol)))
  (check (runtime-error? (string-drop-right "foo" "bar"))))

(define-test string-drop-right/arguments-out-of-range
  (check (runtime-error? (string-drop-right "" 1)))
  (check (runtime-error? (string-drop-right "" 2)))
  (check (runtime-error? (string-drop-right "12345" 6))))

(define-test string-drop-right/positive
  (check (equal? "" (string-drop-right "" 0)))
  (check (equal? "12345" (string-drop-right "12345" 0)))
  (check (equal? "1234" (string-drop-right "12345" 1)))
  (check (equal? "" (string-drop-right "12345" 5))))

(define-test string-drop/bad-arguments
  (check (runtime-error? (string-drop :symbol 0)))
  (check (runtime-error? (string-drop 1234 0)))
  (check (runtime-error? (string-drop "foo" :symbol)))
  (check (runtime-error? (string-drop "foo" "bar"))))


(define-test string-drop/arguments-out-of-range
  (check (runtime-error? (string-drop "" 1)))
  (check (runtime-error? (string-drop "" 2)))
  (check (runtime-error? (string-drop "12345" 6))))

(define-test string-drop/positive
  (check (equal? "" (string-drop "" 0)))
  (check (equal? "12345" (string-drop "12345" 0)))
  (check (equal? "2345" (string-drop "12345" 1)))
  (check (equal? "" (string-drop "12345" 5))))

(define-test equal?/string-positive
  (check (equal? "" ""))
  (check (equal? "a" "a"))
  (check (equal? "abcde" "abcde")))

(define-test equal?/string-negative
  (check (not (equal? "hello" :non-string)))
  (check (not (equal?  :non-string "hello")))
  (check (not (equal? "hello" "HELLO"))))

(define-test string-first-character-substring
  ;; Test error hangling
  (check (runtime-error? (string-first-character 42 (charset-vector " \t\n"))))
  (check (runtime-error? (string-first-character "hello"  42)))
  (check (runtime-error? (string-first-substring 42 (charset-vector " \t\n"))))
  (check (runtime-error? (string-first-substring "hello" 42)))

  (check (runtime-error? (string-first-character "hello" " \t\n" :not-a-number)))
  (check (runtime-error? (string-first-substring "hello" " \t\n" :not-a-number)))

  ;; Character not found.
  (check (not (string-first-character "hello" (charset-vector " \t\n"))))
  (check (not (string-first-substring "hello" (charset-vector " \t\n"))))
  (check (not (string-first-character "hello" (charset-vector " \t\n") 0)))
  (check (not (string-first-substring "hello" (charset-vector " \t\n") 0)))

  ;; iniitial offset out of range
  (check (runtime-error? (string-first-character "hello" (charset-vector "h") -1)))
  (check (runtime-error? (string-first-substring "hello" (charset-vector "h") -1)))
  (check (not (string-first-character "hello" (charset-vector "h") 10)))
  (check (not (string-first-substring "hello" (charset-vector "h") 10)))

  ;; Character in first location
  (check (eq? 0 (string-first-character " \nhello" (charset-vector " \t\n"))))
  (check (eq? 0 (string-first-character "hello 123" (charset-vector "abcdefghijklmnopqrstuvwxyz"))))

  (check (eq? 0 (string-first-character " \nhello" (charset-vector " \t\n") 0)))
  (check (eq? 0 (string-first-character "hello 123" (charset-vector "abcdefghijklmnopqrstuvwxyz") 0)))

  (check (eq? 2 (string-first-substring " \nhello" (charset-vector " \t\n"))))
  (check (eq? 5 (string-first-substring "hello 123" (charset-vector "abcdefghijklmnopqrstuvwxyz"))))

  (check (not (string-first-substring " \nhello" (charset-vector " \t\n") 2)))
  (check (eq? 5 (string-first-substring "hello 123" (charset-vector "abcdefghijklmnopqrstuvwxyz") 2))))

(define-test string-fold/bad-arguments
  (check (runtime-error? (string-fold 12 '() "0123456789")))
  (check (runtime-error? (string-fold cons '() ()))))

(define-test string-fold
  (define (ccons a d) (cons a d))

  (check (equal? '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		 (reverse (string-fold cons '() "0123456789"))))
  (check (equal? '() (reverse (string-fold cons '() ""))))

  (check (equal? '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		 (reverse (string-fold ccons '() "0123456789"))))
  (check (equal? '() (reverse (string-fold ccons '() ""))))

  (check (runtime-error? (string-fold (lambda (ch chs) (error "foo")) () "12345")))
  (check (not (runtime-error? (string-fold (lambda (ch chs) (error "foo")) () ""))))

  (let ((catch-tag (gensym "string-fold-catch-tag"))
	(return-value (gensym "string-fold-return-value")))

    (check (eq? (catch catch-tag
		  (string-fold (lambda (ch chs) (throw catch-tag return-value)) () "12345"))
		return-value))

    (check
     (equal? '(1 2 3 4)
             (checkpoint-order-of
              (checkpoint 1)
              (catch catch-tag
                (checkpoint 2)
                (string-fold 
                 (lambda (ch chs) 
                   (checkpoint 3)
                   (throw catch-tag return-value)
                   (checkpoint #f))
                 () 
                 "12345")
                (checkpoint #f))
              (checkpoint 4))))))


(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-length/bad-arguments
  (check-for (val '(0 #\a [1 2]))
    (runtime-error? (string-length val))))

(define-test string-length/positive
  (check (= 0 (string-length "")))
  (check (= 1 (string-length "1")))
  (check (= 5 (string-length "12345"))))

(define-test string-length/argument-types
  (check (equal? "test" (string-append "test")))
  (check (equal? "test" (string-append :test)))
  (check (equal? "A" (string-append #\A)))
  (check (equal? "A" (string-append 65))))

(define-test string-append/positive
  (check (equal? "foobar" (string-append "foo" "bar")))
  (check (equal? "foobar" (string-append #\f "oo" 98 "a" :r))))

(define-test string-append/bad-arguments
  (check (runtime-error? (string-append -1)))
  (check (runtime-error? (string-append 256)))
  (check (runtime-error? (string-append 65.0)))
  (check (runtime-error? (string-append #t)))
  (check (runtime-error? (string-append '(no lists allowed))))
  (check (runtime-error? (string-append '[no vectors either]))))

(define-test length/string-port
  (let ((os (open-output-string)))
    (check (= 0 (length os)))

    (display "1" os)
    (check (= 1 (length os)))

    (display "2345" os)
    (check (= 5 (length os)))
    
    (display "\n" os)
    (check (= 6 (length os)))

    (display "67890" os)
    (check (= 11 (length os)))))

(define-test port-mode/string-port
  (let ((os (open-output-string))
        (is (open-input-string "test string")))
    (check (eq? :output (port-mode os)))
    (check (eq? :input (port-mode is)))))

(define-test port-mode
  (check (runtime-error? (port-mode #f)))
  (check (eq? :input (port-mode (open-null-input-port))))
  (check (eq? :output (port-mode (open-null-output-port))))

  (let ((ip (current-input-port)))
    (check (input-port? ip))
    (check (eq? :input (port-mode ip))))

  (let ((op (current-output-port)))
    (check (output-port? op))
    (check (eq? :output (port-mode op)))))


(define-test string-port-translate-test
  (define (display-to-string . vals)
    (with-output-to-string
      (dolist (val vals)
        (display val))))

  (check (equal? "hello" (display-to-string "hello")))
  (check (equal? "hello\r" (display-to-string "hello\r")))
  (check (equal? "hello\n" (display-to-string "hello\n")))
  (check (equal? "hello\r\n" (display-to-string "hello\r\n")))
  (check (equal? "\rhello" (display-to-string "\rhello")))
  (check (equal? "\nhello" (display-to-string "\nhello")))
  (check (equal? "\r\nhello" (display-to-string "\r\nhello")))
  (check (equal? "hello\rworld" (display-to-string "hello\rworld")))
  (check (equal? "hello\nworld" (display-to-string "hello\nworld")))
  (check (equal? "hello\r\nworld" (display-to-string "hello\r\nworld")))
  (check (equal? "helloworld" (display-to-string "hello" "world")))
  (check (equal? "hello\rworld" (display-to-string "hello\r" "world")))
  (check (equal? "hello\nworld" (display-to-string "hello\n" "world")))
  (check (equal? "hello\r\nworld" (display-to-string "hello\r\n" "world")))
  (check (equal? "hello\r\nworld" (display-to-string "hello\r" "\nworld"))))

(define-test string-quasiquote
  (let ((str-var "xyzzy")
        (num-var 12)
        (sym-var :test)
        (empty ""))
    (check (equal? #"$$" "$"))
    (check (equal? #"$$ " "$ "))
    (check (equal? #" $$" " $"))
    (check (equal? #" $$ " " $ "))

    (check (equal? #"${str-var}" "xyzzy"))
    (check (equal? #" ${str-var}" " xyzzy"))
    (check (equal? #"${str-var} " "xyzzy "))
    (check (equal? #" ${str-var} " " xyzzy "))

    (check (equal? #"${num-var}" "12"))
    (check (equal? #" ${num-var}" " 12"))
    (check (equal? #"${num-var} " "12 "))
    (check (equal? #" ${num-var} " " 12 "))

    (check (equal? #"${sym-var}" "test"))
    (check (equal? #" ${sym-var}" " test"))
    (check (equal? #"${sym-var} " "test "))
    (check (equal? #" ${sym-var} " " test "))

    (check (equal? #"$$${str-var}$$${num-var}$$" "$xyzzy$12$"))))


(define-test string-ref/non-string
  (check (runtime-error? (string-ref 123 123)))
  (check (runtime-error? (string-ref "1" 'non-a-number)))
  (check (runtime-error? (string-ref "1" 10.0)))
  (check (runtime-error? (string-ref "1" -10)))
  (check (runtime-error? (string-ref "1" 100))))

(define-test string-ref
  (let ((test-string "0123456789"))
    (check (eq? #\0 (string-ref test-string 0)))
    (check (eq? #\1 (string-ref test-string 1)))
    (check (eq? #\2 (string-ref test-string 2)))
    (check (eq? #\3 (string-ref test-string 3)))
    (check (eq? #\4 (string-ref test-string 4)))
    (check (eq? #\5 (string-ref test-string 5)))
    (check (eq? #\6 (string-ref test-string 6)))
    (check (eq? #\7 (string-ref test-string 7)))
    (check (eq? #\8 (string-ref test-string 8)))
    (check (eq? #\9 (string-ref test-string 9)))))


(define-test string-replace/bad-arguments
  (check (runtime-error? (string-replace 23 "old" "new")))
  (check (runtime-error? (string-replace "string" 23 "new")))
  (check (runtime-error? (string-replace "string" "old" 23)))
  (check (runtime-error? (string-replace "string" "old")))
  (check (runtime-error? (string-replace "string"))))

(define-test string-replace/replace-nothing
  (check (equal? "test" (string-replace "test" "old" "new"))))

(define-test string-replace/replace-verything
  (check (equal? "test" (string-replace "replace-everything" "replace-everything" "test"))))

(define-test string-replace

  (check (equal? "foo" (string-replace "foo" "@" "!")))
  (check (equal? "foo" (string-replace "foo" "@" "!")))
  (check (equal? "f!oo" (string-replace "f@oo" "@" "!")))
  (check (equal? "foo!" (string-replace "foo@" "@" "!")))
  (check (equal? "!f!oo" (string-replace "@f@oo" "@" "!")))
  (check (equal? "!f!oo!" (string-replace "@f@oo@" "@" "!")))

  (check (equal? "foo" (string-replace "foo" "@@" "!!")))
  (check (equal? "foo" (string-replace "foo" "@@" "!!")))
  (check (equal? "f!!oo" (string-replace "f@@oo" "@@" "!!")))
  (check (equal? "foo!!" (string-replace "foo@@" "@@" "!!")))
  (check (equal? "!!f!!oo" (string-replace "@@f@@oo" "@@" "!!")))
  (check (equal? "!!f!!oo!!" (string-replace "@@f@@oo@@" "@@" "!!")))

  (check (equal? "!@" (string-replace "@@@" "@@" "!")))

  (let ((original "this should not be changed"))
    (check (equal? "this is changed" (string-replace original "should not be" "is")))
    (check (equal? original "this should not be changed"))))


(define (make-all-byte-string)
  (with-output-to-string
    (dotimes (ii 256)
      (display (integer->char ii)))))

(define (make-all-byte-combo-string)
  (with-output-to-string
    (dotimes (ii 256)
      (dotimes (jj 256)
        (display (integer->char ii))
        (display (integer->char jj))))))

(define-test string-round-trip
  (check-for (n (iseq 0 256))
    (can-read/write-round-trip? (make-string 1 (integer->char n))))
  (check-for (n (iseq 0 256))
    (can-read/write-round-trip? (make-string 3 (integer->char n))))

  (let ((ab-str (make-all-byte-string))
        (abc-str (make-all-byte-combo-string)))
    (check (can-read/write-round-trip? "\t\n"))
    (check (can-read/write-round-trip? " \n hello\n\t"))
    (check (can-read/write-round-trip? "\"\"\""))
    (check (can-read/write-round-trip? "01234567890abcdefghihjkilmnopqrstuvwxyz"))
    (check (can-read/write-round-trip? ab-str))
    (check (can-read/write-round-trip? abc-str))
    (check (= (length abc-str) (* 256 256 2)))))


(define string-escapes/reader
  (check (= 0  (char->integer (string-ref "\000" 0))))
  (check (= 1  (char->integer (string-ref "\1" 0))))
  (check (= 2  (char->integer (string-ref "\2" 0))))
  (check (= 3  (char->integer (string-ref "\3" 0))))
  (check (= 4  (char->integer (string-ref "\4" 0))))
  (check (= 5  (char->integer (string-ref "\5" 0))))
  (check (= 6  (char->integer (string-ref "\6" 0))))
  (check (= 7  (char->integer (string-ref "\7" 0))))
  (check (= 8  (char->integer (string-ref "\10" 0))))
  (check (= 9  (char->integer (string-ref "\11" 0))))
  (check (= 10 (char->integer (string-ref "\12" 0))))
  (check (= 11 (char->integer (string-ref "\13" 0))))
  (check (= 12 (char->integer (string-ref "\14" 0))))
  (check (= 13 (char->integer (string-ref "\15" 0))))
  (check (= 14 (char->integer (string-ref "\16" 0))))
  (check (= 15 (char->integer (string-ref "\17" 0))))
  (check (= 16 (char->integer (string-ref "\20" 0))))
  (check (= 17 (char->integer (string-ref "\21" 0))))
  (check (= 18 (char->integer (string-ref "\22" 0))))
  (check (= 19 (char->integer (string-ref "\23" 0))))

  (check (= 1  (char->integer (string-ref "\19" 0))))
  (check (= #\9 (string-ref "\19" 1))))

(define-test string-escapes

  (check (equal? (write-to-string (make-string 1 (integer->char 0  ))) "\"\\000\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 1  ))) "\"\\001\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 2  ))) "\"\\002\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 3  ))) "\"\\003\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 4  ))) "\"\\004\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 5  ))) "\"\\005\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 6  ))) "\"\\006\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 7  ))) "\"\\007\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 8  ))) "\"\\010\""))

  (check (equal? (write-to-string (make-string 1 #\\               ))  "\"\\\\\""))
  (check (equal? (write-to-string (make-string 1 #\"               ))  "\"\\\"\""))
  (check (equal? (write-to-string (make-string 1 #\newline         ))  "\"\\n\"" ))
  (check (equal? (write-to-string (make-string 1 #\cr              ))  "\"\\r\"" ))
  (check (equal? (write-to-string (make-string 1 #\tab             ))  "\"\\t\"" ))

  (check (equal? (write-to-string (make-string 1 (integer->char 128))) "\"\\200\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 129))) "\"\\201\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 130))) "\"\\202\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 131))) "\"\\203\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 132))) "\"\\204\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 133))) "\"\\205\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 134))) "\"\\206\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 135))) "\"\\207\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 136))) "\"\\210\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 192))) "\"\\300\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 193))) "\"\\301\""))
  (check (equal? (write-to-string (make-string 1 (integer->char 255))) "\"\\377\"")))



(define-test string-search-from-right
  (check (equal? 2  (string-search-from-right "345" "1234567890")))
  (check (equal? #f (string-search-from-right "34567890a" "1234567890")))
  (check (equal? #f (string-search-from-right "hello world" "1234567890")))
  (check (equal? 0  (string-search-from-right "1" "1234567890")))
  (check (equal? 9  (string-search-from-right "0" "1234567890")))
  (check (equal? 6  (string-search-from-right "2" "1234512345")))
  (check (equal? #f (string-search-from-right "2" "1234512345" 0)))
  (check (equal? 1  (string-search-from-right "2" "1234512345" 4)))
  (check (equal? 1  (string-search-from-right "234" "1234512345" 4)))
  (check (equal? #f (string-search-from-right "234" "1234512345" 1)))
  (check (equal? 1  (string-search-from-right "234" "1234512345" 6)))
  (check (equal? #f (string-search-from-right "234" "1234512345" 60)))
  (check (equal? 6  (string-search-from-right "234" "1234512345" 8)))
  (check (equal? 12 (string-search-from-right "a" "abaabaaabaaaab")))
  (check (equal? 12 (string-search-from-right "ab" "abaabaaabaaaab")))
  (check (equal? 10 (string-search-from-right "aaa"  "abaabaaabaaaab")))
  (check (equal? 10 (string-search-from-right "aaab" "abaabaaabaaaab")))

  (check (equal? 1  (string-search-from-right "aaab" "aaaab")))
  (check (equal? #f  (string-search-from-right "aaab" "aaaab" 1)))
  (check (equal? #f  (string-search-from-right "aaab" "aaaab" 2)))

  (check (equal? 0  (string-search-from-right #\1 "1234567890")))
  (check (equal? 9  (string-search-from-right #\0 "1234567890")))
  (check (equal? 6  (string-search-from-right #\2 "1234512345")))
  (check (equal? #f (string-search-from-right #\2 "1234512345" 0)))
  (check (equal? 1  (string-search-from-right #\2 "1234512345" 4)))
  (check (equal? 12 (string-search-from-right #\a "abaabaaabaaaab"))))


(define-test string-search
  (check (equal? 2  (string-search "345" "1234567890")))
  (check (equal? #f (string-search "34567890a" "1234567890")))
  (check (equal? #f (string-search "hello world" "1234567890")))
  (check (equal? 0  (string-search "1" "1234567890")))
  (check (equal? 9  (string-search "0" "1234567890")))
  (check (equal? 1  (string-search "2" "1234512345")))
  (check (equal? 1  (string-search "2" "1234512345" 0)))
  (check (equal? 6  (string-search "2" "1234512345" 4)))
  (check (equal? 6  (string-search "234" "1234512345" 4)))
  (check (equal? 1  (string-search "234" "1234512345" 1)))
  (check (equal? 6  (string-search "234" "1234512345" 6)))
  (check (equal? #f (string-search "234" "1234512345" 60)))
  (check (equal? #f (string-search "234" "1234512345" 8)))
  (check (equal? 0  (string-search "ab" "abaabaaabaaaab")))
  (check (equal? 5  (string-search "aaa"  "abaabaaabaaaab"))   )
  (check (equal? 5  (string-search "aaab" "abaabaaabaaaab")))

  (check (equal? 1  (string-search "aaab" "aaaab")))
  (check (equal? 1  (string-search "aaab" "aaaab" 1)))
  (check (equal? #f  (string-search "aaab" "aaaab" 2)))

  (check (equal? 0  (string-search #\1 "1234567890")))
  (check (equal? 9  (string-search #\0 "1234567890")))
  (check (equal? 1  (string-search #\2 "1234512345")))
  (check (equal? 1  (string-search #\2 "1234512345" 0)))
  (check (equal? 6  (string-search #\2 "1234512345" 4)))
  (check (equal? 0  (string-search #\a "abaabaaabaaaab"))))

(define-test string-set!/bad-arguments
  (let ((test-string "0123456789"))
    (check (runtime-error? (string-set! test-string 1.0 #\c)))
    (check (runtime-error? (string-set! 12 1 #\c)))
    (check (runtime-error? (string-set! 'string-set-doesnt-support-symbols 1 #\c)))
    (check (runtime-error? (string-set! test-string -1 #\a)))
    (check (runtime-error? (string-set! test-string 20 #\a)))
    (check (runtime-error? (string-set! test-string 'no-symbols-here-either)))
    (check (runtime-error? (string-set! test-string "nope. no strings")))
    (check (runtime-error? (string-set! test-string 12 "not here either.")))
    (check (runtime-error? (string-set! test-string 3.14159)))
    ;; test-string should be unaltered
    (check (equal? test-string "0123456789"))))

(define-test string-set!
   (let ((ts-set (string-copy "foobar")))
    (check (equal? ts-set (string-set! (string-copy "*oobar") 0 #\f)))
    (check (equal? ts-set (string-set! (string-copy "*oobar") 0 102)))
    (check (equal? "*oobar" (string-set! ts-set 0 #\*)))
    (check (equal? "*oobar" ts-set))))

(define-test string-take-right/bad-arguments
  (check (runtime-error? (string-take-right :symbol 0)))
  (check (runtime-error? (string-take-right 1234 0)))
  (check (runtime-error? (string-take-right "foo" :symbol)))
  (check (runtime-error? (string-take-right "foo" "bar"))))

(define-test string-take-right/arguments-out-of-range
  (check (runtime-error? (string-take-right "" 1)))
  (check (runtime-error? (string-take-right "" 2)))
  (check (runtime-error? (string-take-right "12345" 6))))

(define-test string-take-right/positive
  (check (equal? (string-take-right "" 0) ""))
  (check (equal? (string-take-right "12345" 0) ""))
  (check (equal? (string-take-right "12345" 1) "5"))
  (check (equal? (string-take-right "12345" 5) "12345")))

(define-test string-take/bad-arguments
  (check (runtime-error? (string-take :symbol 0)))
  (check (runtime-error? (string-take 1234 0)))
  (check (runtime-error? (string-take "foo" :symbol)))
  (check (runtime-error? (string-take "foo" "bar"))))

(define-test string-take/arguments-out-of-range
  (check (runtime-error? (string-take "" 1)))
  (check (runtime-error? (string-take "" 2)))
  (check (runtime-error? (string-take "12345" 6))))

(define-test string-take
  (check (equal? "" (string-take "" 0)))
  (check (equal? "" (string-take "12345" 0)))
  (check (equal? "1" (string-take "12345" 1)))
  (check (equal? "12345" (string-take "12345" 5))))


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


(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-upcase-string-downcase
  (check (runtime-error? (string-upcase 123)))
  (check (runtime-error? (string-downcase 123)))
   
  (check (equal? ts10a (string-upcase ts10a)))
  (check (equal? ts10a (string-downcase ts10a)))
   
  (check (equal? "UPPERCASE" (string-upcase "UPPERCASE")))
  (check (equal? "UPPERCASE" (string-upcase "UpPerCasE")))
  (check (equal? "test" (string-downcase "test")))
  (check (equal? "test" (string-downcase "TEsT")))
  
  (let ((ts-a "abcde")
	(ts-b "ABCDE"))
    (string-upcase ts-a)
    (string-upcase ts-b)
    
    (check (equal? ts-a "abcde"))
    (check (equal? ts-b "ABCDE"))))


(define-test string->number
  (check (runtime-error? (string->number 123 12)))
  (check (runtime-error? (string->number "123" 1)))
  (check (runtime-error? (string->number "123" -1)))
  (check (runtime-error? (string->number "123" 37)))
  (check (runtime-error? (string->number "123" 3.7)))
  (check (runtime-error? (string->number "123" :keyword)))
    
  (check (equal? #f (string->number "")))
  (check (equal? #f (string->number "" 2)))
  (check (equal? #f (string->number "" 8)))
  (check (equal? #f (string->number "" 10)))
  (check (equal? #f (string->number "" 16)))

  (check (equal? 0 (string->number "0")))
  (check (equal? 0 (string->number "-0")))
  (check (equal? 12 (string->number "12")))
  (check (equal? -12 (string->number "-12")))

  (check (equal? -1.2 (string->number "-1.2")))  

  (check (equal? 0 (string->number "0" 10)))
  (check (equal? 0 (string->number "-0" 10)))
  (check (equal? 12 (string->number "12" 10)))
  (check (equal? -12 (string->number "-12" 10)))
  (check (equal? #f (string->number "12f" 10)))
  (check (equal? #f (string->number "-12f" 10)))
  (check (equal? #f (string->number "-12g" 10)))
  (check (equal? #f (string->number "12-2" 10)))
  (check (equal? #f (string->number "12+2" 10)))
  (check (equal? #f (string->number "12i" 10)))
  
  (check (equal? 0 (string->number "0" 16)))
  (check (equal? 0 (string->number "-0" 16)))
  (check (equal? 18 (string->number "12" 16)))
  (check (equal? #f (string->number "1.2" 16)))
  (check (equal? -18 (string->number "-12" 16)))
  (check (equal? 51966 (string->number "cafe" 16)))
  (check (equal? -51966 (string->number "-cafe" 16)))
  (check (equal? 51966 (string->number "CAFE" 16)))
  (check (equal? #f (string->number "CA.FE" 16)))
  (check (equal? -51966 (string->number "-CAFE" 16)))

  (check (equal? 0 (string->number "0" 8)))
  (check (equal? 0 (string->number "-0" 8)))
  (check (equal? 10 (string->number "12" 8)))
  (check (equal? -10 (string->number "-12" 8)))
  (check (equal? #f (string->number "8" 8)))
  (check (equal? #f (string->number "9" 8)))
  (check (equal? #f (string->number "18" 8)))
  (check (equal? #f (string->number "19" 8)))
  (check (equal? #f (string->number "12f" 8)))
  (check (equal? #f (string->number "-12f" 8)))
  (check (equal? #f (string->number "-12g" 8)))
  (check (equal? #f (string->number "12-2" 8)))
  (check (equal? #f (string->number "12+2" 8)))
  (check (equal? #f (string->number "12i" 8)))

  (check (equal? 0 (string->number "0" 2)))
  (check (equal? 0 (string->number "-0" 2)))
  (check (equal? 12 (string->number "1100" 2)))
  (check (equal? -12 (string->number "-1100" 2)))

  (check (equal? #f (string->number "8" 2)))
  (check (equal? #f (string->number "9" 2)))
  (check (equal? #f (string->number "18" 2)))
  (check (equal? #f (string->number "19" 2)))
  (check (equal? #f (string->number "12f" 2)))
  (check (equal? #f (string->number "-12f" 2)))
  (check (equal? #f (string->number "-12g" 2)))
  (check (equal? #f (string->number "12-2" 2)))
  (check (equal? #f (string->number "12+2" 2)))
  (check (equal? #f (string->number "12i" 2))))


(define-test string?/non-string
  (check-for (non-string '(0 0.0 'symbol [] {}))
    (not (string? non-string))))

(define-test string?/string
  (check-for (string '("" "a" "abc"))
    (check  (eq? string (string? string)))))

;; TESTTHIS: Test substring with null end offset

(define-test substring/bad-arguments
  (let ((test-string "0123456789"))
    (check (runtime-error? (substring 123 2 3)))
    (check (runtime-error? (substring test-string 'non-number 3)))
    (check (runtime-error? (substring test-string 3 'non-number)))))

(define-test substring/arguments-out-of-range
  (let ((test-string "0123456789"))
    (check (runtime-error? (substring test-string -1 5)))
    (check (runtime-error? (substring test-string  6 5)))
    (check (runtime-error? (substring test-string  6 -1)))
    (check (runtime-error? (substring test-string  6 11)))))

(define-test substring
  (let ((test-string "0123456789"))
    (check (equal? test-string (substring test-string 0 10)))
    (check (equal? "" (substring test-string 0 0)))
    (check (equal? "01" (substring test-string 0 2)))
    (check (equal? "" (substring test-string 2 2)))
    (check (equal? "234" (substring test-string 2 5)))
    (check (equal? "23456789" (substring test-string 2)))))
