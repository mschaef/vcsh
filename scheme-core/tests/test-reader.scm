(define-package "test-reader"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test read
  (check (runtime-error? (read 12)))
  (check (runtime-error? (read (current-output-port) #f))))


(define-test read/write-character
  (check (char=? (integer->char 97) (read-from-string "#\\a")))
  (check (char=? (integer->char 98) (read-from-string "#\\b")))
  (check (char=? (integer->char 90) (read-from-string "#\\Z")))
  (check (char=? (integer->char 59) (read-from-string "#\\;")))
  (check (char=? (integer->char 64) (read-from-string "#\\@")))
  (check (char=? (integer->char 35) (read-from-string "#\\#")))
  (check (char=? (integer->char 60) (read-from-string "#\\<")))
  (check (char=? (integer->char 65) (read-from-string "#\\<65>")))
  (check (char=? (integer->char 60) (read-from-string "#\\< ")))
  (check (char=? (integer->char 65) (read-from-string "#\\<65> ")))
  (check (char=? (integer->char 0) (read-from-string "#\\nul")))
  (check (char=? (integer->char 1) (read-from-string "#\\soh")))
  (check (char=? (integer->char 2) (read-from-string "#\\stx")))
  (check (char=? (integer->char 3) (read-from-string "#\\etx")))
  (check (char=? (integer->char 4) (read-from-string "#\\eot")))
  (check (char=? (integer->char 5) (read-from-string "#\\eng")))
  (check (char=? (integer->char 6) (read-from-string "#\\ack")))
  (check (char=? (integer->char 7) (read-from-string "#\\bel")))
  (check (char=? (integer->char 8) (read-from-string "#\\bs")))
  (check (char=? (integer->char 9) (read-from-string "#\\tab")))
  (check (char=? (integer->char 10) (read-from-string "#\\newline")))
  (check (char=? (integer->char 11) (read-from-string "#\\vtab")))
  (check (char=? (integer->char 12) (read-from-string "#\\formfeed")))
  (check (char=? (integer->char 13) (read-from-string "#\\cr")))
  (check (char=? (integer->char 14) (read-from-string "#\\so")))
  (check (char=? (integer->char 15) (read-from-string "#\\si")))
  (check (char=? (integer->char 16) (read-from-string "#\\dle")))
  (check (char=? (integer->char 17) (read-from-string "#\\dc1")))
  (check (char=? (integer->char 18) (read-from-string "#\\dc2")))
  (check (char=? (integer->char 19) (read-from-string "#\\dc3")))
  (check (char=? (integer->char 20) (read-from-string "#\\dc4")))
  (check (char=? (integer->char 21) (read-from-string "#\\nak")))
  (check (char=? (integer->char 22) (read-from-string "#\\syn")))
  (check (char=? (integer->char 23) (read-from-string "#\\etb")))
  (check (char=? (integer->char 24) (read-from-string "#\\can")))
  (check (char=? (integer->char 25) (read-from-string "#\\em")))
  (check (char=? (integer->char 26) (read-from-string "#\\sub")))
  (check (char=? (integer->char 27) (read-from-string "#\\esc")))
  (check (char=? (integer->char 28) (read-from-string "#\\fs")))
  (check (char=? (integer->char 29) (read-from-string "#\\gs")))
  (check (char=? (integer->char 30) (read-from-string "#\\rs")))
  (check (char=? (integer->char 31) (read-from-string "#\\us")))
  (check (char=? (integer->char 32) (read-from-string "#\\space")))

  (check (equal? "#\\a" (write-to-string #\a)))
  (check (equal? "#\\b" (write-to-string #\b)))
  (check (equal? "#\\Z" (write-to-string #\Z)))

  (check (equal? "#\\newline" (write-to-string #\newline)))
  (check (equal? "#\\space" (write-to-string #\space)))

  (check (equal? (write-to-string (integer->char 0)) "#\\nul"))
  (check (equal? (write-to-string (integer->char 1)) "#\\soh"))
  (check (equal? (write-to-string (integer->char 2)) "#\\stx"))
  (check (equal? (write-to-string (integer->char 3)) "#\\etx"))
  (check (equal? (write-to-string (integer->char 4)) "#\\eot"))
  (check (equal? (write-to-string (integer->char 5)) "#\\eng"))
  (check (equal? (write-to-string (integer->char 6)) "#\\ack"))
  (check (equal? (write-to-string (integer->char 7)) "#\\bel"))
  (check (equal? (write-to-string (integer->char 8)) "#\\bs"))
  (check (equal? (write-to-string (integer->char 9)) "#\\tab"))
  (check (equal? (write-to-string (integer->char 10)) "#\\newline"))
  (check (equal? (write-to-string (integer->char 11)) "#\\vtab"))
  (check (equal? (write-to-string (integer->char 12)) "#\\formfeed"))
  (check (equal? (write-to-string (integer->char 13)) "#\\cr"))
  (check (equal? (write-to-string (integer->char 14)) "#\\so"))
  (check (equal? (write-to-string (integer->char 15)) "#\\si"))
  (check (equal? (write-to-string (integer->char 16)) "#\\dle"))
  (check (equal? (write-to-string (integer->char 17)) "#\\dc1"))
  (check (equal? (write-to-string (integer->char 18)) "#\\dc2"))
  (check (equal? (write-to-string (integer->char 19)) "#\\dc3"))
  (check (equal? (write-to-string (integer->char 20)) "#\\dc4"))
  (check (equal? (write-to-string (integer->char 21)) "#\\nak"))
  (check (equal? (write-to-string (integer->char 22)) "#\\syn"))
  (check (equal? (write-to-string (integer->char 23)) "#\\etb"))
  (check (equal? (write-to-string (integer->char 24)) "#\\can"))
  (check (equal? (write-to-string (integer->char 25)) "#\\em"))
  (check (equal? (write-to-string (integer->char 26)) "#\\sub"))
  (check (equal? (write-to-string (integer->char 27)) "#\\esc"))
  (check (equal? (write-to-string (integer->char 28)) "#\\fs"))
  (check (equal? (write-to-string (integer->char 29)) "#\\gs"))
  (check (equal? (write-to-string (integer->char 30)) "#\\rs"))
  (check (equal? (write-to-string (integer->char 31)) "#\\us"))
  (check (equal? (write-to-string (integer->char 32)) "#\\space"))

  (check (equal? "#\\;" (write-to-string #\;)))
  (check (equal? "#\\@" (write-to-string #\@)))
  (check (equal? "#\\#" (write-to-string #\#)))
  (check (equal? "#\\<" (write-to-string #\<)))
  (check (equal? "#\\A" (write-to-string #\A))) 
  (let ((number-of-characters 256))
    (dotimes (n number-of-characters) ; REVISIT: test-case-ec would be nice (defined in an srfi around 68)
      (check (can-read/write-round-trip? (integer->char n))))))


(define-test read-boolean
  (check (equal? #t (read-from-string "#t")))
  (check (equal? #f (read-from-string "#f"))))

(define-test read-fixnum
  (check (equal? 0 (read-from-string "0")))
  (check (equal? 0 (read-from-string "-0")))
  (check (equal? 12 (read-from-string "12")))
  (check (equal? -12 (read-from-string "-12")))  

  (check (equal? 0 (read-from-string "#d0")))
  (check (equal? 0 (read-from-string "#d-0")))
  (check (equal? 12 (read-from-string "#d12")))
  (check (equal? -12 (read-from-string "#d-12")))
  (check (read-error? (read-from-string "#d12f")))
  (check (read-error? (read-from-string "#d-12f")))
  (check (read-error? (read-from-string "#d-12g")))
  (check (read-error? (read-from-string "#d12-2")))
  (check (read-error? (read-from-string "#d12+2")))
  (check (read-error? (read-from-string "#d12i")))
  
  (check (equal? 0 (read-from-string "#e0")))
  (check (equal? 0 (read-from-string "#e-0")))
  (check (equal? 12 (read-from-string "#e12")))
  (check (equal? -12 (read-from-string "#e-12")))
  (check (read-error? (read-from-string "#e12f")))
  (check (read-error? (read-from-string "#e-12f")))
  (check (read-error? (read-from-string "#e-12g")))
  (check (read-error? (read-from-string "#e12-2")))
  (check (read-error? (read-from-string "#e12+2")))
  (check (read-error? (read-from-string "#e12i")))

  (check (equal? 0 (read-from-string "#x0")))
  (check (equal? 0 (read-from-string "#x-0")))
  (check (equal? 18 (read-from-string "#x12")))
  (check (equal? -18 (read-from-string "#x-12")))
  (check (equal? 51966 (read-from-string "#xcafe")))
  (check (equal? -51966 (read-from-string "#x-cafe")))
  (check (equal? 51966 (read-from-string "#xCAFE")))
  (check (equal? -51966 (read-from-string "#x-CAFE")))

  (check (equal? 0 (read-from-string "#o0")))
  (check (equal? 0 (read-from-string "#o-0")))
  (check (equal? 10 (read-from-string "#o12")))
  (check (equal? -10 (read-from-string "#o-12")))
  (check (read-error? (read-from-string "#o8")))
  (check (read-error? (read-from-string "#o9")))
  (check (read-error? (read-from-string "#o18")))
  (check (read-error? (read-from-string "#o19")))
  (check (read-error? (read-from-string "#o12f")))
  (check (read-error? (read-from-string "#o-12f")))
  (check (read-error? (read-from-string "#o-12g")))
  (check (read-error? (read-from-string "#o12-2")))
  (check (read-error? (read-from-string "#o12+2")))
  (check (read-error? (read-from-string "#o12i")))

  (check (equal? 0 (read-from-string "#b0")))
  (check (equal? 0 (read-from-string "#b-0")))
  (check (equal? 12 (read-from-string "#b1100")))
  (check (equal? -12 (read-from-string "#b-1100")))

  (check (read-error? (read-from-string "#b8")))
  (check (read-error? (read-from-string "#b9")))
  (check (read-error? (read-from-string "#b18")))
  (check (read-error? (read-from-string "#b19")))
  (check (read-error? (read-from-string "#b12f")))
  (check (read-error? (read-from-string "#b-12f")))
  (check (read-error? (read-from-string "#b-12g")))
  (check (read-error? (read-from-string "#b12-2")))
  (check (read-error? (read-from-string "#b12+2")))
  (check (read-error? (read-from-string "#b12i"))))

(define-test read-flonum
  (check (equal? 0.0 (read-from-string "0.0")))
  (check (equal? #inan (read-from-string "#inan")))
  (check (equal? #iposinf (read-from-string "#iposinf")))
  (check (equal? #ineginf (read-from-string "#ineginf")))
  (check (read-error? (read-from-string "#ifoo"))))

(define-test read-complex

  (define (split-complex-number number)
    (if (number? number)
        (cons (real-part number)
              (imag-part number))
        #f))

  (check (equal? '( 2    .  0   ) (split-complex-number (read-from-string "2"))))
  (check (equal? '( 2.0  .  0.0 ) (split-complex-number (read-from-string "2.0"))))
  (check (equal? '( 2.0  .  0.0 ) (split-complex-number (read-from-string "+2.0"))))
  (check (equal? '(-2.0  .  0.0 ) (split-complex-number (read-from-string "-2.0"))))
  (check (equal? '( 0.0  .  2.0 ) (split-complex-number (read-from-string "+2.0i"))))
  (check (equal? '( 0.0  . -2.0 ) (split-complex-number (read-from-string "-2.0i"))))
  (check (equal? '( 3.0  .  2.0 ) (split-complex-number (read-from-string "3.0+2.0i"))))
  (check (equal? '( 3.0  . -2.0 ) (split-complex-number (read-from-string "3.0-2.0i"))))
  (check (equal? '( 3.0  .  2.0 ) (split-complex-number (read-from-string "+3.0+2.0i"))))
  (check (equal? '( 3.0  . -2.0 ) (split-complex-number (read-from-string "+3.0-2.0i"))))
  (check (equal? '(-3.0  .  2.0 ) (split-complex-number (read-from-string "-3.0+2.0i"))))
  (check (equal? '(-3.0  . -2.0 ) (split-complex-number (read-from-string "-3.0-2.0i"))))

  (check (symbol? (read-from-string "3.0+2.0")))
  (check (symbol? (read-from-string "3.0-2.0")))
  (check (symbol? (read-from-string "3.0+2.0j")))
  (check (symbol? (read-from-string "3.0-2.0j")))
  (check (symbol? (read-from-string "3.0--2.0i")))
  (check (symbol? (read-from-string "3.0+-2.0i")))
  (check (symbol? (read-from-string "3.0-+2.0i")))
  (check (symbol? (read-from-string "+3.0-2.0")))
  (check (symbol? (read-from-string "+3.0-2.0")))
  (check (symbol? (read-from-string "-3.0-2.0")))
  (check (symbol? (read-from-string "-3.0-2.0")))

  ;; REVISIT: The new reader should fix this.
  (check (symbol? (read-from-string "-3.0-2.0-2.0i"))))

(define-test write-flonum/complex
  (equal? "#inan" (write-to-string (/ 0 0)))
  (equal? "#iposinf" (write-to-string (/ 1 0)))
  (equal? "#ineginf" (write-to-string (/ -1 0)))

  (dynamic-let ((*flonum-print-precision* -1))
               (check (runtime-error? (write-to-string 3.14))))

  (dynamic-let ((*flonum-print-precision* 17))
               (check (runtime-error? (write-to-string 3.14))))

  (dynamic-let ((*flonum-print-precision* #f))
               (check (runtime-error? (write-to-string 3.14))))

  (dynamic-let ((*flonum-print-precision* 2))
               (check (equal? "3.14" (write-to-string 3.14)))
               (check (equal? "0.00+2.00i" (write-to-string 2i)))
               (check (equal? "0.00-2.00i" (write-to-string -2i)))

               (check (equal? "3.00+2.00i" (write-to-string 3+2i)))
               (check (equal? "3.00-2.00i" (write-to-string 3-2i)))))




(define-test read-quasiquote-syntax
  (check (eq? 'quasiquote       (car (read-from-string "`a"))))
  (check (eq? 'unquote          (car (read-from-string ",a"))))
  (check (eq? 'unquote-splicing (car (read-from-string ",@a"))))
  (check (eq? 'quote            (car (read-from-string "'a"))))

  (with-package "test-reader"
                (check
                 (equal? '(quasiquote (quote ((unquote a) (unquote-splicing b))))
                         (read-from-string "`'( ,a ,@b )")))))

(define read-time-evaluation-test-var-1 #f)
(define readsharp-eval-test-fn #f)

(define-test read-time-evaluation
  (check (equal? 0.0 (read-from-string "#.0.0")))
  (check (equal? 4 (read-from-string "#.(+ 2 2)")))

  (with-package "test-reader"
                (check (equal? 12 (read-from-string "#.(set! read-time-evaluation-test-var-1 12)")))

                (check (equal? 12 read-time-evaluation-test-var-1))

                (dynamic-let ((readsharp-eval-test-fn (lambda (x)
                                                        (cons x x))))
                  (check (equal? '(12 . 12)
                                 (read-from-string "#.(readsharp-eval-test-fn 12)"))))))


(define-test read-list
  (check (read-error? (read-from-string ")")))
  (check (read-error? (read-from-string "(1 . )")))
  (check (read-error? (read-from-string "( . )")))
  (check (read-error? (read-from-string "(1 . 2 3)")))
  (check (read-error? (read-from-string "(1 2 3")))

  (let ((xs (read-from-string "()")))
    (check (null? xs)))

  (let ((xs (read-from-string "(:a)")))
    (check (list? xs))
    (check (= 1 (length xs)))
    (check (eq? :a (car xs))))

  (let ((xs (read-from-string "(:a . :b)")))
    (check (pair? xs))
    (check (not (list? xs)))
    (check (eq? :a (car xs)))
    (check (eq? :b (cdr xs))))

  (let ((xs (read-from-string "(:a :b :c)")))
    (check (list? xs))
    (check (= 3 (length xs)))
    (check (eq? :a (car xs)))
    (check (eq? :b (cadr xs)))
    (check (eq? :c (caddr xs))))

  (let ((xs (read-from-string "(:a :b . :c)")))
    (check (pair? xs))
    (check (not (list? xs)))
    (check (eq? :a (car xs)))
    (check (eq? :b (cadr xs)))
    (check (eq? :c (cddr xs)))))

(define-test read-string

  (define (reader-string-contains? string-to-read expected-chars)
    "Verifies that <string-to-read> represents the string representation
     of a string that contains the <expected-chars>."
    (let ((str (read-from-string string-to-read)))
      (and (string? str)
           (equal? (string->list str) expected-chars))))

  (check (reader-string-contains? "\"\"" '()))
  (check (reader-string-contains? "\"f\"" '(#\f)))
  (check (reader-string-contains? "\"foo\"" '(#\f #\o #\o)))

  (check (reader-string-contains? "\"\\nfoo\"" '(#\newline #\f #\o #\o)))
  (check (reader-string-contains? "\"foo\\n\"" '(#\f #\o #\o #\newline)))

  (check (reader-string-contains? "\"\\n\"" '(#\newline)))
  (check (reader-string-contains? "\"\\t\"" '(#\tab)))
  (check (reader-string-contains? "\"\\r\"" '(#\cr)))
  (check (reader-string-contains? "\"\\d\"" '(#\eot)))
  (check (reader-string-contains? "\"\\s\"" '(#\space)))
  (check (reader-string-contains? "\"\\\"\"" '(#\")))

  (check (reader-string-contains? "\" \"" '(#\space)))
  (check (reader-string-contains? "\"  \"" '(#\space #\space))))

(define-test read-annotated
  (check (read-error? (read-from-string "#=")))
  (check (read-error? (read-from-string "#@123")))
  (check (eof-object? (read-from-string "#@123=")))
  (check (eof-object? (read-from-string "#@=")))

  (check (equal? 12 (read-from-string "#@beef=12")))
  (check (equal? :keyword (read-from-string "#@beef=:keyword")))
  (with-package "test-reader"
                (check (equal? 'sym (read-from-string "#@beef=sym")))))
   

(define-test read-vector
  (let ((vec (read-from-string "[]")))
    (check (vector? vec))
    (check (= 0 (length vec))))

  (let ((vec (read-from-string "[1 2 3]")))
    (check (vector? vec))
    (check (= 3 (length vec)))
    
    (check (= 1 (vector-ref vec 0)))
    (check (= 2 (vector-ref vec 1)))
    (check (= 3 (vector-ref vec 2))))

  (let ((vec (read-from-string "[[1 2 3] [4 5 6] [7 8 9]]")))
    (check (vector? vec))
    (check (= 3 (length vec)))
    
    (check (equal? [1 2 3] (vector-ref vec 0)))
    (check (equal? [4 5 6] (vector-ref vec 1)))
    (check (equal? [7 8 9] (vector-ref vec 2))))

  (check (can-read/write-round-trip? []))
  (check (can-read/write-round-trip? [1 2 3]))
  (check (can-read/write-round-trip? (make-vector 10)))
  (check (can-read/write-round-trip? [[1 2 3] [4 5 6] [7 8 9]])))

(define-test read-sexpr-comment
  (check (= 1 (read-from-string "#;0 1")))
  (check (= 1 (read-from-string "#;() 1")))
  (check (= 1 (read-from-string "#;(2 3 \n 4) 1")))

  (check (= 1 (read-from-string "#;\"2 3 4\" 1")))

  (check (equal? [1 2 8 9] (read-from-string "[1 2 #;(3 \n 4) 8 9]"))))


(when (find-package "stp-no-package")
  (delete-package! "stp-no-package"))

(when (find-package "stp1")
  (delete-package! "stp1"))

(make-package! "stp1")


(when (find-package "stp:2")
  (delete-package! "stp:2"))

(make-package! "stp:2")

(define-test read-symbol

  (begin
    (check (read-error? (read-from-string "stp1:no-private-access")))
    (export! (intern! "no-private-access" (find-package "stp1")) (find-package "stp1"))
    (check (not (runtime-error? (read-from-string "stp1:no-private-access")))))

  (check (read-error? (read-from-string "too:many:segments")))
  (check (read-error? (read-from-string ":also:too:many:segments")))

  (check (not (runtime-error? 
                   (read-from-string (make-string 100 "x")))))

  (check (not (runtime-error?
                   (read-from-string (string-append "stp1::" (make-string 100 "x"))))))

  (check (not (runtime-error?
                   (read-from-string (string-append ":" (make-string 100 "x"))))))


  (let ((s (read-from-string "test-symbol")))
    (check (symbol? s))
    (check (eq? *package* (symbol-package s)))
    (check (equal? "test-symbol" (symbol-name s))))

  (let ((s (read-from-string "\\:test-symbol")))
    (check (symbol? s))
    (check (eq? *package* (symbol-package s)))
    (check (equal? ":test-symbol" (symbol-name s))))

  (let ((s (read-from-string "test-symbol\\:")))
    (check (symbol? s))
    (check (eq? *package* (symbol-package s)))
    (check (equal? "test-symbol:" (symbol-name s))))

  (let ((s (read-from-string "test\\:symbol")))
    (check (symbol? s))
    (check (eq? *package* (symbol-package s)))
    (check (equal? "test:symbol" (symbol-name s))))

  (let ((s (read-from-string ":test-keyword")))
    (check (symbol? s))
    (check (eq? (find-package "keyword") (symbol-package s)))
    (check (equal? "test-keyword" (symbol-name s)))
    (check (runtime-error? (set-symbol-value! s "not possible w/keywords."))))

  (let ((s (read-from-string ":\\:test-keyword")))
    (check (symbol? s))
    (check (eq? (find-package "keyword") (symbol-package s)))
    (check (equal? ":test-keyword" (symbol-name s)))
    (check (runtime-error? (set-symbol-value! s) "not possible w/keywords.")))

  (let ((s (read-from-string ":test\\:keyword")))
    (check (symbol? s))
    (check (eq? (find-package "keyword") (symbol-package s)))
    (check (equal? "test:keyword" (symbol-name s)))
    (check (runtime-error? (set-symbol-value! s "not possible w/keywords."))))

  (let ((s (read-from-string "::test-keyword")))
    (check (symbol? s))
    (check (eq? (find-package "keyword") (symbol-package s)))
    (check (equal? "test-keyword" (symbol-name s)))
    (check (runtime-error? (set-symbol-value! s "not possible w/keywords."))))

  (let ((s (read-from-string "test-keyword:")))
    (check (symbol? s))
    (check (eq? (find-package "keyword") (symbol-package s)))
    (check (equal? "test-keyword" (symbol-name s)))
    (check (runtime-error? (set-symbol-value! s "not possible w/keywords."))))

  (let ((s (read-from-string "test-keyword::")))
    (check (symbol? s))
    (check (eq? (find-package "keyword") (symbol-package s)))
    (check (equal? "test-keyword" (symbol-name s)))
    (check (runtime-error? (set-symbol-value! s "not possible w/keywords."))))

  (let ((s (read-from-string "stp1::test-symbol")))
    (check (symbol? s))
    (check (eq? (find-package "stp1") (symbol-package s)))
    (check (equal? "test-symbol" (symbol-name s))))

  (let ((s (read-from-string "stp\\:2::test-symbol")))
    (check (symbol? s))
    (check (eq? (find-package "stp:2") (symbol-package s)))
    (check (equal? "test-symbol" (symbol-name s))))

  (check (equal? (symbol-name (read-from-string "test\\ symbol")) "test symbol"))

  (check (equal? (symbol-name (read-from-string ":test\\ symbol")) "test symbol"))

  (check (equal? (symbol-name (read-from-string "stp1::test\\ symbol")) "test symbol"))
  (check (equal? (symbol-name (read-from-string "stp1::test\\:symbol")) "test:symbol"))
  (check (equal? (symbol-name (read-from-string "stp1::test\\:\\:symbol")) "test::symbol"))
  (check (equal? (symbol-name (read-from-string "stp1::\\:test\\:symbol")) ":test:symbol"))
  (check (equal? (symbol-name (read-from-string "stp1::\\:\\:test\\:\\:symbol")) "::test::symbol"))

  (check (equal? (symbol-name (read-from-string "stp\\:2::test\\ symbol")) "test symbol"))
  (check (equal? (symbol-name (read-from-string "stp\\:2::test\\:symbol")) "test:symbol"))
  (check (equal? (symbol-name (read-from-string "stp\\:2::test\\:\\:symbol")) "test::symbol"))
  (check (equal? (symbol-name (read-from-string "stp\\:2::\\:test\\:symbol")) ":test:symbol"))
  (check (equal? (symbol-name (read-from-string "stp\\:2::\\:\\:test\\:\\:symbol")) "::test::symbol")))

(define-test read/location-mapping
  (dynamic-let ((*location-mapping* (make-identity-hash)))
    (let ((form (read-from-string "(:a :b \n :c 4)" *location-mapping*)))
    
      (check (equal? '(1 . 1) (cdr (hash-ref *location-mapping* :a))))
      (check (equal? '(1 . 4) (cdr (hash-ref *location-mapping* :b))))
      (check (equal? '(2 . 1) (cdr (hash-ref *location-mapping* :c))))

      ;; immediates are not hashed
      (check (not (hash-ref *location-mapping* 4 #f)))
      
      (check (equal? '(1 . 0) (cdr (hash-ref *location-mapping* form))))
      (check (equal? '(1 . 4) (cdr (hash-ref *location-mapping* (cdr form)))))
      (check (equal? '(2 . 1) (cdr (hash-ref *location-mapping* (cddr form)))))
    
      (let ((ports (set-union/eq (map #L(car (hash-ref *location-mapping* _)) (hash-keys *location-mapping*)))))
        (check (= 1 (length ports)))
        (check (input-port? (car ports)))))))
