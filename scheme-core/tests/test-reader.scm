(use-package! "unit-test")


(define-test read
  (test-case (runtime-error? (read 12)))
  (test-case (runtime-error? (read (current-output-port) #f))))


(define-test read/write-character
  (test-case (char=? (integer->char 97) (read-from-string "#\\a")))
  (test-case (char=? (integer->char 98) (read-from-string "#\\b")))
  (test-case (char=? (integer->char 90) (read-from-string "#\\Z")))
  (test-case (char=? (integer->char 59) (read-from-string "#\\;")))
  (test-case (char=? (integer->char 64) (read-from-string "#\\@")))
  (test-case (char=? (integer->char 35) (read-from-string "#\\#")))
  (test-case (char=? (integer->char 60) (read-from-string "#\\<")))
  (test-case (char=? (integer->char 65) (read-from-string "#\\<65>")))
  (test-case (char=? (integer->char 60) (read-from-string "#\\< ")))
  (test-case (char=? (integer->char 65) (read-from-string "#\\<65> ")))
  (test-case (char=? (integer->char 0) (read-from-string "#\\nul")))
  (test-case (char=? (integer->char 1) (read-from-string "#\\soh")))
  (test-case (char=? (integer->char 2) (read-from-string "#\\stx")))
  (test-case (char=? (integer->char 3) (read-from-string "#\\etx")))
  (test-case (char=? (integer->char 4) (read-from-string "#\\eot")))
  (test-case (char=? (integer->char 5) (read-from-string "#\\eng")))
  (test-case (char=? (integer->char 6) (read-from-string "#\\ack")))
  (test-case (char=? (integer->char 7) (read-from-string "#\\bel")))
  (test-case (char=? (integer->char 8) (read-from-string "#\\bs")))
  (test-case (char=? (integer->char 9) (read-from-string "#\\tab")))
  (test-case (char=? (integer->char 10) (read-from-string "#\\newline")))
  (test-case (char=? (integer->char 11) (read-from-string "#\\vtab")))
  (test-case (char=? (integer->char 12) (read-from-string "#\\formfeed")))
  (test-case (char=? (integer->char 13) (read-from-string "#\\cr")))
  (test-case (char=? (integer->char 14) (read-from-string "#\\so")))
  (test-case (char=? (integer->char 15) (read-from-string "#\\si")))
  (test-case (char=? (integer->char 16) (read-from-string "#\\dle")))
  (test-case (char=? (integer->char 17) (read-from-string "#\\dc1")))
  (test-case (char=? (integer->char 18) (read-from-string "#\\dc2")))
  (test-case (char=? (integer->char 19) (read-from-string "#\\dc3")))
  (test-case (char=? (integer->char 20) (read-from-string "#\\dc4")))
  (test-case (char=? (integer->char 21) (read-from-string "#\\nak")))
  (test-case (char=? (integer->char 22) (read-from-string "#\\syn")))
  (test-case (char=? (integer->char 23) (read-from-string "#\\etb")))
  (test-case (char=? (integer->char 24) (read-from-string "#\\can")))
  (test-case (char=? (integer->char 25) (read-from-string "#\\em")))
  (test-case (char=? (integer->char 26) (read-from-string "#\\sub")))
  (test-case (char=? (integer->char 27) (read-from-string "#\\esc")))
  (test-case (char=? (integer->char 28) (read-from-string "#\\fs")))
  (test-case (char=? (integer->char 29) (read-from-string "#\\gs")))
  (test-case (char=? (integer->char 30) (read-from-string "#\\rs")))
  (test-case (char=? (integer->char 31) (read-from-string "#\\us")))
  (test-case (char=? (integer->char 32) (read-from-string "#\\space")))

  (test-case (equal? "#\\a" (write-to-string #\a)))
  (test-case (equal? "#\\b" (write-to-string #\b)))
  (test-case (equal? "#\\Z" (write-to-string #\Z)))

  (test-case (equal? "#\\newline" (write-to-string #\newline)))
  (test-case (equal? "#\\space" (write-to-string #\space)))

  (test-case (equal? (write-to-string (integer->char 0)) "#\\nul"))
  (test-case (equal? (write-to-string (integer->char 1)) "#\\soh"))
  (test-case (equal? (write-to-string (integer->char 2)) "#\\stx"))
  (test-case (equal? (write-to-string (integer->char 3)) "#\\etx"))
  (test-case (equal? (write-to-string (integer->char 4)) "#\\eot"))
  (test-case (equal? (write-to-string (integer->char 5)) "#\\eng"))
  (test-case (equal? (write-to-string (integer->char 6)) "#\\ack"))
  (test-case (equal? (write-to-string (integer->char 7)) "#\\bel"))
  (test-case (equal? (write-to-string (integer->char 8)) "#\\bs"))
  (test-case (equal? (write-to-string (integer->char 9)) "#\\tab"))
  (test-case (equal? (write-to-string (integer->char 10)) "#\\newline"))
  (test-case (equal? (write-to-string (integer->char 11)) "#\\vtab"))
  (test-case (equal? (write-to-string (integer->char 12)) "#\\formfeed"))
  (test-case (equal? (write-to-string (integer->char 13)) "#\\cr"))
  (test-case (equal? (write-to-string (integer->char 14)) "#\\so"))
  (test-case (equal? (write-to-string (integer->char 15)) "#\\si"))
  (test-case (equal? (write-to-string (integer->char 16)) "#\\dle"))
  (test-case (equal? (write-to-string (integer->char 17)) "#\\dc1"))
  (test-case (equal? (write-to-string (integer->char 18)) "#\\dc2"))
  (test-case (equal? (write-to-string (integer->char 19)) "#\\dc3"))
  (test-case (equal? (write-to-string (integer->char 20)) "#\\dc4"))
  (test-case (equal? (write-to-string (integer->char 21)) "#\\nak"))
  (test-case (equal? (write-to-string (integer->char 22)) "#\\syn"))
  (test-case (equal? (write-to-string (integer->char 23)) "#\\etb"))
  (test-case (equal? (write-to-string (integer->char 24)) "#\\can"))
  (test-case (equal? (write-to-string (integer->char 25)) "#\\em"))
  (test-case (equal? (write-to-string (integer->char 26)) "#\\sub"))
  (test-case (equal? (write-to-string (integer->char 27)) "#\\esc"))
  (test-case (equal? (write-to-string (integer->char 28)) "#\\fs"))
  (test-case (equal? (write-to-string (integer->char 29)) "#\\gs"))
  (test-case (equal? (write-to-string (integer->char 30)) "#\\rs"))
  (test-case (equal? (write-to-string (integer->char 31)) "#\\us"))
  (test-case (equal? (write-to-string (integer->char 32)) "#\\space"))

  (test-case (equal? "#\\;" (write-to-string #\;)))
  (test-case (equal? "#\\@" (write-to-string #\@)))
  (test-case (equal? "#\\#" (write-to-string #\#)))
  (test-case (equal? "#\\<" (write-to-string #\<)))
  (test-case (equal? "#\\A" (write-to-string #\A))) 
  (let ((number-of-characters 256))
    (dotimes (n number-of-characters) ; REVISIT: test-case-ec would be nice (defined in an srfi around 68)
      (test-case (can-read/write-round-trip? (integer->char n))))))


(define-test read-boolean
  (test-case (equal? #t (read-from-string "#t")))
  (test-case (equal? #f (read-from-string "#f"))))

(define-test read-fixnum
  (test-case (equal? 0 (read-from-string "0")))
  (test-case (equal? 0 (read-from-string "-0")))
  (test-case (equal? 12 (read-from-string "12")))
  (test-case (equal? -12 (read-from-string "-12")))  

  (test-case (equal? 0 (read-from-string "#d0")))
  (test-case (equal? 0 (read-from-string "#d-0")))
  (test-case (equal? 12 (read-from-string "#d12")))
  (test-case (equal? -12 (read-from-string "#d-12")))
  (test-case (read-error? (read-from-string "#d12f")))
  (test-case (read-error? (read-from-string "#d-12f")))
  (test-case (read-error? (read-from-string "#d-12g")))
  (test-case (read-error? (read-from-string "#d12-2")))
  (test-case (read-error? (read-from-string "#d12+2")))
  (test-case (read-error? (read-from-string "#d12i")))
  
  (test-case (equal? 0 (read-from-string "#e0")))
  (test-case (equal? 0 (read-from-string "#e-0")))
  (test-case (equal? 12 (read-from-string "#e12")))
  (test-case (equal? -12 (read-from-string "#e-12")))
  (test-case (read-error? (read-from-string "#e12f")))
  (test-case (read-error? (read-from-string "#e-12f")))
  (test-case (read-error? (read-from-string "#e-12g")))
  (test-case (read-error? (read-from-string "#e12-2")))
  (test-case (read-error? (read-from-string "#e12+2")))
  (test-case (read-error? (read-from-string "#e12i")))

  (test-case (equal? 0 (read-from-string "#x0")))
  (test-case (equal? 0 (read-from-string "#x-0")))
  (test-case (equal? 18 (read-from-string "#x12")))
  (test-case (equal? -18 (read-from-string "#x-12")))
  (test-case (equal? 51966 (read-from-string "#xcafe")))
  (test-case (equal? -51966 (read-from-string "#x-cafe")))
  (test-case (equal? 51966 (read-from-string "#xCAFE")))
  (test-case (equal? -51966 (read-from-string "#x-CAFE")))

  (test-case (equal? 0 (read-from-string "#o0")))
  (test-case (equal? 0 (read-from-string "#o-0")))
  (test-case (equal? 10 (read-from-string "#o12")))
  (test-case (equal? -10 (read-from-string "#o-12")))
  (test-case (read-error? (read-from-string "#o8")))
  (test-case (read-error? (read-from-string "#o9")))
  (test-case (read-error? (read-from-string "#o18")))
  (test-case (read-error? (read-from-string "#o19")))
  (test-case (read-error? (read-from-string "#o12f")))
  (test-case (read-error? (read-from-string "#o-12f")))
  (test-case (read-error? (read-from-string "#o-12g")))
  (test-case (read-error? (read-from-string "#o12-2")))
  (test-case (read-error? (read-from-string "#o12+2")))
  (test-case (read-error? (read-from-string "#o12i")))

  (test-case (equal? 0 (read-from-string "#b0")))
  (test-case (equal? 0 (read-from-string "#b-0")))
  (test-case (equal? 12 (read-from-string "#b1100")))
  (test-case (equal? -12 (read-from-string "#b-1100")))

  (test-case (read-error? (read-from-string "#b8")))
  (test-case (read-error? (read-from-string "#b9")))
  (test-case (read-error? (read-from-string "#b18")))
  (test-case (read-error? (read-from-string "#b19")))
  (test-case (read-error? (read-from-string "#b12f")))
  (test-case (read-error? (read-from-string "#b-12f")))
  (test-case (read-error? (read-from-string "#b-12g")))
  (test-case (read-error? (read-from-string "#b12-2")))
  (test-case (read-error? (read-from-string "#b12+2")))
  (test-case (read-error? (read-from-string "#b12i")))
  )


(define-test read-flonum
  (test-case (equal? 0.0 (read-from-string "0.0")))
  (test-case (equal? #inan (read-from-string "#inan")))
  (test-case (equal? #iposinf (read-from-string "#iposinf")))
  (test-case (equal? #ineginf (read-from-string "#ineginf")))
  (test-case (read-error? (read-from-string "#ifoo"))))

(define-test read-complex

  (define (split-complex-number number)
    (if (number? number)
        (cons (real-part number)
              (imag-part number))
        #f))

  (test-case (equal? '( 2    .  0   ) (split-complex-number (read-from-string "2"))))
  (test-case (equal? '( 2.0  .  0.0 ) (split-complex-number (read-from-string "2.0"))))
  (test-case (equal? '( 2.0  .  0.0 ) (split-complex-number (read-from-string "+2.0"))))
  (test-case (equal? '(-2.0  .  0.0 ) (split-complex-number (read-from-string "-2.0"))))
  (test-case (equal? '( 0.0  .  2.0 ) (split-complex-number (read-from-string "+2.0i"))))
  (test-case (equal? '( 0.0  . -2.0 ) (split-complex-number (read-from-string "-2.0i"))))
  (test-case (equal? '( 3.0  .  2.0 ) (split-complex-number (read-from-string "3.0+2.0i"))))
  (test-case (equal? '( 3.0  . -2.0 ) (split-complex-number (read-from-string "3.0-2.0i"))))
  (test-case (equal? '( 3.0  .  2.0 ) (split-complex-number (read-from-string "+3.0+2.0i"))))
  (test-case (equal? '( 3.0  . -2.0 ) (split-complex-number (read-from-string "+3.0-2.0i"))))
  (test-case (equal? '(-3.0  .  2.0 ) (split-complex-number (read-from-string "-3.0+2.0i"))))
  (test-case (equal? '(-3.0  . -2.0 ) (split-complex-number (read-from-string "-3.0-2.0i"))))

  (test-case (symbol? (read-from-string "3.0+2.0")))
  (test-case (symbol? (read-from-string "3.0-2.0")))
  (test-case (symbol? (read-from-string "3.0+2.0j")))
  (test-case (symbol? (read-from-string "3.0-2.0j")))
  (test-case (symbol? (read-from-string "3.0--2.0i")))
  (test-case (symbol? (read-from-string "3.0+-2.0i")))
  (test-case (symbol? (read-from-string "3.0-+2.0i")))
  (test-case (symbol? (read-from-string "+3.0-2.0")))
  (test-case (symbol? (read-from-string "+3.0-2.0")))
  (test-case (symbol? (read-from-string "-3.0-2.0")))
  (test-case (symbol? (read-from-string "-3.0-2.0")))
 
  (test-case (symbol? (read-from-string "-3.0-2.0-2.0i"))) ; REVISIT: The new reader should fix this.
  )

(define-test write-flonum/complex
  (equal? "#inan" (write-to-string (/ 0 0)))
  (equal? "#iposinf" (write-to-string (/ 1 0)))
  (equal? "#ineginf" (write-to-string (/ -1 0)))

  (dynamic-let ((*flonum-print-precision* -1))
               (test-case (runtime-error? (write-to-string 3.14))))

  (dynamic-let ((*flonum-print-precision* 17))
               (test-case (runtime-error? (write-to-string 3.14))))

  (dynamic-let ((*flonum-print-precision* #f))
               (test-case (runtime-error? (write-to-string 3.14))))

  (dynamic-let ((*flonum-print-precision* 2))
               (test-case (equal? "3.14" (write-to-string 3.14)))
               (test-case (equal? "0.00+2.00i" (write-to-string 2i)))
               (test-case (equal? "0.00-2.00i" (write-to-string -2i)))

               (test-case (equal? "3.00+2.00i" (write-to-string 3+2i)))
               (test-case (equal? "3.00-2.00i" (write-to-string 3-2i)))))



(define base-point
  (let ((o (make-instance)))
    (slot-set! o 'class-name 'base-point)
    (slot-set! o 't 'nothing-to-see-here)
    o))

(define-test read-instance
  (let ((ob #f))
    
    (test-case (read-error? (read-from-string "#I12")))
    (test-case (read-error? (read-from-string "#I(12)")))
    (test-case (read-error? (read-from-string "#I(12 x 12)")))
    (test-case (read-error? (read-from-string "#I(#f (x . 12) (y . 16)")))
    (test-case (runtime-error? (read-from-string "#I(#I(#f) (x . 12) (y . 16))")))
    (test-case (runtime-error? (read-from-string "#I(#f 12 12)")))

    (test-case (not (non-local-escape?
                     (set! ob (read-from-string "#I(#f)")))))

    (test-case (not (has-slot? ob 'class-name)))
    (test-case (not (has-slot? ob 'x)))
    (test-case (not (has-slot? ob 'y)))
    (test-case (not (has-slot? ob 'r)))
    (test-case (not (has-slot? ob 't)))

    (test-case (not (non-local-escape? 
                     (set! ob (read-from-string "#I(#f x 12 y 13 r 20)")))))

    (test-case (not (has-slot? ob 'class-name)))
    (test-case (eq? 12    (slot-ref ob 'x)))
    (test-case (eq? 13    (slot-ref ob 'y)))
    (test-case (eq? 20    (slot-ref ob 'r)))
    (test-case (not (has-slot? ob 't)))
    ))

(define-test read-quasiquote-syntax
  (test-case (eq? 'quasiquote       (car (read-from-string "`a"))))
  (test-case (eq? 'unquote          (car (read-from-string ",a"))))
  (test-case (eq? 'unquote-splicing (car (read-from-string ",@a"))))

  (test-case (eq? 'quote            (car (read-from-string "'a"))))

  (test-case
   (equal? '(quasiquote (quote ((unquote a) (unquote-splicing b))))
           (read-from-string "`'( ,a ,@b )"))))

(define read-time-evaluation-test-var-1 #f)
(define readsharp-eval-test-fn #f)

(define-test read-time-evaluation
  (test-case (equal? 0.0 (read-from-string "#.0.0")))
  (test-case (equal? 4 (read-from-string "#.(+ 2 2)")))

  (test-case (equal? 12 (read-from-string "#.(set! read-time-evaluation-test-var-1 12)")))

  (test-case (equal? 12 read-time-evaluation-test-var-1))

  (dynamic-let ((readsharp-eval-test-fn (lambda (x)
                                          (cons x x))))
               (test-case (equal? '(12 . 12)
                                  (read-from-string "#.(readsharp-eval-test-fn 12)")))))


(define-test read-list
  (test-case (read-error? (read-from-string ")")))
  (test-case (read-error? (read-from-string "(1 . )")))
  (test-case (read-error? (read-from-string "( . )")))
  (test-case (read-error? (read-from-string "(1 . 2 3)")))
  (test-case (read-error? (read-from-string "(1 2 3")))

  (let ((xs (read-from-string "()")))
    (test-case (null? xs)))

  (let ((xs (read-from-string "(a)")))
    (test-case (list? xs))
    (test-case (= 1 (length xs)))
    (test-case (eq? 'a (car xs))))

  (let ((xs (read-from-string "(a . b)")))
    (test-case (pair? xs))
    (test-case (not (list? xs)))
    (test-case (eq? 'a (car xs)))
    (test-case (eq? 'b (cdr xs))))

  (let ((xs (read-from-string "(a b c)")))
    (test-case (list? xs))
    (test-case (= 3 (length xs)))
    (test-case (eq? 'a (car xs)))
    (test-case (eq? 'b (cadr xs)))
    (test-case (eq? 'c (caddr xs))))

  (let ((xs (read-from-string "(a b . c)")))
    (test-case (pair? xs))
    (test-case (not (list? xs)))
    (test-case (eq? 'a (car xs)))
    (test-case (eq? 'b (cadr xs)))
    (test-case (eq? 'c (cddr xs)))))

(define-test read-string

  (define (reader-string-contains? string-to-read expected-chars)
    "Verifies that <string-to-read> represents the string representation
     of a string that contains the <expected-chars>."
    (let ((str (read-from-string string-to-read)))
      (and (string? str)
           (equal? (string->list str) expected-chars))))

  (test-case (reader-string-contains? "\"\"" '()))
  (test-case (reader-string-contains? "\"f\"" '(#\f)))
  (test-case (reader-string-contains? "\"foo\"" '(#\f #\o #\o)))

  (test-case (reader-string-contains? "\"\\nfoo\"" '(#\newline #\f #\o #\o)))
  (test-case (reader-string-contains? "\"foo\\n\"" '(#\f #\o #\o #\newline)))

  (test-case (reader-string-contains? "\"\\n\"" '(#\newline)))
  (test-case (reader-string-contains? "\"\\t\"" '(#\tab)))
  (test-case (reader-string-contains? "\"\\r\"" '(#\cr)))
  (test-case (reader-string-contains? "\"\\d\"" '(#\eot)))
  (test-case (reader-string-contains? "\"\\s\"" '(#\space)))
  (test-case (reader-string-contains? "\"\\\"\"" '(#\")))

  (test-case (reader-string-contains? "\" \"" '(#\space)))
  (test-case (reader-string-contains? "\"  \"" '(#\space #\space))))

(define-test read-annotated
  (test-case (read-error? (read-from-string "#=")))
  (test-case (read-error? (read-from-string "#@123")))
  (test-case (eof-object? (read-from-string "#@123=")))
  (test-case (eof-object? (read-from-string "#@=")))

  (test-case (equal? 12 (read-from-string "#@beef=12")))
  (test-case (equal? :keyword (read-from-string "#@beef=:keyword")))
  (test-case (equal? 'sym (read-from-string "#@beef=sym"))))
   

(define-test read-vector
  (let ((vec (read-from-string "#()")))
    (test-case (vector? vec))
    (test-case (= 0 (length vec))))

  (let ((vec (read-from-string "#(1 2 3)")))
    (test-case (vector? vec))
    (test-case (= 3 (length vec)))
    
    (test-case (= 1 (vector-ref vec 0)))
    (test-case (= 2 (vector-ref vec 1)))
    (test-case (= 3 (vector-ref vec 2))))

  (let ((vec (read-from-string "#(#(1 2 3) #(4 5 6) #(7 8 9))")))
    (test-case (vector? vec))
    (test-case (= 3 (length vec)))
    
    (test-case (equal? #(1 2 3) (vector-ref vec 0)))
    (test-case (equal? #(4 5 6) (vector-ref vec 1)))
    (test-case (equal? #(7 8 9) (vector-ref vec 2))))

  (test-case (can-read/write-round-trip? #()))
  (test-case (can-read/write-round-trip? #(1 2 3)))
  (test-case (can-read/write-round-trip? (make-vector 10)))
  (test-case (can-read/write-round-trip? #(#(1 2 3) #(4 5 6) #(7 8 9)))))

(define-test read-sexpr-comment
  (test-case (= 1 (read-from-string "#;0 1")))
  (test-case (= 1 (read-from-string "#;() 1")))
  (test-case (= 1 (read-from-string "#;(2 3 \n 4) 1")))

  (test-case (= 1 (read-from-string "#;\"2 3 4\" 1")))

  (test-case (equal? #(1 2 8 9) (read-from-string "#(1 2 #;(3 \n 4) 8 9)"))))


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
    (test-case (read-error? (read-from-string "stp1:no-private-access")))
    (export! (intern! "no-private-access" (find-package "stp1")) (find-package "stp1"))
    (test-case (not (runtime-error? (read-from-string "stp1:no-private-access")))))

  (test-case (read-error? (read-from-string "too:many:segments")))
  (test-case (read-error? (read-from-string ":also:too:many:segments")))

  (test-case (not (runtime-error? 
                   (read-from-string (make-string 100 "x")))))

  (test-case (not (runtime-error?
                   (read-from-string (string-append "stp1::" (make-string 100 "x"))))))

  (test-case (not (runtime-error?
                   (read-from-string (string-append ":" (make-string 100 "x"))))))


  (let ((s (read-from-string "test-symbol")))
    (test-case (symbol? s))
    (test-case (eq? *package* (symbol-package s)))
    (test-case (equal? "test-symbol" (symbol-name s))))

  (let ((s (read-from-string "\\:test-symbol")))
    (test-case (symbol? s))
    (test-case (eq? *package* (symbol-package s)))
    (test-case (equal? ":test-symbol" (symbol-name s))))

  (let ((s (read-from-string "test-symbol\\:")))
    (test-case (symbol? s))
    (test-case (eq? *package* (symbol-package s)))
    (test-case (equal? "test-symbol:" (symbol-name s))))

  (let ((s (read-from-string "test\\:symbol")))
    (test-case (symbol? s))
    (test-case (eq? *package* (symbol-package s)))
    (test-case (equal? "test:symbol" (symbol-name s))))

  (let ((s (read-from-string ":test-keyword")))
    (test-case (symbol? s))
    (test-case (eq? (find-package "keyword") (symbol-package s)))
    (test-case (equal? "test-keyword" (symbol-name s)))
    (test-case (runtime-error? (set-symbol-value! s "not possible w/keywords."))))

  (let ((s (read-from-string ":\\:test-keyword")))
    (test-case (symbol? s))
    (test-case (eq? (find-package "keyword") (symbol-package s)))
    (test-case (equal? ":test-keyword" (symbol-name s)))
    (test-case (runtime-error? (set-symbol-value! s) "not possible w/keywords.")))

  (let ((s (read-from-string ":test\\:keyword")))
    (test-case (symbol? s))
    (test-case (eq? (find-package "keyword") (symbol-package s)))
    (test-case (equal? "test:keyword" (symbol-name s)))
    (test-case (runtime-error? (set-symbol-value! s "not possible w/keywords."))))

  (let ((s (read-from-string "::test-keyword")))
    (test-case (symbol? s))
    (test-case (eq? (find-package "keyword") (symbol-package s)))
    (test-case (equal? "test-keyword" (symbol-name s)))
    (test-case (runtime-error? (set-symbol-value! s "not possible w/keywords."))))

  (let ((s (read-from-string "test-keyword:")))
    (test-case (symbol? s))
    (test-case (eq? (find-package "keyword") (symbol-package s)))
    (test-case (equal? "test-keyword" (symbol-name s)))
    (test-case (runtime-error? (set-symbol-value! s "not possible w/keywords."))))

  (let ((s (read-from-string "test-keyword::")))
    (test-case (symbol? s))
    (test-case (eq? (find-package "keyword") (symbol-package s)))
    (test-case (equal? "test-keyword" (symbol-name s)))
    (test-case (runtime-error? (set-symbol-value! s "not possible w/keywords."))))

  (let ((s (read-from-string "stp1::test-symbol")))
    (test-case (symbol? s))
    (test-case (eq? (find-package "stp1") (symbol-package s)))
    (test-case (equal? "test-symbol" (symbol-name s))))

  (let ((s (read-from-string "stp\\:2::test-symbol")))
    (test-case (symbol? s))
    (test-case (eq? (find-package "stp:2") (symbol-package s)))
    (test-case (equal? "test-symbol" (symbol-name s))))

  (test-case (equal? (symbol-name (read-from-string "test\\ symbol")) "test symbol"))

  (test-case (equal? (symbol-name (read-from-string ":test\\ symbol")) "test symbol"))

  (test-case (equal? (symbol-name (read-from-string "stp1::test\\ symbol")) "test symbol"))
  (test-case (equal? (symbol-name (read-from-string "stp1::test\\:symbol")) "test:symbol"))
  (test-case (equal? (symbol-name (read-from-string "stp1::test\\:\\:symbol")) "test::symbol"))
  (test-case (equal? (symbol-name (read-from-string "stp1::\\:test\\:symbol")) ":test:symbol"))
  (test-case (equal? (symbol-name (read-from-string "stp1::\\:\\:test\\:\\:symbol")) "::test::symbol"))

  (test-case (equal? (symbol-name (read-from-string "stp\\:2::test\\ symbol")) "test symbol"))
  (test-case (equal? (symbol-name (read-from-string "stp\\:2::test\\:symbol")) "test:symbol"))
  (test-case (equal? (symbol-name (read-from-string "stp\\:2::test\\:\\:symbol")) "test::symbol"))
  (test-case (equal? (symbol-name (read-from-string "stp\\:2::\\:test\\:symbol")) ":test:symbol"))
  (test-case (equal? (symbol-name (read-from-string "stp\\:2::\\:\\:test\\:\\:symbol")) "::test::symbol"))
  )

(define-test read/location-mapping
  (dynamic-let ((*location-mapping* (make-hash :eq)))
    (let ((form (read-from-string "(:a :b \n :c 4)" *location-mapping*)))
    
      (test-case (equal? '(1 . 1) (cdr (hash-ref *location-mapping* :a))))
      (test-case (equal? '(1 . 4) (cdr (hash-ref *location-mapping* :b))))
      (test-case (equal? '(2 . 1) (cdr (hash-ref *location-mapping* :c))))

      ;; immediates are not hashed
      (test-case (not (hash-ref *location-mapping* 4 #f)))
      
      (test-case (equal? '(1 . 0) (cdr (hash-ref *location-mapping* form))))
      (test-case (equal? '(1 . 4) (cdr (hash-ref *location-mapping* (cdr form)))))
      (test-case (equal? '(2 . 1) (cdr (hash-ref *location-mapping* (cddr form)))))
    
      (let ((ports (set-union/eq (map #L(car (hash-ref *location-mapping* _)) (hash-keys *location-mapping*)))))
        (test-case (= 1 (length ports)))
        (test-case (input-port? (car ports)))))))

  


