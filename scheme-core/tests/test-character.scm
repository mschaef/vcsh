(define-package "test-character"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test char-alphabetic?/non-character
  (check-for (non-char '(0 0.0 "non-character"))
    (not (char-alphabetic? non-char))))

(define-test char-alphabetic?/non-alphabetic
  (check-for (char '(#\0 #\9 #\space #\newline #\cr #\tab #\\))
    (not (char-alphabetic? char))))

(define-test char-alphabetic?
  (check-for (char '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                     #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                     #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                     #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
    (char-alphabetic? char)))

(define-test char-numeric?/non-character
  (check-for (char '(0 0.0 "non-character"))
    (not (char-numeric? char))))

(define-test char-numeric?/non-numeric
  (check-for (char '(#\a #\z #\A #\Z #\space #\newline #\cr #\tab #\\))
    (not (char-numeric? char))))

(define-test char-numeric?/numeric
  (check-for (char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
    (char-numeric? char)))

(define-test char-whitespace?/non-character
  (check-for (char '(0 0.0 "non-character"))
    (not (char-whitespace? char))))

(define-test char-whitespace?/non-whitespace
  (check-for (char '(#\a #\z #\A #\Z #\\ #\0 #\0))
    (not (char-whitespace? char))))

(define-test char-whitespace?/whitespace
  (check-for (char '(#\space #\newline #\cr #\tab))
    (char-whitespace? char)))

(define-test char-upper-case?/non-character
  (check-for (char '(0 0.0 "non-character"))
    (not (char-upper-case? char))))

(define-test char-upper-case?/non-upper-case
  (check-for (char '(#\space #\newline #\cr #\tab #\\ #\0 #\9))
    (not (char-upper-case? char)))
  
  (check-for (char '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                     #\n #\o #\p #\q #\r #\x #\t #\u #\v #\w #\x #\y #\z))
    (not (char-upper-case? char))))

(define-test char-upper-case?/upper-case
  (check-for (char '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                     #\N #\O #\P #\Q #\R #\X #\T #\U #\V #\W #\X #\Y #\Z))
    (char-upper-case? char)))

(define-test char-lower-case?/non-character
  (check-for (char '(0 0.0 "non-character"))
    (not (char-lower-case? char))))

(define-test char-lower-case?/non-lower-case
  (check-for (char '(#\space #\newline #\cr #\tab #\\ #\0 #\9))
    (not (char-lower-case? char))))


(define-test char-lower-case?/non-lower-case
  (check-for (char '(#\space #\newline #\cr #\tab #\\ #\0 #\9))
    (not (char-lower-case? char)))
  
  (check-for (char '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                     #\N #\O #\P #\Q #\R #\X #\T #\U #\V #\W #\X #\Y #\Z))
    (not (char-lower-case? char))))

(define-test char-lower-case?/lower-case
  (check-for (char '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                     #\n #\o #\p #\q #\r #\x #\t #\u #\v #\w #\x #\y #\z))
    (char-lower-case? char)))

(define-test char-octal?/non-character
  (check-for (char '(0 0.0 "non-character"))
    (not (char-octal? char))))

(define-test char-octal?/non-octal
  (check-for (char '(#\space #\newline #\cr #\tab #\\ #\8 #\9
                     #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D
                     #\E #\F))
    (not (char-octal? char))))

(define-test char-octal?/non-octal
  (check-for (char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
    (char-octal? char)))

(define-test char-hex?/non-character
  (check-for (char '(0 0.0 "non-character"))
    (not (char-hex? char))))

(define-test char-hex?/non-hex
  (check-for (char '(#\space #\newline #\cr #\tab
                     #\g #\G #\z #\Z))
    (not (char-hex? char))))

(define-test char-hex?/hex
  (check-for (char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                     #\a #\b #\c #\d #\e #\f
                     #\A #\B #\C #\D #\E #\F))
    (char-hex? char)))


(define-test char-upcase/non-character
  (check-for (char '(0 0.0 "non-character"))
    (runtime-error? (char-upcase char))))

(define-test char-upcase/non-alphabetic
  (check-for (char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                     #\+ #\- #\space #\newline #\cr #\tab #\\))
    (check-for (eq? char (char-upcase char)))))

(define-test char-upcase/uppercase
  (check-for (char '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
                     #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
                     #\U #\V #\W #\X #\Y #\Z))
    (check-for (eq? char (char-upcase char)))))

(define-test char-upcase/lowercase
  (check-for (letter-pair '((#\a . #\A) (#\b . #\B) (#\c . #\C) (#\d . #\D) (#\e . #\E)
                            (#\f . #\F) (#\g . #\G) (#\h . #\H) (#\i . #\I) (#\j . #\J)
                            (#\k . #\K) (#\l . #\L) (#\m . #\M) (#\n . #\N) (#\o . #\O)
                            (#\p . #\P) (#\q . #\Q) (#\r . #\R) (#\s . #\S) (#\t . #\T)
                            (#\u . #\U) (#\v . #\V) (#\w . #\W) (#\x . #\X) (#\y . #\Y)
                            (#\z . #\Z)))
    (dbind (input . expected) letter-pair
      (check (eq? expected (char-upcase input))))))


(define-test char-downcase/non-character
  (check-for (char '(0 0.0 "non-character"))
    (runtime-error? (char-downcase char))))

(define-test char-downcase/non-alphabetic
  (check-for (char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                     #\+ #\- #\space #\newline #\cr #\tab #\\))
    (check-for (eq? char (char-downcase char)))))

(define-test char-downcase/uppercase
  (check-for (char '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
                     #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
                     #\u #\v #\w #\x #\y #\z))
    (check-for (eq? char (char-downcase char)))))

(define-test char-downcase/lowercase
  (check-for (letter-pair '((#\A . #\a) (#\B . #\b) (#\C . #\c) (#\D . #\d) (#\E . #\e)
                            (#\F . #\f) (#\G . #\g) (#\H . #\h) (#\I . #\i) (#\J . #\j)
                            (#\K . #\k) (#\L . #\l) (#\M . #\m) (#\N . #\n) (#\O . #\o)
                            (#\P . #\p) (#\Q . #\q) (#\R . #\r) (#\S . #\s) (#\T . #\t)
                            (#\U . #\u) (#\V . #\v) (#\W . #\w) (#\X . #\x) (#\Y . #\y)
                            (#\Z . #\z)))
    (dbind (input . expected) letter-pair
      (check (eq? expected (char-downcase input))))))


(define-test character->string
  (check (runtime-error? (character->string 11)))
  (check (equal? "a" (character->string #\a))))

(define-test char?/positive
  (check-for (char '(#\a #\b #\Z #\newline #\space #\; #\@ #\#))
    (char? char)))

(define-test char?/negative
  (check-for (not-char '(1 symbol #t [1 2 3]))
    (not (char? not-char))))

(define-test integer->char/error
  (check-for (bad-val '(-1.0 65536.0 #\a [1] [1 2 3] [1 2 3]
                        '(1 2 3) -1 256 65535 65536))
    (runtime-error? (integer->char bad-val))))

(define-test integer->char/algebraic
  (check-for (n (iseq 0 256))
    (char? (integer->char n)))

  (check-for (n (iseq 0 256))
    (= n (char->integer (integer->char n))))

  (check-for (n (iseq 0 256))
    (char=? (integer->char n) (integer->char n))))
