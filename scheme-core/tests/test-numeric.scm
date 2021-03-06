(define-package "test-numeric"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test number-add

  (check (runtime-error? (+ :non-number 12)))
  (check (runtime-error? (+ 12 :non-number)))
  (check (runtime-error? (+ :non-number)))
    
  (check (= (+ 1) 1))
  (check (inexact-= (+ 1.0) 1.0))
  (check (inexact-= (+ 1.0) 1))
  (check (inexact-= (+ 1) 1.0))
  (check (exact? (+ 1)))
  (check (inexact? (+ 1.0)))

  (check (= (+ 1 2) 3))
  (check (inexact-= (+ 21.7 90.2) 111.9))
  (check (inexact-= (+ 1.0 7) 8))
  (check (inexact-= (+ 1 9.8) 10.8))
  (check (exact? (+ 1 2)))
  (check (inexact? (+ 1.0 2)))
  (check (inexact? (+ 1 2.0)))
  (check (inexact? (+ 1.0 2.0)))

  (check (= (+ 1 2 3) 6))
  (check (inexact-= (+ 21.7 90.2 34.2) 146.1))
  (check (inexact-= (+ 1.0 7 1) 9))
  (check (inexact-= (+ 1 9.8 11) 21.8))
  (check (exact? (+ 1 2 1)))
  (check (inexact? (+ 1.0 2 1)))
  (check (inexact? (+ 1 2.0 2)))
  (check (inexact? (+ 1 2 2.0)))
  (check (inexact? (+ 1.0 2.0 4.0)))
   
   (check (= 55 (+ 10 9 8 7 6 5 4 3 2 1)))
   (check (exact? (+ 10 9 8 7 6 5 4 3 2 1)))
   (check (inexact? (+ 10 9 8 7 6 5.1 4 3 2 1)))
   (check (inexact-= (+ 10 9 8 7 6 5.1 4 3 2 1) 55.1)))

(define *fixnum-bits* (* 8 (system-info :size-of-fixnum)))

(define-test bitwise-and
  (check (= (bitwise-and 15 7) 7))
  (check (= (bitwise-and 15 39) 7))
  (check (= (bitwise-and 65535 39) 39))
  (check (= (bitwise-and 65535 0) 0))
  (check (= (bitwise-and 0 0) 0))
  (check (= (bitwise-and 65535 65535) 65535))
  (check (runtime-error? (bitwise-and 15.0 7)))
  (check (runtime-error? (bitwise-and 15 7.0)))
  (check (runtime-error? (bitwise-and 15 #t)))
  (check (runtime-error? (bitwise-and #t 15))))

(define-test bitwise-or
  (check (= (bitwise-or 0) 0))
  (check (= (bitwise-or 15) 15))
  (check (= (bitwise-or 15 7) 15))
  (check (= (bitwise-or 15 39) 47))
  (check (= (bitwise-or 65535 39) 65535))
  (check (= (bitwise-or 65535 0) 65535))
  (check (= (bitwise-or 0 0) 0))
  (check (= (bitwise-or 65535 65535) 65535))
  (check (runtime-error? (bitwise-or 15.0 7)))
  (check (runtime-error? (bitwise-or 15 7.0)))
  (check (runtime-error? (bitwise-or 15 #t)))
  (check (runtime-error? (bitwise-or #t 15))))

(define-test bitwise-xor
  (check (= (bitwise-xor 0) 0))
  (check (= (bitwise-xor 15) 15))
  (check (= (bitwise-xor 15 7) 8))
  (check (= (bitwise-xor 15 39) 40))
  (check (= (bitwise-xor 65535 39) 65496))
  (check (= (bitwise-xor 65535 0) 65535))
  (check (= (bitwise-xor 0 0) 0))
  (check (= (bitwise-xor 65535 65535) 0))
  (check (runtime-error? (bitwise-xor 15.0 7)))
  (check (runtime-error? (bitwise-xor 15 7.0)))
  (check (runtime-error? (bitwise-xor 15 #t)))
  (check (runtime-error? (bitwise-xor #t 15))))

(define-test bitwise-not
  (check (= (bitwise-not 0) -1))
  (check (= (bitwise-not 1) -2))
  (check (= (bitwise-not -1) 0))
  (check (= (bitwise-not -2) 1))
  (check (= (bitwise-not 1000) -1001))
  (check (runtime-error? (bitwise-not 7.0)))
  (check (runtime-error? (bitwise-not #t))))

(define-test bitwise-shift-left
  (check (= (bitwise-shift-left 0 (+ 1 *fixnum-bits*)) 0))
  (check (= (bitwise-shift-left -256 *fixnum-bits*) 0))
  (check (= (bitwise-shift-left 256 *fixnum-bits*) 0))

  (check (= (bitwise-shift-left 0 (+ 1 *fixnum-bits*)) 0))
  (check (= (bitwise-shift-left -256 (+ 1 *fixnum-bits*)) 0))
  (check (= (bitwise-shift-left 256 (+ 1 *fixnum-bits*)) 0))

  (check (= (bitwise-shift-left 1 1) 2))
  (check (= (bitwise-shift-left 1 2) 4))
  (check (= (bitwise-shift-left 1 3) 8))
  (check (= (bitwise-shift-left 1 4) 16))
  (check (= (bitwise-shift-left 1 5) 32))
  (check (= (bitwise-shift-left 1 6) 64))
  (check (= (bitwise-shift-left 1 7) 128))
  (check (= (bitwise-shift-left 1 8) 256))
  (check (= (bitwise-shift-left 5 1) 10))
  (check (= (bitwise-shift-left 5 2) 20))
  (check (= (bitwise-shift-left 5 3) 40))
  (check (= (bitwise-shift-left 5 4) 80))
  (check (= (bitwise-shift-left 5 5) 160))
  (check (= (bitwise-shift-left 5 6) 320))
  (check (= (bitwise-shift-left 5 7) 640))
  (check (= (bitwise-shift-left 5 8) 1280))
  (check (= (bitwise-shift-left 1 0) 1))
  (check (= (bitwise-shift-left 2 0) 2))
  (check (= (bitwise-shift-left 0 1) 0))
  (check (= (bitwise-shift-left 0 2) 0))
  (check (runtime-error? (bitwise-shift-left)))
  (check (runtime-error? (bitwise-shift-left 1))))

(define-test bitwise-shift-right
  (check (= (bitwise-shift-right 0 (+ 1 *fixnum-bits*)) 0))
  (check (= (bitwise-shift-right -256 *fixnum-bits*) 0))
  (check (= (bitwise-shift-right 256 *fixnum-bits*) 0))

  (check (= (bitwise-shift-right 0 (+ 1 *fixnum-bits*)) 0))
  (check (= (bitwise-shift-right -256 (+ 1 *fixnum-bits*)) 0))
  (check (= (bitwise-shift-right 256 (+ 1 *fixnum-bits*)) 0))

  (check (= (bitwise-shift-right 256 9) 0))
  (check (= (bitwise-shift-right 256 8) 1))
  (check (= (bitwise-shift-right 256 7) 2))
  (check (= (bitwise-shift-right 256 6) 4))
  (check (= (bitwise-shift-right 256 5) 8))
  (check (= (bitwise-shift-right 256 4) 16))
  (check (= (bitwise-shift-right 256 3) 32))
  (check (= (bitwise-shift-right 256 2) 64))
  (check (= (bitwise-shift-right 256 1) 128))
  (check (= (bitwise-shift-right 256 0) 256))
  (check (= (bitwise-shift-right 1 0) 1))
  (check (= (bitwise-shift-right 1 1) 0))
  (check (= (bitwise-shift-right 1 8) 0))
  (check (= (bitwise-shift-right 5 0) 5))
  (check (= (bitwise-shift-right 5 1) 2))
  (check (= (bitwise-shift-right 5 2) 1))
  (check (= (bitwise-shift-right 5 3) 0))
  (check (= (bitwise-shift-right 1 0) 1))
  (check (= (bitwise-shift-right 1 0) 1))
  (check (= (bitwise-shift-right 0 1) 0))
  (check (= (bitwise-shift-right 0 8) 0))
  (check (runtime-error? (bitwise-shift-right)))
  (check (runtime-error? (bitwise-shift-right 1))))

(define-test bitwise-arithmatic-shift-right
  (check (= (bitwise-arithmatic-shift-right 0 (+ 1 *fixnum-bits*)) 0))
  (check (= (bitwise-arithmatic-shift-right -256 *fixnum-bits*) 0))
  (check (= (bitwise-arithmatic-shift-right 256 *fixnum-bits*) 0))

  (check (= (bitwise-arithmatic-shift-right 0 (+ 1 *fixnum-bits*)) 0))
  (check (= (bitwise-arithmatic-shift-right -256 (+ 1 *fixnum-bits*)) 0))
  (check (= (bitwise-arithmatic-shift-right 256 (+ 1 *fixnum-bits*)) 0))

  (check (= (bitwise-arithmatic-shift-right 256 9) 0))
  (check (= (bitwise-arithmatic-shift-right 256 8) 1))
  (check (= (bitwise-arithmatic-shift-right 256 7) 2))
  (check (= (bitwise-arithmatic-shift-right 256 6) 4))
  (check (= (bitwise-arithmatic-shift-right 256 5) 8))
  (check (= (bitwise-arithmatic-shift-right 256 4) 16))
  (check (= (bitwise-arithmatic-shift-right 256 3) 32))
  (check (= (bitwise-arithmatic-shift-right 256 2) 64))
  (check (= (bitwise-arithmatic-shift-right 256 1) 128))
  (check (= (bitwise-arithmatic-shift-right 256 0) 256))
  (check (= (bitwise-arithmatic-shift-right 1 0) 1))
  (check (= (bitwise-arithmatic-shift-right 1 1) 0))
  (check (= (bitwise-arithmatic-shift-right 1 8) 0))
  (check (= (bitwise-arithmatic-shift-right 5 0) 5))
  (check (= (bitwise-arithmatic-shift-right 5 1) 2))
  (check (= (bitwise-arithmatic-shift-right 5 2) 1))
  (check (= (bitwise-arithmatic-shift-right 5 3) 0))
  (check (= (bitwise-arithmatic-shift-right 1 0) 1))
  (check (= (bitwise-arithmatic-shift-right 1 0) 1))
  (check (= (bitwise-arithmatic-shift-right 0 0) 0))
  (check (= (bitwise-arithmatic-shift-right 0 1) 0))
  (check (= (bitwise-arithmatic-shift-right 0 8) 0))
  (check (runtime-error? (bitwise-arithmatic-shift-right)))
  (check (runtime-error? (bitwise-arithmatic-shift-right 1))))

(define-test number-divide
  (check (runtime-error? (/ :non-number 12)))
  (check (runtime-error? (/ 12 :non-number)))
  (check (runtime-error? (/ :non-number)))
  
  (check (= (/ 1) 1))
  (check (inexact-= (/ 2.0) 0.5))
  (check (inexact-= (/ 4.0) 0.25))
  (check (inexact-= (/ 10) 0.1))
  (check (inexact? (/ 1)))
  (check (inexact? (/ 1.0)))

  (check (= (/ 1 2) 0.5))
  (check (inexact-= (/ 16.0 256.0) 0.0625))
  (check (inexact-= (/ 1.0 20) 0.05))
  (check (inexact? (/ 1 2)))
  (check (inexact? (/ 1.0 2.23)))
  (check (inexact? (/ 112.2 23.0)))
  (check (inexact? (/ 1.065 2.0)))

  (check (inexact-= (/ 1 2 3) (/ 1 6))))

(define-test quotient
  (check (runtime-error? (quotient :non-number 12)))
  (check (runtime-error? (quotient 12 :non-number)))

  (check (runtime-error? (quotient 12i 12)))
  (check (runtime-error? (quotient 12 12i)))
  
  (check (runtime-error? (quotient 1)))

  (check (= 5 (quotient 27 5)))
  (check (= -5 (quotient -27 5)))
  (check (= 0 (quotient 0 5)))

  (check (runtime-error? (quotient 0 0)))
  (check (runtime-error? (quotient 2 0))))

(define-test modulo
  (check (runtime-error? (modulo :non-number 12)))
  (check (runtime-error? (modulo 12 :non-number)))

  (check (runtime-error? (modulo 12.0 12)))
  (check (runtime-error? (modulo 12 12.0)))

  (check (runtime-error? (modulo 12i 12)))
  (check (runtime-error? (modulo 12 12i)))
  
  (check (runtime-error? (modulo 1)))

  (check (runtime-error? (modulo 0 0)))
  (check (runtime-error? (modulo 2 0)))
 
  (check (= 1 (modulo 13 4)))
  (check (= 3 (modulo -13 4)))
  (check (= -3 (modulo 13 -4)))
  (check (= -1 (modulo -13 -4))))

(define-test remainder
  (check (runtime-error? (remainder :non-number 12)))
  (check (runtime-error? (remainder 12 :non-number)))

  (check (runtime-error? (remainder 12i 12)))
  (check (runtime-error? (remainder 12 12i)))
  
  (check (runtime-error? (remainder 1)))

  (check (runtime-error? (remainder 0 0)))
  (check (runtime-error? (remainder 2 0)))

  (check (= 1 (remainder 13 4)))
  (check (= -1 (remainder -13 4)))
  (check (= 1 (remainder 13 -4)))
  (check (= -1 (remainder -13 -4))))

(define-test number-exactness
  (check (exact? 1))
  (check (exact? 100))
  (check (exact? -100))
  (check (inexact? 1.0))
  (check (inexact? 100.0))
  (check (inexact? -100.0))
  (check (inexact? 1e1))
  (check (inexact? 100e1))
  (check (inexact? -100e1))
  (check (not (inexact? 1)))
  (check (not (inexact? 100)))
  (check (not (inexact? -100)))
  (check (not (exact? 1.0)))
  (check (not (exact? 100.0)))
  (check (not (exact? -100.0)))
  (check (not (exact? 1e1)))
  (check (not (exact? 100e1)))
  (check (not (exact? -100e1)))

  (check (runtime-error? (exact->inexact :non-numeric)))
  (check (not (exact? (exact->inexact 1))))
  (check (not (exact? (exact->inexact 100))))
  (check (not (exact? (exact->inexact -100))))

  (check (equal? 1.0 (exact->inexact 1)))
  (check (equal? 100.0 (exact->inexact 100)))
  (check (equal? -100.0 (exact->inexact -100))) 
  
  (check (runtime-error? (inexact->exact :non-numeric)))
  (check (not (inexact? (inexact->exact 1.0))))
  (check (not (inexact? (inexact->exact 100.0))))
  (check (not (inexact? (inexact->exact -100.0))))

  (check (equal? 1 (inexact->exact 1)))
  (check (equal? 1 (inexact->exact 1.0)))
  (check (equal? 100 (inexact->exact 100.0)))
  (check (equal? -100 (inexact->exact -100.0)))

  (check (not (inexact->exact 1e30)))
  (check (not (inexact->exact -1e30))))

(define-test number-io
  (check (can-read/write-round-trip? #iposinf))
  (check (can-read/write-round-trip? #ineginf))
  (check (can-read/write-round-trip? #inan))
  (check (can-read/write-round-trip? 1))
  (check (can-read/write-round-trip? 100))
  (check (can-read/write-round-trip? -100))
  (check (can-read/write-round-trip? 1.0))
  (check (can-read/write-round-trip? 100.0))
  (check (can-read/write-round-trip? -100.0))
  (check (can-read/write-round-trip? 1e1))
  (check (can-read/write-round-trip? 100e1))
  (check (can-read/write-round-trip? -100e1)))

(define-test number-multiply
  (check (runtime-error? (* :non-number 12)))
  (check (runtime-error? (* 12 :non-number)))
  (check (runtime-error? (* :non-number)))

  (check (= (* 1) 1))
  (check (inexact-= (* 1.0) 1.0))
  (check (inexact-= (* 1.0) 1))
  (check (inexact-= (* 1) 1.0))
  (check (exact? (* 1)))
  (check (inexact? (* 1.0)))

  (check (= (* 1 2) 2))
  (check (inexact-= (* 21.7 90.2) 1957.34))
  (check (inexact-= (* 1.0 7) 7.0))
  (check (inexact-= (* 1 9.8) 9.8))
  (check (exact? (* 1 2)))
  (check (inexact? (* 1.0 2)))
  (check (inexact? (* 1 2.0)))
  (check (inexact? (* 1.0 2.0)))

  (check (= (* 1 2 3) 6))
  (check (inexact-= (* 21.7 90.2 34.2) 66941.028))
  (check (inexact-= (* 1.0 7 1) 7.0))
  (check (inexact-= (* 1 9.8 11) 107.8))
  (check (exact? (* 1 2 1)))
  (check (inexact? (* 1.0 2 1)))
  (check (inexact? (* 1 2.0 2)))
  (check (inexact? (* 1 2 2.0)))
  (check (inexact? (* 1.0 2.0 4.0)))

  (check (= 3628800 (* 10 9 8 7 6 5 4 3 2 1)))
  (check (exact? (* 10 9 8 7 6 5 4 3 2 1)))
  (check (inexact? (* 10 9 8 7 6 5.1 4 3 2 1)))
  (check (inexact-= (* 10 9 8 7 6 5.1 4 3 2 1) 3701376)))

;; Test cases generated with this expression:
;;
;; (dynamic-let ((*flonum-print-precision* 3))
;;   (stable (map (lambda (x)
;;                  `(check (= ,(eval x) ,x)))
;;                (list-combinations '((round truncate floor ceiling)
;;                                     (-20 -1 0 1 20
;;                                          -20.9 -20.6 -20.5 -20.4 -20.1 -20
;;                                          -1.9 -1.6 -1.5 -1.4 -1.1 -1.0
;;                                          -0.9 -0.6 -0.5 -0.4 -0.1 -0.0
;;                                          20.9 20.6 20.5 20.4 20.1 20
;;                                          1.9 1.6 1.5 1.4 1.1 1.0
;sw;                                          0.9 0.6 0.5 0.4 0.1))))))

(define-test number-parts/special-numbers
  (check (runtime-error? (round :non-number)))
  (check (runtime-error? (ceiling :non-number)))
  (check (runtime-error? (floor :non-number)))
  (check (runtime-error? (truncate :non-number))))

(define-test number-parts/exact-round
  (check (= -20 (round -20)))
  (check (= -1  (round -1 )))
  (check (= 0   (round 0  )))
  (check (= 1   (round 1  )))
  (check (= 20  (round 20 )))
  (check (= -20 (round -20)))
  (check (= 20  (round 20 ))))

(define-test number-parts/exact-truncate
  (check (= -20 (truncate -20)))
  (check (= -1  (truncate -1 )))
  (check (= 0   (truncate 0  )))
  (check (= 1   (truncate 1  )))
  (check (= 20  (truncate 20 )))
  (check (= -20 (truncate -20)))
  (check (= 20  (truncate 20 ))))

(define-test number-parts/exact-floor
  (check (= -20 (floor -20)))
  (check (= -1  (floor -1 )))
  (check (= 0   (floor 0  )))
  (check (= 1   (floor 1  )))
  (check (= 20  (floor 20 )))
  (check (= -20 (floor -20)))
  (check (= 20  (floor 20 ))))

(define-test number-parts/exact-ceiling
  (check (= -20 (floor -20)))
  (check (= -1  (floor -1 )))
  (check (= 0   (floor 0  )))
  (check (= 1   (floor 1  )))
  (check (= 20  (floor 20 )))
  (check (= -20 (floor -20)))
  (check (= 20  (floor 20 ))))

(define-test number-parts/inexact-round
  (check (= -21.000 (round -20.900)))
  (check (= -21.000 (round -20.600)))
  (check (= -21.000 (round -20.500)))
  (check (= -20.000 (round -20.400)))
  (check (= -20.000 (round -20.100)))

  (check (= -2.000 (round -1.900)))
  (check (= -2.000 (round -1.600)))
  (check (= -2.000 (round -1.500)))
  (check (= -1.000 (round -1.400)))
  (check (= -1.000 (round -1.100)))
  (check (= -1.000 (round -1.000)))
  (check (= -1.000 (round -0.900)))
  (check (= -1.000 (round -0.600)))
  (check (= -1.000 (round -0.500)))
  (check (= -0.000 (round -0.400)))
  (check (= -0.000 (round -0.100)))
  (check (= -0.000 (round -0.000)))
  (check (= 21.000 (round 20.900)))
  (check (= 21.000 (round 20.600)))
  (check (= 21.000 (round 20.500)))
  (check (= 20.000 (round 20.400)))
  (check (= 20.000 (round 20.100)))

  (check (= 2.000 (round 1.900)))
  (check (= 2.000 (round 1.600)))
  (check (= 2.000 (round 1.500)))
  (check (= 1.000 (round 1.400)))
  (check (= 1.000 (round 1.100)))
  (check (= 1.000 (round 1.000)))
  (check (= 1.000 (round 0.900)))
  (check (= 1.000 (round 0.600)))
  (check (= 1.000 (round 0.500)))
  (check (= 0.000 (round 0.400)))
  (check (= 0.000 (round 0.100))))

(define-test number-parts/inexact-truncate
  (check (= -20.000 (truncate -20.900)))
  (check (= -20.000 (truncate -20.600)))
  (check (= -20.000 (truncate -20.500)))
  (check (= -20.000 (truncate -20.400)))
  (check (= -20.000 (truncate -20.100)))
  (check (= -1.000 (truncate -1.900)))
  (check (= -1.000 (truncate -1.600)))
  (check (= -1.000 (truncate -1.500)))
  (check (= -1.000 (truncate -1.400)))
  (check (= -1.000 (truncate -1.100)))
  (check (= -1.000 (truncate -1.000)))
  (check (= -0.000 (truncate -0.900)))
  (check (= -0.000 (truncate -0.600)))
  (check (= -0.000 (truncate -0.500)))
  (check (= -0.000 (truncate -0.400)))
  (check (= -0.000 (truncate -0.100)))
  (check (= -0.000 (truncate -0.000)))
  (check (= 20.000 (truncate 20.900)))
  (check (= 20.000 (truncate 20.600)))
  (check (= 20.000 (truncate 20.500)))
  (check (= 20.000 (truncate 20.400)))
  (check (= 20.000 (truncate 20.100)))
  (check (= 1.000 (truncate 1.900)))
  (check (= 1.000 (truncate 1.600)))
  (check (= 1.000 (truncate 1.500)))
  (check (= 1.000 (truncate 1.400)))
  (check (= 1.000 (truncate 1.100)))
  (check (= 1.000 (truncate 1.000)))
  (check (= 0.000 (truncate 0.900)))
  (check (= 0.000 (truncate 0.600)))
  (check (= 0.000 (truncate 0.500)))
  (check (= 0.000 (truncate 0.400)))
  (check (= 0.000 (truncate 0.100))))


(define-test number-parts/inexact-floor
  
  (check (= -21.000 (floor -20.900)))
  (check (= -21.000 (floor -20.600)))
  (check (= -21.000 (floor -20.500)))
  (check (= -21.000 (floor -20.400)))
  (check (= -21.000 (floor -20.100)))
  (check (= -2.000 (floor -1.900)))
  (check (= -2.000 (floor -1.600)))
  (check (= -2.000 (floor -1.500)))
  (check (= -2.000 (floor -1.400)))
  (check (= -2.000 (floor -1.100)))
  (check (= -1.000 (floor -1.000)))
  (check (= -1.000 (floor -0.900)))
  (check (= -1.000 (floor -0.600)))
  (check (= -1.000 (floor -0.500)))
  (check (= -1.000 (floor -0.400)))
  (check (= -1.000 (floor -0.100)))
  (check (= -0.000 (floor -0.000)))
  (check (= 20.000 (floor 20.900)))
  (check (= 20.000 (floor 20.600)))
  (check (= 20.000 (floor 20.500)))
  (check (= 20.000 (floor 20.400)))
  (check (= 20.000 (floor 20.100)))
  (check (= 1.000 (floor 1.900)))
  (check (= 1.000 (floor 1.600)))
  (check (= 1.000 (floor 1.500)))
  (check (= 1.000 (floor 1.400)))
  (check (= 1.000 (floor 1.100)))
  (check (= 1.000 (floor 1.000)))
  (check (= 0.000 (floor 0.900)))
  (check (= 0.000 (floor 0.600)))
  (check (= 0.000 (floor 0.500)))
  (check (= 0.000 (floor 0.400)))
  (check (= 0.000 (floor 0.100))))

(define-test number-parts/inexact-ceiling
  (check (= -20.000 (ceiling -20.900)))
  (check (= -20.000 (ceiling -20.600)))
  (check (= -20.000 (ceiling -20.500)))
  (check (= -20.000 (ceiling -20.400)))
  (check (= -20.000 (ceiling -20.100)))
  (check (= -1.000 (ceiling -1.900)))
  (check (= -1.000 (ceiling -1.600)))
  (check (= -1.000 (ceiling -1.500)))
  (check (= -1.000 (ceiling -1.400)))
  (check (= -1.000 (ceiling -1.100)))
  (check (= -1.000 (ceiling -1.000)))
  (check (= -0.000 (ceiling -0.900)))
  (check (= -0.000 (ceiling -0.600)))
  (check (= -0.000 (ceiling -0.500)))
  (check (= -0.000 (ceiling -0.400)))
  (check (= -0.000 (ceiling -0.100)))
  (check (= -0.000 (ceiling -0.000)))
  (check (= 21.000 (ceiling 20.900)))
  (check (= 21.000 (ceiling 20.600)))
  (check (= 21.000 (ceiling 20.500)))
  (check (= 21.000 (ceiling 20.400)))
  (check (= 21.000 (ceiling 20.100)))
  (check (= 2.000 (ceiling 1.900)))
  (check (= 2.000 (ceiling 1.600)))
  (check (= 2.000 (ceiling 1.500)))
  (check (= 2.000 (ceiling 1.400)))
  (check (= 2.000 (ceiling 1.100)))
  (check (= 1.000 (ceiling 1.000)))
  (check (= 1.000 (ceiling 0.900)))
  (check (= 1.000 (ceiling 0.600)))
  (check (= 1.000 (ceiling 0.500)))
  (check (= 1.000 (ceiling 0.400)))
  (check (= 1.000 (ceiling 0.100))))

(define-test number-type-predicates
  (check (integer? 0))
  (check (integer? 1))
  (check (integer? 100))
  (check (rational? 0))
  (check (rational? 1))
  (check (rational? 100))
  (check (real? 0))
  (check (real? 1))
  (check (real? 100))
  (check (not (complex? 0)))
  (check (not (complex? 1)))
  (check (not (complex? 100)))
  (check (number? 0))
  (check (number? 1))
  (check (number? 100))
  (check (not (integer? 0.0)))
  (check (not (integer? 1.0)))
  (check (not (integer? 100.0)))
  (check (not (rational? 0.0)))
  (check (not (rational? 1.0)))
  (check (not (rational? 100.0)))
  (check (real? 0.0))
  (check (real? 1.0))
  (check (real? 100.0))
  (check (not (complex? :not-complex)))
  (check (not (complex? 0.0)))
  (check (not (complex? 1.0)))
  (check (not (complex? 100.0)))
  (check (not (number? :foo)))
  (check (number? 0.0))
  (check (number? 1.0))
  (check (number? 100.0))
  (check (complex? 1+2i))
  (check (complex? 1-2i))
  (check (complex? -1+2i))
  (check (complex? -1-2i))
  (check (not (real? 1+2i)))
  (check (not (real? 1-2i)))
  (check (not (real? -1+2i)))
  (check (not (real? -1-2i))))

(define-test number->string
  (check (equal? "0" (number->string 0 10)))
  (check (equal? "12" (number->string 12 10)))
  
  (check (equal? "-12" (number->string -12 10 #t)))
  (check (equal? "18446744073709551604" (number->string -12 10 #f)))
  (check (equal? "18446744073709551604" (number->string -12 10)))
  
  (check (equal? "0" (number->string 0 16)))
  (check (equal? "12" (number->string 18 16)))

  (check (equal? "-12" (number->string -18 16 #t)))
  (check (equal?  "ffffffffffffffee" (number->string -18 16 #f)))
  (check (equal?  "ffffffffffffffee" (number->string -18 16)))
    
  (check (equal? "cafe" (number->string 51966 16)))

  (check (equal? "-cafe" (number->string -51966 16 #t)))
  (check (equal? "ffffffffffff3502" (number->string -51966 16 #f)))
  (check (equal? "ffffffffffff3502" (number->string -51966 16)))

  (check (equal? "0" (number->string 0 8)))
  (check (equal? "12" (number->string 10 8)))

  (check (equal? "-12" (number->string -10 8 #t)))
  (check (equal? "1777777777777777777766" (number->string -10 8 #f)))
  (check (equal? "1777777777777777777766" (number->string -10 8)))

  (check (runtime-error? (number->string 12 2)))
  (check (runtime-error? (number->string 12 2.2)))
  (check (runtime-error? (number->string 12 #\a)))
  (check (runtime-error? (number->string 12 #f))))

(define-test numeric-complex-comparison
  (check (> 10 9 8 7 6 5 4 3 2 1))
  (check (< 1 2 3 4 5 6 7 8 9 10))
  (check (>= 10 9 8 7 6 5 4 3 2 1))
  (check (<= 1 2 3 4 5 6 7 8 9 10))
  (check (>= 10 10 8 8 6 6 4 4 2 2))
  (check (<= 1 1 3 3 5 5 7 7 9 9))
  (check (not (> 1 9 8 7 6 5 4 3 2 1)))
  (check (not (> 10 9 8 7 60 5 4 3 2 1)))
  (check (not (> 10 9 8 7 6 5 4 3 2 10)))
  (check (not (< 10 2 3 4 5 6 7 8 9 10)))
  (check (not (< 1 2 3 4 50 6 7 8 9 10)))
  (check (not (< 1 2 3 4 5 6 7 8 9 1))	)
  (check (not (>= 1 9 8 7 6 5 4 3 2 1)))
  (check (not (>= 10 9 80 7 6 5 4 3 2 1)))
  (check (not (>= 10 9 8 7 6 5 4 3 2 10)))
  (check (not (<= 10 2 3 4 5 6 7 8 9 10)))
  (check (not (<= 1 2 3 45 5 6 7 8 9 1)))
  (check (not (<= 12 2 3 4 5 6 77 8 9 1)))
  (check (not (>= 1 1 8 8 6 6 4 4 2 2)))
  (check (not (>= 10 10 8 8 60 6 4 4 2 2)))
  (check (not (>= 10 10 8 8 6 6 4 4 20 20)))
  (check (not (<= 10 10 3 3 5 5 7 7 9 9)))
  (check (not (<= 1 1 3 3 55 5 7 7 9 9)))
  (check (not (<= 1 1 3 3 5 5 7 7 1 9)))

  (check (> 10.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0))
  (check (< 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0))
  (check (>= 10.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0))
  (check (<= 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0))
  (check (>= 10.0 10.0 8.0 8.0 6.0 6.0 4.0 4.0 2.0 2.0))
  (check (<= 1.0 1.0 3.0 3.0 5.0 5.0 7.0 7.0 9.0 9.0))
  (check (not (> 1.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0)))
  (check (not (> 10.0 9.0 8.0 7.0 60.0 5.0 4.0 3.0 2.0 1.0)))
  (check (not (> 10.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 10.0))	)
  (check (not (< 10.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0)))
  (check (not (< 1.0 2.0 3.0 4.0 50.0 6.0 7.0 8.0 9.0 10.0)))
  (check (not (< 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 1.0))	)
  (check (not (>= 1.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0)))
  (check (not (>= 10.0 9.0 80.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0)))
  (check (not (>= 10.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 10.0))	)
  (check (not (<= 10.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0)))
  (check (not (<= 1.0 2.0 3.0 45.0 5.0 6.0 7.0 8.0 9.0 1.0)))
  (check (not (<= 12 2.0 3.0 4.0 5.0 6.0 77.0 8.0 9.0 1.0))	)
  (check (not (>= 1.0 1.0 8.0 8.0 6.0 6.0 4.0 4.0 2.0 2.0)))
  (check (not (>= 10.0 10.0 8.0 8.0 60.0 6.0 4.0 4.0 2.0 2.0)))
  (check (not (>= 10.0 10.0 8.0 8.0 6.0 6.0 4.0 4.0 20.0 20.0))	)
  (check (not (<= 10.0 10.0 3.0 3.0 5.0 5.0 7.0 7.0 9.0 9.0)))
  (check (not (<= 1.0 1.0 3.0 3.0 55.0 5.0 7.0 7.0 9.0 9.0)))
  (check (not (<= 1.0 1.0 3.0 3.0 5.0 5.0 7.0 7.0 1.0 9.0)))

  (check (> 10.0 9 8 7 6 5 4 3 2 1))
  (check (< 1 2.0 3 4 5 6 7 8 9 10))
  (check (>= 10 9 8.0 7 6 5 4 3 2 1))
  (check (<= 1 2 3 4.0 5 6 7 8 9 10))
  (check (>= 10 10 8 8 6.0 6 4 4 2 2))
  (check (<= 1 1 3 3 5 5.0 7 7 9 9))
  (check (not (> 1 9 8 7 6 5 4.0 3 2 1)))
  (check (not (> 10 9 8 7 60 5 4 3.0 2 1)))
  (check (not (> 10 9 8 7 6 5 4 3 2.0 10))	)
  (check (not (< 10 2 3 4 5 6 7 8 9 10.0)))
  (check (not (< 1.0 2 3 4 50 6 7 8 9 10)))
  (check (not (< 1 2.0 3 4 5 6 7 8 9 1))	)
  (check (not (>= 1 9 8.0 7 6 5 4 3 2 1)))
  (check (not (>= 10 9 80 7.0 6 5 4 3 2 1)))
  (check (not (>= 10 9 8 7 6.0 5 4 3 2 10))	)
  (check (not (<= 10 2 3 4 5 6.0 7 8 9 10)))
  (check (not (<= 1 2 3 45 5 6 7.0 8 9 1)))
  (check (not (<= 12 2 3 4 5 6 77 8.0 9 1))	)
  (check (not (>= 1 1 8 8 6 6 4 4 2.0 2.0)))
  (check (not (>= 10.0 10 8 8 60 6 4 4 2 2)))
  (check (not (>= 10 10.0 8 8 6 6 4 4 20 20))	)
  (check (not (<= 10 10 3.0 3 5 5 7 7 9 9)))
  (check (not (<= 1 1 3 3.0 55 5 7 7 9 9)))
  (check (not (<= 1 1 3 3 5.0 5 7 7 1 9)))

  (check (> 10 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0))
  (check (< 1.0 2 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0))
  (check (>= 10.0 9.0 8 7.0 6.0 5.0 4.0 3.0 2.0 1.0))
  (check (<= 1.0 2.0 3.0 4 5.0 6.0 7.0 8.0 9.0 10.0))
  (check (>= 10.0 10.0 8.0 8.0 6 6.0 4.0 4.0 2.0 2.0))
  (check (<= 1.0 1.0 3.0 3.0 5.0 5 7.0 7.0 9.0 9.0))
  (check (not (> 1.0 9.0 8.0 7.0 6.0 5 4.0 3.0 2.0 1.0)))
  (check (not (> 10.0 9.0 8.0 7.0 60.0 5.0 4 3.0 2.0 1.0)))
  (check (not (> 10.0 9.0 8.0 7.0 6.0 5.0 4.0 3 2.0 10.0)))
  (check (not (< 10.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9 10.0)))
  (check (not (< 1.0 2.0 3.0 4.0 50.0 6.0 7.0 8.0 9.0 10)))
  (check (not (< 1 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 1.0)))
  (check (not (>= 1.0 9 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0)))
  (check (not (>= 10.0 9.0 80 7.0 6.0 5.0 4.0 3.0 2.0 1.0)))
  (check (not (>= 10.0 9.0 8.0 7 6.0 5.0 4.0 3.0 2.0 10.0)))
  (check (not (<= 10.0 2.0 3.0 4.0 5 6.0 7.0 8.0 9.0 10.0)))
  (check (not (<= 1.0 2.0 3.0 45.0 5.0 6 7.0 8.0 9.0 1.0)))
  (check (not (<= 12 2.0 3.0 4.0 5.0 6.0 77 8 9 1.0)))
  (check (not (>= 1.0 1.0 8.0 8.0 6.0 6.0 4.0 4.0 2 2.0)))
  (check (not (>= 10.0 10.0 8.0 8.0 60.0 6.0 4.0 4.0 2 2.0)))
  (check (not (>= 10.0 10.0 8.0 8.0 6.0 6.0 4.0 4.0 20.0 20)))
  (check (not (<= 10 10.0 3.0 3.0 5.0 5.0 7.0 7.0 9.0 9.0)))
  (check (not (<= 1.0 1 3.0 3.0 55.0 5.0 7.0 7.0 9.0 9.0))))

(define-test numeric-equality
  (check (= 1))
  (check (= 1.0))

  (check (= 1 1))
  (check (not (= 1 2)))
  (check (= 1.0 1.0))
  (check (not (= 1.0 2.0)))
  (check (= 1 1.0))
  (check (not (= 1 2.0)))
  (check (= 1.0 1))
  (check (not (= 1.0 2)))

  (check (= 1 1 1))
  (check (not (= 2 1 1)))
  (check (not (= 1 2 1)))
  (check (not (= 1 1 2)))
  (check (= 1.0 1.0 1.0))
  (check (not (= 2.0 1.0 1.0)))
  (check (not (= 1.0 2.0 1.0)))
  (check (not (= 1.0 1.0 2.0)))
  (check (= 1.0 1 1.0))
  (check (not (= 2.0 1 1.0)))
  (check (not (= 1.0 2 1.0)))
  (check (not (= 1.0 1 2.0)))
  (check (= 1 1.0 1.0))
  (check (not (= 2 1.0 1.0)))
  (check (not (= 1 2.0 1.0)))
  (check (not (= 1 1.0 2.0)))

  (check (= 1 1 1 1))
  (check (not (= 2 1 1 1)))
  (check (not (= 1 2 1 1)))
  (check (not (= 1 1 2 1)))
  (check (not (= 1 1 2 2)))

  (check (eqv? 1 1))
  (check (eqv? 1.0 1.0))
  (check (not (eqv? 1 2)))
  (check (not (eqv? 1.0 2.0)))
  (check (not (eqv? 1.0 1)))
  (check (not (eqv? 1 1.0)))
  (check (not (eqv? 1.0 2)))
  (check (not (eqv? 1 2.0)))

  (check (equal? 1 1))
  (check (equal? 1.0 1.0))
  (check (not (equal? 1 2)))
  (check (not (equal? 2 1)))
  (check (not (equal? 1.0 2.0)))
  (check (not (equal? 2.0 1.0)))
  (check (not (equal? 1 1.0)))
  (check (not (equal? 1.0 1)))
  (check (not (equal? 2 1.0)))
  (check (not (equal? 1.0 2)))

  (check (= #iposinf #iposinf))
  (check (= #ineginf #ineginf))

  (check (not (= #inan #inan)))
  (check (not (= #iposinf #ineginf)))
  (check (not (= #ineginf #iposinf)))
  (check (not (= #iposinf #inan)))
  (check (not (= #ineginf #inan)))
  (check (not (= #inan #ineginf)))
  (check (not (= #inan #iposinf)))

  (check (equal? #iposinf #iposinf))
  (check (equal? #ineginf #ineginf))
  (check (equal? #inan #inan))
  (check (not (equal? #iposinf #ineginf)))
  (check (not (equal? #ineginf #iposinf)))

  (check (not (equal? #iposinf #inan)))
  (check (not (equal? #ineginf #inan)))
  (check (not (equal? #inan #ineginf)))
  (check (not (equal? #inan #iposinf))))

(define-test numeric-simple-comparison
  (check (> 2 1))
  (check (not (> 1 1)))
  (check (not (> 1 2)))
  (check (>= 2 1))
  (check (>= 1 1))
  (check (not (>= 1 2)))
  (check (< 1 2))
  (check (not (< 1 1)))
  (check (not (< 2 1)))
  (check (<= 1 2))
  (check (<= 1 1))
  (check (not (<= 2 1)))

  (check (> 2.0 1.0))
  (check (not (> 1.0 1.0)))
  (check (not (> 1.0 2.0)))
  (check (>= 2.0 1.0))
  (check (>= 1.0 1.0))
  (check (not (>= 1.0 2.0)))
  (check (< 1.0 2.0))
  (check (not (< 1.0 1.0)))
  (check (not (< 2.0 1.0)))
  (check (<= 1.0 2.0))
  (check (<= 1.0 1.0))
  (check (not (<= 2.0 1.0)))

  (check (> 2.0 1))
  (check (not (> 1.0 1)))
  (check (not (> 1.0 2)))
  (check (>= 2.0 1))
  (check (>= 1.0 1))
  (check (not (>= 1.0 2)))
  (check (< 1.0 2))
  (check (not (< 1.0 1)))
  (check (not (< 2.0 1)))
  (check (<= 1.0 2))
  (check (<= 1.0 1))
  (check (not (<= 2.0 1)))
  (check (> 2 1.0))
  (check (not (> 1 1.0)))
  (check (not (> 1 2.0)))
  (check (>= 2 1.0))
  (check (>= 1 1.0))
  (check (not (>= 1 2.0)))
  (check (< 1 2.0))
  (check (not (< 1 1.0)))
  (check (not (< 2 1.0)))
  (check (<= 1 2.0))
  (check (<= 1 1.0))
  (check (not (<= 2 1.0)))

  (check (> 8 4 2))
  (check (not (> 3 4 2)))
  (check (not (> 8 9 2)))
  (check (not (> 8 4 5)))
  (check (not (> 2 4 8)))

  (check (>= 8 4 2))
  (check (>= 8 8 2))
  (check (>= 8 4 4))
  (check (>= 8 8 8)		)
  (check (not (>= 3 4 2)))
  (check (not (>= 8 9 2)))
  (check (not (>= 8 4 5)))
  (check (not (>= 2 4 8)))
  (check (not (>= 4 4 8)))
  (check (not (>= 8 9 9)))

  (check (< 2 4 8))
  (check (not (< 2 4 3)))
  (check (not (< 2 9 8)))
  (check (not (< 5 4 8)))
  (check (not (< 8 4 2)))

  (check (<= 2 4 8))
  (check (<= 2 8 8))
  (check (<= 4 4 8))
  (check (<= 8 8 8))
  (check (not (<= 2 4 3)))
  (check (not (<= 2 9 8)))
  (check (not (<= 5 4 8)))
  (check (not (<= 8 4 2)))
  (check (not (<= 8 4 4)))
  (check (not (<= 9 9 8)))

  (check (> 8.0 4.0 2.0))
  (check (not (> 3.0 4.0 2.0)))
  (check (not (> 8.0 9.0 2.0)))
  (check (not (> 8.0 4.0 5.0)))
  (check (not (> 2.0 4.0 8.0)))

  (check (>= 8.0 4.0 2.0))
  (check (>= 8.0 8.0 2.0))
  (check (>= 8.0 4.0 4.0))
  (check (>= 8.0 8.0 8.0))
  (check (not (>= 3.0 4.0 2.0)))
  (check (not (>= 8.0 9.0 2.0)))
  (check (not (>= 8.0 4.0 5.0)))
  (check (not (>= 2.0 4.0 8.0)))
  (check (not (>= 4.0 4.0 8.0)))
  (check (not (>= 8.0 9.0 9.0)))

  (check (< 2.0 4.0 8.0))
  (check (not (< 2.0 4.0 3.0)))
  (check (not (< 2.0 9.0 8.0)))
  (check (not (< 5.0 4.0 8.0)))
  (check (not (< 8.0 4.0 2.0)))

  (check (<= 2.0 4.0 8.0))
  (check (<= 2.0 8.0 8.0))
  (check (<= 4.0 4.0 8.0))
  (check (<= 8.0 8.0 8.0))
  (check (not (<= 2.0 4.0 3.0)))
  (check (not (<= 2.0 9.0 8.0)))
  (check (not (<= 5.0 4.0 8.0)))
  (check (not (<= 8.0 4.0 2.0)))
  (check (not (<= 8.0 4.0 4.0)))
  (check (not (<= 9.0 9.0 8.0))))

(define-test complex-rect-accessors
  (check (equal? 3   (real-part 3)))
  (check (equal? 0   (imag-part 3)))
  (check (equal? 3.0 (real-part 3.0)))
  (check (equal? 0.0 (imag-part 3.0)))
  (check (equal? 3.0 (real-part (make-rectangular 3.0 4.0))))
  (check (equal? 4.0 (imag-part (make-rectangular 3.0 4.0)))))

(define-test random
  (check (runtime-error? (random #t)))
  (check (runtime-error? (random :non-numeric)))
  
  (check (exact? (random 43)))
  (check (inexact? (random 43.0)))
  (check (inexact? (random)))
  (check (complex? (random 2+2i)))
  (check (complex? (random 2i))))

(define-test nan-infinite?
  (check (not (infinite? 0)))
  (check (not (infinite? #\a)))
  (check (not (infinite? 'symbol)))
  (check (not (infinite? '())))
  (check (not (infinite? '(1 2 3))))
  (check (not (infinite? 0.0)))

  (check (not (nan? 0)))
  (check (not (nan? #\a)))
  (check (not (nan? 'symbol)))
  (check (not (nan? '())))
  (check (not (nan? '(1 2 3))))
  (check (not (nan? 0.0)))

  (check (infinite? (/ 12 0)))
  (check (infinite? (/ -12 0)))

  (check (infinite? (make-rectangular (/ 12 0) 12)))
  (check (infinite? (make-rectangular (/ -12 0) 12)))
  (check (infinite? (make-rectangular 12 (/ 12 0))))
  (check (infinite? (make-rectangular 12 (/ -12 0))))
  (check (infinite? (make-rectangular (/ 12 0) (/ 12 0))))
  (check (infinite? (make-rectangular (/ 12 0) (/ -12 0))))
  (check (infinite? (make-rectangular (/ -12 0) (/ 12 0))))
  (check (infinite? (make-rectangular (/ -12 0) (/ -12 0))))

  (check (nan? (/ 0 0)))
  (check (nan? (/ 0 0)))
  (check (nan? (make-rectangular (/ 0 0) 12)))
  (check (nan? (make-rectangular 12 (/ 0 0))))
  (check (nan? (make-rectangular (/ 0 0) (/ 0 0)))))

(define-test flonum-sin ; TESTTHIS
 ) 
