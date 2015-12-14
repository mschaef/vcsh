(use-package! "unit-test")

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
