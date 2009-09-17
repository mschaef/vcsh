(use-package! "unit-test")

(define-test number-divide
  (test-case (runtime-error? (/ :non-number 12)))
  (test-case (runtime-error? (/ 12 :non-number)))
  (test-case (runtime-error? (/ :non-number)))
  
  (test-case (= (/ 1) 1))
  (test-case (inexact-= (/ 2.0) 0.5))
  (test-case (inexact-= (/ 4.0) 0.25))
  (test-case (inexact-= (/ 10) 0.1))
  (test-case (inexact? (/ 1)))
  (test-case (inexact? (/ 1.0)))

  (test-case (= (/ 1 2) 0.5))
  (test-case (inexact-= (/ 16.0 256.0) 0.0625))
  (test-case (inexact-= (/ 1.0 20) 0.05))
  (test-case (inexact? (/ 1 2)))
  (test-case (inexact? (/ 1.0 2.23)))
  (test-case (inexact? (/ 112.2 23.0)))
  (test-case (inexact? (/ 1.065 2.0)))

  (test-case (inexact-= (/ 1 2 3) (/ 1 6))))

(define-test quotient
  (test-case (runtime-error? (quotient :non-number 12)))
  (test-case (runtime-error? (quotient 12 :non-number)))

  (test-case (runtime-error? (quotient 12.0 12)))
  (test-case (runtime-error? (quotient 12 12.0)))

  (test-case (runtime-error? (quotient 12i 12)))
  (test-case (runtime-error? (quotient 12 12i)))
  
  (test-case (runtime-error? (quotient 1)))

  (test-case (= 5 (quotient 27 5)))
  (test-case (= -5 (quotient -27 5)))
  (test-case (= 0 (quotient 0 5)))

  (test-case (runtime-error? (quotient 0 0)))
  (test-case (runtime-error? (quotient 2 0)))
  )

(define-test modulo
  (test-case (runtime-error? (modulo :non-number 12)))
  (test-case (runtime-error? (modulo 12 :non-number)))

  (test-case (runtime-error? (modulo 12.0 12)))
  (test-case (runtime-error? (modulo 12 12.0)))

  (test-case (runtime-error? (modulo 12i 12)))
  (test-case (runtime-error? (modulo 12 12i)))
  
  (test-case (runtime-error? (modulo 1)))

  (test-case (runtime-error? (modulo 0 0)))
  (test-case (runtime-error? (modulo 2 0)))
 
  (test-case (= 1 (modulo 13 4)))
  (test-case (= 3 (modulo -13 4)))
  (test-case (= -3 (modulo 13 -4)))
  (test-case (= -1 (modulo -13 -4)))
  )

(define-test remainder
  (test-case (runtime-error? (remainder :non-number 12)))
  (test-case (runtime-error? (remainder 12 :non-number)))

  (test-case (runtime-error? (remainder 12.0 12)))
  (test-case (runtime-error? (remainder 12 12.0)))

  (test-case (runtime-error? (remainder 12i 12)))
  (test-case (runtime-error? (remainder 12 12i)))
  
  (test-case (runtime-error? (remainder 1)))

  (test-case (runtime-error? (remainder 0 0)))
  (test-case (runtime-error? (remainder 2 0)))

  (test-case (= 1 (remainder 13 4)))
  (test-case (= -1 (remainder -13 4)))
  (test-case (= 1 (remainder 13 -4)))
  (test-case (= -1 (remainder -13 -4)))
  )