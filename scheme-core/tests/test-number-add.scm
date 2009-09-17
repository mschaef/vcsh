(use-package! "unit-test")

(define-test number-add

  (test-case (runtime-error? (+ :non-number 12)))
  (test-case (runtime-error? (+ 12 :non-number)))
  (test-case (runtime-error? (+ :non-number)))
    
  (test-case (= (+ 1) 1))
  (test-case (inexact-= (+ 1.0) 1.0))
  (test-case (inexact-= (+ 1.0) 1))
  (test-case (inexact-= (+ 1) 1.0))
  (test-case (exact? (+ 1)))
  (test-case (inexact? (+ 1.0)))

  (test-case (= (+ 1 2) 3))
  (test-case (inexact-= (+ 21.7 90.2) 111.9))
  (test-case (inexact-= (+ 1.0 7) 8))
  (test-case (inexact-= (+ 1 9.8) 10.8))
  (test-case (exact? (+ 1 2)))
  (test-case (inexact? (+ 1.0 2)))
  (test-case (inexact? (+ 1 2.0)))
  (test-case (inexact? (+ 1.0 2.0)))

  (test-case (= (+ 1 2 3) 6))
  (test-case (inexact-= (+ 21.7 90.2 34.2) 146.1))
  (test-case (inexact-= (+ 1.0 7 1) 9))
  (test-case (inexact-= (+ 1 9.8 11) 21.8))
  (test-case (exact? (+ 1 2 1)))
  (test-case (inexact? (+ 1.0 2 1)))
  (test-case (inexact? (+ 1 2.0 2)))
  (test-case (inexact? (+ 1 2 2.0)))
  (test-case (inexact? (+ 1.0 2.0 4.0)))
   
   (test-case (= 55 (+ 10 9 8 7 6 5 4 3 2 1)))
   (test-case (exact? (+ 10 9 8 7 6 5 4 3 2 1)))
   (test-case (inexact? (+ 10 9 8 7 6 5.1 4 3 2 1)))
   (test-case (inexact-= (+ 10 9 8 7 6 5.1 4 3 2 1) 55.1)))
