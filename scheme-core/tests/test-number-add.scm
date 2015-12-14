(use-package! "unit-test")

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
