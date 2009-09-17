(use-package! "unit-test")

(define-test random
  (test-case (runtime-error? (random 0)))
  (test-case (runtime-error? (random 0.0)))
  (test-case (runtime-error? (random 0.0+0.0i)))
  (test-case (runtime-error? (random #t)))
  (test-case (runtime-error? (random :non-numeric)))
  
  (test-case (exact? (random 43)))
  (test-case (inexact? (random 43.0)))
  (test-case (inexact? (random)))

  (let ((cr (random 2+2i)))
    (inexact? cr)
    (not (= (imag-part cr) 0.0))))