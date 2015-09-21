(use-package! "unit-test")

(define-test random
  (test-case (runtime-error? (random #t)))
  (test-case (runtime-error? (random :non-numeric)))
  
  (test-case (exact? (random 43)))
  (test-case (inexact? (random 43.0)))
  (test-case (inexact? (random)))
  (test-case (complex? (random 2+2i)))
  (test-case (complex? (random 2i))))
