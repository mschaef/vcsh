(use-package! "unit-test")

(define-test random
  (check (runtime-error? (random #t)))
  (check (runtime-error? (random :non-numeric)))
  
  (check (exact? (random 43)))
  (check (inexact? (random 43.0)))
  (check (inexact? (random)))
  (check (complex? (random 2+2i)))
  (check (complex? (random 2i))))
