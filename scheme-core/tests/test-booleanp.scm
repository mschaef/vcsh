(use-package! "unit-test")

(define-test boolean?
  (check (boolean? #f))
  (check (boolean? #t))
  (check (not (boolean? 1)))
  (check (not (boolean? 'symbol)))
  (check (not (boolean? #\a)))
  (check (not (boolean? [1 2 3 4 5]))))
