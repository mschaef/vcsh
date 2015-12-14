(define-package "test-misc"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test misc
  (check (can-read/write-round-trip? #t))
  (check (can-read/write-round-trip? #f))
  (check (boolean? (> 1 2)))
  (check (boolean? (> 2 1)))
  (check (boolean? (not #t)))
  (check (boolean? (not #f)))
  (check (runtime-error? (= #t #t)))
  (check (eq? #t #t))
  (check (eqv? #t #t))
  (check (equal? #t #t))
  (check (not (eq? #t #f)))
  (check (not (eqv? #t #f)))
  (check (not (equal? #t #f)))
  (check #t)
  (check (not #f)))
