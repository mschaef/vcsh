
(define (cond-test x)
  (cond ((eq? x 0) :foo)
        ((> x 1) :bar)
        (#t :frobozzle)))