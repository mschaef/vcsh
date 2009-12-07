
(define (incr-xyzzy x)
  (+ x 1))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format #t "This should be 2 == ~s\n" (incr-xyzzy 1)))
