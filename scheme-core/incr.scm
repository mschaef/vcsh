(define (incr x) (+ x 1))

(defmacro (incr2! x) `(set! ,x (incr ,x)))

(define-structure point x y)