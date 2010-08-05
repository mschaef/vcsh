(define *const* 42)

(define (incr x) (+ x 1))

(defmacro (incr! x) `(set! ,x (incr ,x)))

(define-structure point x y)
