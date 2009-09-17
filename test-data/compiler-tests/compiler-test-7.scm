
(defmacro (foo x)
  (unless (symbol? x) (error "foo") (error "bar"))
  x)

(define (incr x) (+ 1 (foo x)))