(define-generic-function (foo x y z))

(define-method (foo (x fixnum) (y fixnum) (z fixnum)) (+ x y z))

(foo 1 2 3)