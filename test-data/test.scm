
(define (+1/form var) `(+ 1 ,var))

(defmacro (increment var)
  (+1/form var))

(define *ii* 0)

(define (next)
  (set! *ii* (increment *ii*))
  *ii*)

(defmacro (compile-time-hello-world)
  '(format #t "hello world\n"))


(compile-time-hello-world)

(eval-when (:compile-toplevel :load-toplevel)
  (write "foo")
  (write "bar"))


(define-proto xyzzy
  'x 1
  'y 2)

(define-proto (contra xyzzy)	
  'z 2
   )

(defmacro (add-eleven x)
  (error "compile time error")
  `(+ ,x 11))

#;(define (bar2 x)
  (add-eleven x))

(define (foo a b)
  #"${a} foo ${b}")

(define (x)
  (if x 1 2))



(define (q)
  (dynamic-let ((*x* 11))
    (if x 1 2 3)))

 (define (x y)
  (list-let (a b c) y
    (+ a b c)))

(defmacro (test2)
  (list 'if 1 2 3 4))

(define (foo)
  (test2))