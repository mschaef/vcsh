(define (incr x)
  (define (doit y) (+ y 1))
  (doit x))

(defmacro (begin-1 . code)
  (case (length code)
    ((0) ())
    ((1) `(begin ,@code))
    (#t  (with-gensyms (return-value-sym)
           `(let ((,return-value-sym ,(car code)))
              ,@(cdr code)
              ,return-value-sym)))))