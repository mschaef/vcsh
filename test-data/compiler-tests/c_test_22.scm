
(defmacro (make-incrementer name value)
  `(begin
     (define (,name sym) (incr! sym ,value))))

(make-incrementer plusser 2)

