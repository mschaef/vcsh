
(defmacro (while cond-form . body)
  (with-gensyms (while-loop-sym)
    `(begin 
       (let ,while-loop-sym ()
            (when ,cond-form
              ,@body
              (,while-loop-sym)))
       (values))))
  
(defmacro (repeat limit-form . body)
  (with-gensyms (ii-sym limit-value-sym)
    `(let ((,ii-sym 0)
           (,limit-value-sym ,limit-form))
       (while (< ,ii-sym ,limit-value-sym)
         ,@body
         (incr! ,ii-sym)))))
