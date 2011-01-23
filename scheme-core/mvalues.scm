;;;; mvalues.scm
;;;;
;;;; Multiple value support

(define (values . v)
  (if (= (length v) 1)
      (first v)
      (apply %values v)))

(define (call-with-values proc vals)
  (apply proc (%values->list vals)))

(defmacro (mvbind vars form . body)
  `(call-with-values (lambda ,vars ,@body) ,form))
