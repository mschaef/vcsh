(defmacro (define-list-maker name . elements)
  `(define (,name) ',elements))

(define-list-maker foo eleven twelve thirteen 14)