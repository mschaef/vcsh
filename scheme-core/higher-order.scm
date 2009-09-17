;;;; higher-order.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; Tools related to higher order programming.

(define (always x)
  "Returns a closure that always returns <x>."
  (lambda () x))

(define (negate pred?)
  "Returns a one argument predicate that has the opposite sense
   as <pred?>"
  (lambda (x) (not (pred? x))))


(define (identity x) x)

(define (%template->args/params template)
  (map (lambda (template-arg) (cons (gensym "cut-arg") template-arg)) template))

