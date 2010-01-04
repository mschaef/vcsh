;;;; tagged-object.scm
;;;; Mike Schaeffer
;;
;; Tag Object
;;
;; This code implements a mechanism by which arbitrary lisp
;; objects can be tagged with descriptive information.

(define-structure tagged-object
  value
  tag)

(define-method (vc-object->f-text (o tagged-object))
  (list #f 
	(list :italic (list :bold (tagged-object-tag o) ": "))
	(vc-object->f-text (tagged-object-value o))))

(define-method (write-vc-object (o tagged-object) port)
  (format port ":~a:" (tagged-object-tag o))
  (write-vc-object (tagged-object-value o) port))

(define (tag-object value tag)
  (make-tagged-object :value value
                      :tag   tag))

(define (object-value object)
  (while (tagged-object? object)
    (set! object (slot-ref object 'value)))
  object)

(defmacro (with-object-values vars . body)
  `(let (,@(map (lambda (var) `(,var (object-value ,var))) vars))
     ,@body))

(defmacro (strip-tags! . vars)
  `(begin
     ,@(map
        (lambda (var)
          `(set! ,var (object-value ,var)))
        vars)))
