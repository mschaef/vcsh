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

(define (strip-tags x)
  (if (tagged-object? x)
      (strip-tags (tagged-object-value x))
      x))

