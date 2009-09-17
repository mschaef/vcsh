(use-package! "unit-test")

(define-generic-function (tgfi-1 x)
  (checkpoint :base-handling))

(define-method (tgfi-1 (x number))
  (checkpoint :pre-number)
  (call-next-method)
  (checkpoint :post-number))

(define-method (tgfi-1 (x fixnum))
  (checkpoint :pre-fixnum)
  (call-next-method)
  (checkpoint :post-fixnum))

(define-method (tgfi-1 (x flonum))
  (checkpoint :pre-flonum)
  (call-next-method)
  (checkpoint :post-flonum))

(define-method (tgfi-1 (x complex))
  (checkpoint :pre-complex)
  (call-next-method)
  (checkpoint :post-complex))

(define-test generic-function-inheritance-arity-1
  (test-case/execution-order (:base-handling)
    (tgfi-1 #\i))

  (test-case/execution-order (:pre-fixnum 
			  :pre-number 
			  :base-handling 
			  :post-number 
			  :post-fixnum)
    (tgfi-1 1))

  (test-case/execution-order (:pre-flonum 
			  :pre-number 
			  :base-handling 
			  :post-number 
			  :post-flonum)
    (tgfi-1 1.0))

  (test-case/execution-order (:pre-complex
			  :pre-flonum 
			  :pre-number 
			  :base-handling 
			  :post-number 
			  :post-flonum
			  :post-complex)
    (tgfi-1 1i))

  )

