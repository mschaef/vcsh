(use-package! "unit-test")


(define-generic-function (tgfi-2 x y)
  (checkpoint :base-handling))

(define-method (tgfi-2 (x number) (y number))
  (checkpoint :pre-number)
  (call-next-method)
  (checkpoint :post-number))

(define-method (tgfi-2 (x fixnum) (y fixnum))
  (checkpoint :pre-fixnum)
  (call-next-method)
  (checkpoint :post-fixnum))

(define-method (tgfi-2 (x flonum) (y flonum))
  (checkpoint :pre-flonum)
  (call-next-method)
  (checkpoint :post-flonum))

(define-method (tgfi-2 (x complex) (y complex))
  (checkpoint :pre-complex)
  (call-next-method)
  (checkpoint :post-complex))

(define-generic-function (tgfi-2a x y)
  (checkpoint :base-handling))

(define-method (tgfi-2a (x flonum) (y number))
  (checkpoint :pre-flonum-number)
  (call-next-method)
  (checkpoint :post-flonum-number))

(define-method (tgfi-2a (x number) (y flonum))
  (checkpoint :pre-number-flonum)
  (call-next-method)
  (checkpoint :post-number-flonum))

(define-method (tgfi-2a (x complex) (y number))
  (checkpoint :pre-complex-number)
  (call-next-method)
  (checkpoint :post-complex-number))

(define-method (tgfi-2a (x number) (y complex))
  (checkpoint :pre-number-complex)
  (call-next-method)
  (checkpoint :post-number-complex))


(define-test generic-function-inheritance-arity-2
  (test-case/execution-order (:base-handling)
    (tgfi-2 #\i #\i))

  (test-case/execution-order (:base-handling)
    (tgfi-2 1 #\i))

  (test-case/execution-order (:base-handling)
    (tgfi-2 #\i 1))

  (test-case/execution-order (:pre-fixnum 
			  :pre-number 
			  :base-handling 
			  :post-number 
			  :post-fixnum)
    (tgfi-2 1 1))

  (test-case/execution-order (:pre-number 
			  :base-handling 
			  :post-number)
    (tgfi-2 1 1.0))

  (test-case/execution-order (:pre-number 
			  :base-handling 
			  :post-number)
    (tgfi-2 1.0 1))

  (test-case/execution-order (:pre-flonum 
			  :pre-number 
			  :base-handling 
			  :post-number 
			  :post-flonum)
    (tgfi-2 1.0 1.0))

  (test-case/execution-order (:pre-flonum 
			  :pre-number 
			  :base-handling 
			  :post-number 
			  :post-flonum)
    (tgfi-2 1.0 1i))

  (test-case/execution-order (:pre-flonum 
			  :pre-number 
			  :base-handling 
			  :post-number 
			  :post-flonum)
    (tgfi-2 1i 1.0))

  (test-case/execution-order (:pre-complex
			  :pre-flonum 
			  :pre-number 
			  :base-handling 
			  :post-number 
			  :post-flonum
			  :post-complex)
    (tgfi-2 1i 1i))

  (test-case/execution-order (:base-handling)
    (tgfi-2a 1 1))

  (test-case/execution-order (:base-handling)
    (tgfi-2a #\1 1))

  (test-case/execution-order (:base-handling)
    (tgfi-2a 1 #\1))


  (test-case/execution-order (:base-handling)
    (tgfi-2a #\1 1.0))

  (test-case/execution-order (:base-handling)
    (tgfi-2a 1.0 #\1))

  (test-case/execution-order (:pre-flonum-number
			  :base-handling
			  :post-flonum-number)
    (tgfi-2a 1.0 1))

  (test-case/execution-order (:pre-number-flonum
			  :base-handling
			  :post-number-flonum)
    (tgfi-2a 1 1.0))


  (test-case/execution-order (:pre-complex-number
			  :pre-flonum-number
			  :base-handling
			  :post-flonum-number
			  :post-complex-number)
    (tgfi-2a 1i 1))

  (test-case/execution-order (:pre-number-complex
			  :pre-number-flonum
			  :base-handling
			  :post-number-flonum
			  :post-number-complex)
    (tgfi-2a 1 1i))

  )

