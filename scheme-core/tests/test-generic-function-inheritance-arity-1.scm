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
  (check
   (equal? '(:base-handling)
           (checkpoint-order-of
            (tgfi-1 #\i))))
  
  (check
   (equal? '(:pre-fixnum 
             :pre-number 
             :base-handling 
             :post-number 
             :post-fixnum)
           (checkpoint-order-of
            (tgfi-1 1))))

  (check
   (equal? '(:pre-flonum 
             :pre-number 
             :base-handling 
             :post-number 
             :post-flonum)
           (checkpoint-order-of
            (tgfi-1 1.0))))

  (check
   (equal? '(:pre-complex
             :pre-flonum 
             :pre-number 
             :base-handling 
             :post-number 
             :post-flonum
             :post-complex)
           (checkpoint-order-of
            (tgfi-1 1i)))))

