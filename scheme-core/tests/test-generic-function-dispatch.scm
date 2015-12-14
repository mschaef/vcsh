(define-package "test-generic-function-dispatch"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

;;; Arity 1

(define-generic-function (tgf-1 x)
  :default-handling)

(define-method (tgf-1 (x fixnum)) :fixnum)
(define-method (tgf-1 (x flonum)) :flonum)
(define-method (tgf-1 (x complex)) :complex)
(define-method (tgf-1 (x character)) :character)
(define-method (tgf-1 (x string)) :streng) ; overwritten
(define-method (tgf-1 (x string)) :string)
(define-method (tgf-1 (x number)) :number)

(define-test generic-function-arity-1
  (check (eq? (tgf-1 3) :fixnum))
  (check (eq? (tgf-1 3.0) :flonum))
  (check (eq? (tgf-1 3i) :complex))
  (check (eq? (tgf-1 #\3) :character))
  (check (eq? (tgf-1 "3") :string))
  (check (eq? (tgf-1 [3]) :default-handling))
  (check (eq? (tgf-1 '(3)) :default-handling)))

;;; Arity 2

(define-generic-function (tgf-2 x y)
  :default-handling)

(define-method (tgf-2 (x fixnum) (y fixnum)) :fixnum)
(define-method (tgf-2 (x flonum) (y flonum)) :flonum)
(define-method (tgf-2 (x complex) (y complex)) :complex)
(define-method (tgf-2 (x character) (y character)) :character)
(define-method (tgf-2 (x string) (y string)) :streng) ; overwritten
(define-method (tgf-2 (x string) (y string)) :string) 

(define-generic-function (tgf-2a x y)
  :default-handling)

(define-method (tgf-2a (x fixnum) (y number)) :fixnum)
(define-method (tgf-2a (x flonum) (y number)) :flonum)
(define-method (tgf-2a (x complex) (y number)) :complex)

(define-generic-function (tgf-2b x y)
  :default-handling)

(define-method (tgf-2b (x number) (y fixnum)) :fixnum)
(define-method (tgf-2b (x number) (y flonum)) :flonum)
(define-method (tgf-2b (x number) (y complex)) :complex)

(define-test generic-function-arity-2
  (check (eq? (tgf-2 3 3) :fixnum))
  (check (eq? (tgf-2 3.0 3.0) :flonum))
  (check (eq? (tgf-2 3i 3i) :complex))
  (check (eq? (tgf-2 #\3  #\3) :character))
  (check (eq? (tgf-2 "3" "3") :string))

  (check (eq? (tgf-2 3 3) :fixnum))
  (check (eq? (tgf-2 3.0 3) :default-handling))
  (check (eq? (tgf-2 3i 3) :default-handling))

  (check (eq? (tgf-2 3 3) :fixnum))
  (check (eq? (tgf-2 3 3.0) :default-handling))
  (check (eq? (tgf-2 3 3i) :default-handling))


  (check (eq? (tgf-2a 3 3) :fixnum))
  (check (eq? (tgf-2a 3.0 3.0) :flonum))
  (check (eq? (tgf-2a 3i 3i) :complex))
  (check (eq? (tgf-2a #\3  #\3) :default-handling))
  (check (eq? (tgf-2a "3" "3") :default-handling))

  (check (eq? (tgf-2a 3 3) :fixnum))
  (check (eq? (tgf-2a 3.0 3) :flonum))
  (check (eq? (tgf-2a 3i 3) :complex))

  (check (eq? (tgf-2a 3 3) :fixnum))
  (check (eq? (tgf-2a 3 3.0) :fixnum))
  (check (eq? (tgf-2a 3 3i) :fixnum))

  (check (eq? (tgf-2b 3 3) :fixnum))
  (check (eq? (tgf-2b 3.0 3.0) :flonum))
  (check (eq? (tgf-2b 3i 3i) :complex))
  (check (eq? (tgf-2b #\3  #\3) :default-handling))
  (check (eq? (tgf-2b "3" "3") :default-handling))

  (check (eq? (tgf-2b 3 3) :fixnum))
  (check (eq? (tgf-2b 3.0 3) :fixnum))
  (check (eq? (tgf-2b 3i 3) :fixnum))

  (check (eq? (tgf-2b 3 3) :fixnum))
  (check (eq? (tgf-2b 3 3.0) :flonum))
  (check (eq? (tgf-2b 3 3i) :complex)))

;;; Arity 4

(define-generic-function (tgf-4 w x y z)
  :default-handling)

(define-method (tgf-4 (w fixnum) (x fixnum) (y fixnum) (z fixnum))
  :fixnum)

(define-method (tgf-4 (w number) (x number) (y number) (z number))
  :number)

(define-method (tgf-4 (w flonum) (x flonum) (y flonum) (z flonum))
  :flonum)

(define-test generic-function-arity-4
  (check (eq? (tgf-4 #\1 2 3 4) :default-handling))
  (check (eq? (tgf-4 1 #\2 3 4) :default-handling))
  (check (eq? (tgf-4 1 2 #\3 4) :default-handling))
  (check (eq? (tgf-4 1 2 3 #\4) :default-handling))
  (check (eq? (tgf-4 #\1 #\2 #\3 #\4) :default-handling))

  (check (eq? (tgf-4 1 2 3 4) :fixnum))

  (check (eq? (tgf-4 1.0 2 3 4) :number))
  (check (eq? (tgf-4 1 2.0 3 4) :number))
  (check (eq? (tgf-4 1 2 3.0 4) :number))
  (check (eq? (tgf-4 1 2 3 4.0) :number))

  (check (eq? (tgf-4 1.0 2.0 3.0 4.0) :flonum))
  (check (eq? (tgf-4 1i 2i 3i 4i) :flonum)))

;;; Arity 1 Inheritance

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

;;; Arity 2 Inheritance

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
  (check
   (equal? '(:base-handling)
           (checkpoint-order-of
            (tgfi-2 #\i #\i))))
  
  (check
   (equal? '(:base-handling)
           (checkpoint-order-of
            (tgfi-2 1 #\i))))
  
  (check
   (equal? '(:base-handling)
           (checkpoint-order-of
            (tgfi-2 #\i 1))))

  (check
   (equal? '(:pre-fixnum 
             :pre-number 
             :base-handling 
             :post-number 
             :post-fixnum)
           (checkpoint-order-of
            (tgfi-2 1 1))))

  (check
   (equal? '(:pre-number 
             :base-handling 
             :post-number)
           (checkpoint-order-of
            (tgfi-2 1 1.0))))

  (check
   (equal? '(:pre-number 
             :base-handling 
             :post-number)
           (checkpoint-order-of
            (tgfi-2 1.0 1))))

  (check
   (equal? '(:pre-flonum 
             :pre-number 
             :base-handling 
             :post-number 
             :post-flonum)
           (checkpoint-order-of
            (tgfi-2 1.0 1.0))))

  (check
   (equal? '(:pre-flonum 
             :pre-number 
             :base-handling 
             :post-number 
             :post-flonum)
           (checkpoint-order-of
            (tgfi-2 1.0 1i))))

  (check
   (equal? '(:pre-flonum 
             :pre-number 
             :base-handling 
             :post-number 
             :post-flonum)
           (checkpoint-order-of
            (tgfi-2 1i 1.0))))

  (check
   (equal? '(:pre-complex
             :pre-flonum 
             :pre-number 
             :base-handling 
             :post-number 
             :post-flonum
             :post-complex)
           (checkpoint-order-of
            (tgfi-2 1i 1i))))

  (check
   (equal? '(:base-handling)
           (checkpoint-order-of
            (tgfi-2a 1 1))))

  (check
   (equal? '(:base-handling)
           (checkpoint-order-of
            (tgfi-2a #\1 1))))

  (check
   (equal? '(:base-handling)
           (checkpoint-order-of
            (tgfi-2a 1 #\1))))

  (check
   (equal? '(:base-handling)
           (checkpoint-order-of
            (tgfi-2a #\1 1.0))))

  (check
   (equal? '(:base-handling)
           (checkpoint-order-of
            (tgfi-2a 1.0 #\1))))

  (check
   (equal? '(:pre-flonum-number
             :base-handling
             :post-flonum-number)
           (checkpoint-order-of
            (tgfi-2a 1.0 1))))
  
  (check
   (equal? '(:pre-number-flonum
             :base-handling
             :post-number-flonum)
           (checkpoint-order-of
            (tgfi-2a 1 1.0))))

  (check
   (equal? '(:pre-complex-number
             :pre-flonum-number
             :base-handling
             :post-flonum-number
             :post-complex-number)
           (checkpoint-order-of
            (tgfi-2a 1i 1))))

  (check
   (equal? '(:pre-number-complex
             :pre-number-flonum
             :base-handling
             :post-number-flonum
             :post-number-complex)
           (checkpoint-order-of
            (tgfi-2a 1 1i)))))

;;; Verify Return Values

(define-generic-function (tgf-rv x)
  1)

(define-method (tgf-rv (x number))
  (+ 3 (call-next-method)))

(define-method (tgf-rv (x fixnum))
  (+ 5 (call-next-method)))

(define-method (tgf-rv (x flonum))
  (+ 7 (call-next-method)))

(define-method (tgf-rv (x complex))
  (+ 13 (call-next-method)))

(define-test generic-function-return-value
  (check (= (tgf-rv #\f) 1))
  (check (= (tgf-rv 1) 9))
  (check (= (tgf-rv 1.0) 11))
  (check (= (tgf-rv 1i) 24)))

