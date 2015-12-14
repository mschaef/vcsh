(use-package! "unit-test")

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
