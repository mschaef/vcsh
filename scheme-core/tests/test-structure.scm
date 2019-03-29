(define-package "test-structure"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-structure ship x y dx dy)
(define-structure ship-2 x y dx dy)

(define-structure %int-test foo)

(define (symbol-bound-to-procedure? sym)
  (runtime-check symbol? sym)
  (and (symbol-bound? sym)
       (procedure? (symbol-value sym))))

(define-test structure-slots
  (check (runtime-error? (structure-slots 12)))
  (check (set-same? '(:x :y :dx :dy) (structure-slots 'ship)))
  (check (set-same? '(:x :y :dx :dy) (structure-slots (make-ship)))))

(define-test structure-accessors
  ;; All of these should exist
  (check (symbol-bound-to-procedure? 'make-ship))
  (check (symbol-bound-to-procedure? 'copy-ship))
  (check (symbol-bound-to-procedure? 'ship?))

  (check (symbol-bound-to-procedure? 'set-ship-x!))
  (check (symbol-bound-to-procedure? 'set-ship-y!))
  (check (symbol-bound-to-procedure? 'set-ship-dx!))
  (check (symbol-bound-to-procedure? 'set-ship-dy!))

  (check (symbol-bound-to-procedure? 'ship-x))
  (check (symbol-bound-to-procedure? 'ship-y))
  (check (symbol-bound-to-procedure? 'ship-dx))
  (check (symbol-bound-to-procedure? 'ship-dy))

  ;; test naming of accessors for internal structures
  (check (symbol-bound-to-procedure? 'make-%int-test))
  (check (symbol-bound-to-procedure? 'copy-%int-test))
  (check (symbol-bound-to-procedure? '%int-test?))

  (check (symbol-bound-to-procedure? 'set-%int-test-foo!))
  (check (symbol-bound-to-procedure? '%int-test-foo))

  ;; Type predicates
  (check (ship? (make-ship)))
  (check (ship-2? (make-ship-2)))

  (check (not (ship? (make-ship-2))))
  (check (not (ship-2? (make-ship))))

  (check (not (ship? 12)))
  (check (not (ship? 'ship)))

  (let ((ts (make-ship)))
    (check (eq? (ship-x ts) ()))
    (check (eq? (ship-y ts) ()))
    (check (eq? (ship-dx ts) ()))
    (check (eq? (ship-dx ts) ()))

    (set-ship-x! ts :foo)

    (check (eq? (ship-x ts) :foo))
    (check (eq? (ship-y ts) ()))
    (check (eq? (ship-dx ts) ()))
    (check (eq? (ship-dx ts) ()))

    (set-ship-y! ts :bar)
    (set-ship-dx! ts :baz)
    (set-ship-dy! ts :blo)

    (check (eq? (ship-x ts) :foo))
    (check (eq? (ship-y ts) :bar))
    (check (eq? (ship-dx ts) :baz))
    (check (eq? (ship-dy ts) :blo))

    (let ((ts2 (make-ship)))
      (check (eq? (ship-x ts2) ()))
      (check (eq? (ship-y ts2) ()))
      (check (eq? (ship-dx ts2) ()))
      (check (eq? (ship-dx ts2) ())))

    ;; TODO: test copy-ship on non-ship structure
    (check (runtime-error? (copy-ship :not-a-structure)))

    (let ((tsc (copy-ship ts)))
      (check (not (eq? ts tsc)))
      (check (ship? tsc))

      (check (eq? (ship-x tsc) :foo))
      (check (eq? (ship-y tsc) :bar))
      (check (eq? (ship-dx tsc) :baz))
      (check (eq? (ship-dy tsc) :blo))))

  (check (runtime-error? (set-ship-x! :not-a-stucture 12)))
  (check (runtime-error? (ship-x :not-a-stucture)))

  (let ((ts (make-ship)))
    ;; (check (runtime-error? (copy-ship-2 ts)))
    (check (runtime-error? (set-ship-2-x! ts 0)))
    (check (runtime-error? (set-ship-2-y! ts 0)))
    (check (runtime-error? (set-ship-2-dx! ts 0)))
    (check (runtime-error? (set-ship-2-dy! ts 0))))

  (let ((ts2 (make-ship :x 2 :y 4 :dx -3.4 :dy :stationary)))
    (check (equal? (ship-x ts2) 2))
    (check (equal? (ship-y ts2) 4))
    (check (equal? (ship-dx ts2) -3.4))
    (check (equal? (ship-dy ts2) :stationary)))

  (check (runtime-error? (make-ship :bad-slot 12))))

(define-test structure-equal
  (let ((ts-a (make-ship))
        (ts-b (make-ship))
        (ts2 (make-ship-2)))

    (check (not (equal? ts-a ts2)))
    (check (not (equal? ts-a 'symbol)))
    (check (not (equal? ts-a 12)))

    (check (equal? ts-a ts-b))

    (set-ship-x! ts-a 12)
    (check (not (equal? ts-a ts-b)))
    (set-ship-x! ts-b 12)

    (check (equal? ts-a ts-b))

    (set-ship-y! ts-a :foo)
    (check (not (equal? ts-a ts-b)))
    (set-ship-y! ts-b :foo)

    (check (equal? ts-a ts-b))

    (set-ship-dx! ts-a '(1 2 3))
    (check (not (equal? ts-a ts-b)))
    (set-ship-dx! ts-b '(1 2 3))

    (check (equal? ts-a ts-b))

    (set-ship-dy! ts-a ts2)
    (check (not (equal? ts-a ts-b)))
    (set-ship-dy! ts-b ts2)

    (check (equal? ts-a ts-b))))

(define-test structure-sxhash
  (let ((ts-a (make-ship))
        (ts-b (make-ship))
        (ts2 (make-ship-2)))

    (check (not (= (sxhash ts-a) (sxhash ts2))))

    (check (= (sxhash ts-a) (sxhash ts-b)))

    (set-ship-x! ts-a 12)
    (check (not (= (sxhash ts-a) (sxhash ts-b))))
    (set-ship-x! ts-b 12)

    (check (= (sxhash ts-a) (sxhash ts-b)))

    (set-ship-y! ts-a :foo)
    (check (not (= (sxhash ts-a) (sxhash ts-b))))
    (set-ship-y! ts-b :foo)

    (check (= (sxhash ts-a) (sxhash ts-b)))

    (set-ship-dx! ts-a '(1 2 3))
    (check (not (= (sxhash ts-a) (sxhash ts-b))))
    (set-ship-dx! ts-b '(1 2 3))

    (check (= (sxhash ts-a) (sxhash ts-b)))

    (set-ship-dy! ts-a ts2)
    (check (not (= (sxhash ts-a) (sxhash ts-b))))
    (set-ship-dy! ts-b ts2)

    (check (= (sxhash ts-a) (sxhash ts-b)))))

(define-structure empty-structure)

(define-test structure-empty
  (check (symbol-bound-to-procedure? 'make-empty-structure))
  (check (symbol-bound-to-procedure? 'copy-empty-structure))
  (check (symbol-bound-to-procedure? 'empty-structure?))

  (check (empty-structure? (make-empty-structure))))

(define-structure ship-defaults
  (x :default 3)
  (y :default 4.0)
  (dx :default #\a)
  (dy :default :foo))

(define-test structure-simple-defaults
  (check (symbol-bound-to-procedure? 'make-ship-defaults))
  (check (symbol-bound-to-procedure? 'copy-ship-defaults))
  (check (symbol-bound-to-procedure? 'ship-defaults?))

  (check (symbol-bound-to-procedure? 'set-ship-defaults-x!))
  (check (symbol-bound-to-procedure? 'set-ship-defaults-y!))
  (check (symbol-bound-to-procedure? 'set-ship-defaults-dx!))
  (check (symbol-bound-to-procedure? 'set-ship-defaults-dy!))

  (check (symbol-bound-to-procedure? 'ship-defaults-x))
  (check (symbol-bound-to-procedure? 'ship-defaults-y))
  (check (symbol-bound-to-procedure? 'ship-defaults-dx))
  (check (symbol-bound-to-procedure? 'ship-defaults-dy))

  (let ((ts (make-ship-defaults)))
    (check (equal? (ship-defaults-x ts) 3))
    (check (equal? (ship-defaults-y ts) 4.0))
    (check (equal? (ship-defaults-dx ts) #\a))
    (check (equal? (ship-defaults-dy ts) :foo))

    (set-ship-defaults-x! ts :foo)
    (set-ship-defaults-y! ts :bar)
    (set-ship-defaults-dx! ts :baz)
    (set-ship-defaults-dy! ts :blo)

    (let ((ts2 (make-ship-defaults)))
      (check (equal? (ship-defaults-x ts2) 3))
      (check (equal? (ship-defaults-y ts2) 4.0))
      (check (equal? (ship-defaults-dx ts2) #\a))
      (check (equal? (ship-defaults-dy ts2) :foo)))))

(define-structure default-tester
  (s-1 :default (begin (checkpoint 1) :foo))
  (s-2 :default (begin (checkpoint 2) :bar))
  (s-3 :default (begin (checkpoint 3) :baz)))

(define-test structure-evaluated-defaults
  (check
   (equal? '(1 2 3)
           (checkpoint-order-of 
            (let ((dt (make-default-tester)))
              (check (eq? (default-tester-s-1 dt) :foo))
              (check (eq? (default-tester-s-2 dt) :bar))
              (check (eq? (default-tester-s-3 dt) :baz))))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of 
            (let ((dt (make-structure-by-name 'default-tester)))
              (check (eq? (default-tester-s-1 dt) :foo))
              (check (eq? (default-tester-s-2 dt) :bar))
              (check (eq? (default-tester-s-3 dt) :baz))))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of 
            (let ((dt (with-package "test-structure"
                                    (read-from-string "#S(default-tester)"))))
              (check (eq? (default-tester-s-1 dt) :foo))
              (check (eq? (default-tester-s-2 dt) :bar))
              (check (eq? (default-tester-s-3 dt) :baz))))))

  (check
   (equal? '()
           (checkpoint-order-of 
            (let ((dt (make-default-tester :s-1 1 :s-2 2 :s-3 3)))
              (check (eq? (default-tester-s-1 dt) 1))
              (check (eq? (default-tester-s-2 dt) 2))
              (check (eq? (default-tester-s-3 dt) 3))))))

  (check
   (equal? '()
           (checkpoint-order-of 
            (let ((dt (make-structure-by-name 'default-tester :s-1 1 :s-2 2 :s-3 3)))
              (check (eq? (default-tester-s-1 dt) 1))
              (check (eq? (default-tester-s-2 dt) 2))
              (check (eq? (default-tester-s-3 dt) 3))))))

  (check
   (equal? '()
           (checkpoint-order-of 
            (let ((dt (with-package "test-structure"
                                    (read-from-string "#S(default-tester :s-1 1 :s-2 2 :s-3 3)"))))
              (check (eq? (default-tester-s-1 dt) 1))
              (check (eq? (default-tester-s-2 dt) 2))
              (check (eq? (default-tester-s-3 dt) 3)))))))

(define-structure ship-fancy
  (x :default 3 :get "westing")
  (y :default 4.0 :set #f)
  (z :default 4 :get "northing" :set "raise-the-mainsail"))

(define-test test-structure-fancy
  (check (symbol-bound-to-procedure? 'make-ship-fancy))
  (check (symbol-bound-to-procedure? 'copy-ship-fancy))
  (check (symbol-bound-to-procedure? 'ship-fancy?))

  (check (symbol-bound-to-procedure? 'set-ship-fancy-x!))
  (check (symbol-bound-to-procedure? 'raise-the-mainsail))

  (check (symbol-bound-to-procedure? 'westing))
  (check (symbol-bound-to-procedure? 'ship-fancy-y))
  (check (symbol-bound-to-procedure? 'northing)))

(define-test structure-io
  (check (can-read/write-round-trip?  (make-ship)))
  (check (can-read/write-round-trip? (make-empty-structure)))

  (let ((ts (make-ship)))
    (set-ship-x! ts 12)
    (set-ship-y! ts (make-ship))
    (check (can-read/write-round-trip? ts)))

  (let ((ts (with-package "test-structure" (read-from-string "#S(ship)"))))
    (check (ship? ts))
    (check (eq? () (ship-x ts)))
    (check (eq? () (ship-y ts))))

  (let ((ts (with-package "test-structure" (read-from-string "#S(ship :x 3)"))))
    (check (ship? ts))
    (check (eq? 3 (ship-x ts)))
    (check (eq? () (ship-y ts))))

  (let ((ts (with-package "test-structure" (read-from-string "#S(ship :x 3 :y 4)"))))
    (check (ship? ts))
    (check (eq? 3 (ship-x ts)))
    (check (eq? 4 (ship-y ts))))

  (check (runtime-error? (read-from-string "#S(bad-structure-type)")))
  (check (runtime-error? (read-from-string "#S(12)")))
  (check (runtime-error? (read-from-string "#S()")))
  (check (runtime-error? (read-from-string "#S(ship :bad-slot 12)")))
  (check (runtime-error? (read-from-string "#S(ship :x 3 :bad-slot 12)")))
  (check (runtime-error? (read-from-string "#S(ship :bad-slot 12 :y 5)"))))

(define-test structure-fast-io
  (check (can-fast-io-round-trip? (make-ship)))
  (check (can-fast-io-round-trip? (make-empty-structure)))
  (check (can-fast-io-round-trip? (make-ship :x 3 :y 4)))

  (check (can-fast-io-round-trip? (make-ship :x 3 :y (make-ship :x 12 :y 23))))

  (let ((st (make-ship :x 12)))
    (set-ship-y! st st))

  (let ((st (make-ship)))
    (set-ship-x! st st)
    (set-ship-y! st st)))

(define-test define-structure-errors
  (check (eq? 'define-structure-test-structure
                  (eval '(define-structure define-structure-test-structure))))

  (check (eq? 'define-structure-test-structure
                  (eval '(define-structure define-structure-test-structure slot-1 slot-2 slot-3))))

  (check (runtime-error? (eval '(define-structure :keyword-type-name x y z))))
  (check (runtime-error? (eval '(define-structure #.(gensym "uninterned-sym") x y z))))
  (check (runtime-error? (eval '(define-structure 12 x y z))))
  (check (runtime-error? (eval '(define-structure #\a x y z))))

  (check (runtime-error? (eval '(define-structure test-struct 12 x y z))))
  (check (runtime-error? (eval '(define-structure "test-struct" x "string" y z))))

  (check (not (runtime-error? (eval '(define-structure test-struct x y z)))))
  (check (not (runtime-error? (eval '(define-structure test-struct "docs" x y z)))))

  (check (not (runtime-error? (eval '(define-structure test-market-structure)))))

  (check (runtime-error? (eval '(define-structure structure-name
                                     (slot :set)))))

  (check (runtime-error? (eval '(define-structure structure-name
                                     (slot :invalid-attr 12)))))

  (check (runtime-error? (eval '(define-structure structure-name
                                     (slot :set 12)))))

  (check (runtime-error? (eval '(define-structure structure-name
                                     (slot :get 12)))))

  (check (runtime-error? (eval '(define-structure test-structure
                                      duplicate-slot
                                      duplicate-slot))))

  (check (runtime-error? (eval '(define-structure test-structure
                                      x
                                      duplicate-slot
                                      duplicate-slot))))
  
  (check (runtime-error? (eval '(define-structure test-structure
                                      duplicate-slot
                                      duplicate-slot
                                      x))))

  (check (runtime-error? (eval '(define-structure test-structure
                                      x
                                      duplicate-slot
                                      y
                                      z
                                      duplicate-slot)))))

(define-test structure-meta
  (check (set-same? (structure-slots 'ship) '(:x :y :dx :dy)))
  (check (set-same? (structure-slots 'empty-structure) '()))

  (check (set-same? (structure-slots (make-ship)) '(:x :y :dx :dy)))
  (check (set-same? (structure-slots (make-empty-structure)) '()))

  (check (ship? (make-structure-by-name 'ship)))
  (check (empty-structure? (make-structure-by-name 'empty-structure)))

  (check (runtime-error? (structure-has-slot? 12 :foo)))
  (check (runtime-error? (structure-has-slot? 'ship 12)))
  (check (runtime-error? (structure-has-slot? 'ship 'x)))

  (check (structure-has-slot? 'ship :x))
  (check (structure-has-slot? 'ship :y))
  (check (structure-has-slot? 'ship :dx))
  (check (structure-has-slot? 'ship :dy))
  (check (not (structure-has-slot? 'ship :foo)))

  (let ((s1 (make-ship)))
    (slot-set! s1 :x 3)
    (slot-set! s1 :y 4)
    (slot-set! s1 :dx 1)
    (slot-set! s1 :dy -2)

    (check (runtime-error? (slot-set! s1 :bad-slot 12)))
    (check (runtime-error? (slot-set! s1 23 12)))

    (check (eq? (ship-x s1) 3))
    (check (eq? (ship-y s1) 4))
    (check (eq? (ship-dx s1) 1))
    (check (eq? (ship-dy s1) -2)))


  (let ((s1 (make-ship :x 3 :y 4 :dx 1 :dy -2)))
    (check (runtime-error? (slot-set! 'ship :x 12)))
    (check (runtime-error? (slot-set! "ship" :x 12)))
    (check (runtime-error? (slot-set! s1 :bad-slot 12)))
    (check (runtime-error? (slot-set! s1 23 12)))

    (check (eq? (ship-x s1) 3))
    (check (eq? (ship-y s1) 4))
    (check (eq? (ship-dx s1) 1))
    (check (eq? (ship-dy s1) -2))))
