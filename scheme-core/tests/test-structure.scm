(use-package! "unit-test")

(define-structure ship x y dx dy)
(define-structure ship-2 x y dx dy)

(define-structure %int-test foo)

(define (symbol-bound-to-procedure? sym)
  (check symbol? sym)
  (and (symbol-bound? sym)
       (procedure? (symbol-value sym))))

(define-test structure-slots
  (test-case (runtime-error? (structure-slots 12)))
  (test-case (equal? '(:x :y :dx :dy) (structure-slots 'ship)))
  (test-case (equal? '(:x :y :dx :dy) (structure-slots (make-ship)))))

(define-test structure-accessors
  ;; All of these should exist
  (test-case (symbol-bound-to-procedure? 'make-ship))
  (test-case (symbol-bound-to-procedure? 'copy-ship))
  (test-case (symbol-bound-to-procedure? 'ship?))

  (test-case (symbol-bound-to-procedure? 'set-ship-x!))
  (test-case (symbol-bound-to-procedure? 'set-ship-y!))
  (test-case (symbol-bound-to-procedure? 'set-ship-dx!))
  (test-case (symbol-bound-to-procedure? 'set-ship-dy!))

  (test-case (symbol-bound-to-procedure? 'ship-x))
  (test-case (symbol-bound-to-procedure? 'ship-y))
  (test-case (symbol-bound-to-procedure? 'ship-dx))
  (test-case (symbol-bound-to-procedure? 'ship-dy))

  ;; test naming of accessors for internal structures
  (test-case (symbol-bound-to-procedure? '%make-int-test))
  (test-case (symbol-bound-to-procedure? '%copy-int-test))
  (test-case (symbol-bound-to-procedure? '%int-test?))

  (test-case (symbol-bound-to-procedure? '%set-int-test-foo!))
  (test-case (symbol-bound-to-procedure? '%int-test-foo))

  ;; Type predicates
  (test-case (ship? (make-ship)))
  (test-case (ship-2? (make-ship-2)))

  (test-case (not (ship? (make-ship-2))))
  (test-case (not (ship-2? (make-ship))))

  (test-case (not (ship? 12)))
  (test-case (not (ship? 'ship)))

  (let ((ts (make-ship)))
    (test-case (eq? (ship-x ts) ()))
    (test-case (eq? (ship-y ts) ()))
    (test-case (eq? (ship-dx ts) ()))
    (test-case (eq? (ship-dx ts) ()))

    (set-ship-x! ts :foo)

    (test-case (eq? (ship-x ts) :foo))
    (test-case (eq? (ship-y ts) ()))
    (test-case (eq? (ship-dx ts) ()))
    (test-case (eq? (ship-dx ts) ()))

    (set-ship-y! ts :bar)
    (set-ship-dx! ts :baz)
    (set-ship-dy! ts :blo)

    (test-case (eq? (ship-x ts) :foo))
    (test-case (eq? (ship-y ts) :bar))
    (test-case (eq? (ship-dx ts) :baz))
    (test-case (eq? (ship-dy ts) :blo))

    (let ((ts2 (make-ship)))
      (test-case (eq? (ship-x ts2) ()))
      (test-case (eq? (ship-y ts2) ()))
      (test-case (eq? (ship-dx ts2) ()))
      (test-case (eq? (ship-dx ts2) ())))

    ;; TODO: test copy-ship on non-ship structure
    (test-case (runtime-error? (copy-ship :not-a-structure)))

    (let ((tsc (copy-ship ts)))
      (test-case (not (eq? ts tsc)))
      (test-case (ship? tsc))

      (test-case (eq? (ship-x tsc) :foo))
      (test-case (eq? (ship-y tsc) :bar))
      (test-case (eq? (ship-dx tsc) :baz))
      (test-case (eq? (ship-dy tsc) :blo))
      ))

  (test-case (runtime-error? (set-ship-x! :not-a-stucture 12)))
  (test-case (runtime-error? (ship-x :not-a-stucture)))

  (let ((ts (make-ship)))
    (test-case (runtime-error? (set-ship-2-x! ts 0)))
    (test-case (runtime-error? (set-ship-2-y! ts 0)))
    (test-case (runtime-error? (set-ship-2-dx! ts 0)))
    (test-case (runtime-error? (set-ship-2-dy! ts 0)))
    ;(test-case (runtime-error? (copy-ship-2 ts)))
    )

  (let ((ts2 (make-ship :x 2 :y 4 :dx -3.4 :dy :stationary)))
    (test-case (equal? (ship-x ts2) 2))
    (test-case (equal? (ship-y ts2) 4))
    (test-case (equal? (ship-dx ts2) -3.4))
    (test-case (equal? (ship-dy ts2) :stationary)))

  (test-case (runtime-error? (make-ship :bad-slot 12)))
  )

(define-test structure-equal
  (let ((ts-a (make-ship))
        (ts-b (make-ship))
        (ts2 (make-ship-2)))

    (test-case (not (equal? ts-a ts2)))
    (test-case (not (equal? ts-a 'symbol)))
    (test-case (not (equal? ts-a 12)))

    (test-case (equal? ts-a ts-b))

    (set-ship-x! ts-a 12)
    (test-case (not (equal? ts-a ts-b)))
    (set-ship-x! ts-b 12)

    (test-case (equal? ts-a ts-b))

    (set-ship-y! ts-a :foo)
    (test-case (not (equal? ts-a ts-b)))
    (set-ship-y! ts-b :foo)

    (test-case (equal? ts-a ts-b))

    (set-ship-dx! ts-a '(1 2 3))
    (test-case (not (equal? ts-a ts-b)))
    (set-ship-dx! ts-b '(1 2 3))

    (test-case (equal? ts-a ts-b))

    (set-ship-dy! ts-a ts2)
    (test-case (not (equal? ts-a ts-b)))
    (set-ship-dy! ts-b ts2)

    (test-case (equal? ts-a ts-b))
    ))

(define-test structure-sxhash
  (let ((ts-a (make-ship))
        (ts-b (make-ship))
        (ts2 (make-ship-2)))

    (test-case (not (= (sxhash ts-a) (sxhash ts2))))

    (test-case (= (sxhash ts-a) (sxhash ts-b)))

    (set-ship-x! ts-a 12)
    (test-case (not (= (sxhash ts-a) (sxhash ts-b))))
    (set-ship-x! ts-b 12)

    (test-case (= (sxhash ts-a) (sxhash ts-b)))

    (set-ship-y! ts-a :foo)
    (test-case (not (= (sxhash ts-a) (sxhash ts-b))))
    (set-ship-y! ts-b :foo)

    (test-case (= (sxhash ts-a) (sxhash ts-b)))

    (set-ship-dx! ts-a '(1 2 3))
    (test-case (not (= (sxhash ts-a) (sxhash ts-b))))
    (set-ship-dx! ts-b '(1 2 3))

    (test-case (= (sxhash ts-a) (sxhash ts-b)))

    (set-ship-dy! ts-a ts2)
    (test-case (not (= (sxhash ts-a) (sxhash ts-b))))
    (set-ship-dy! ts-b ts2)

    (test-case (= (sxhash ts-a) (sxhash ts-b)))
    ))


(define-structure empty-structure)

(define-test structure-empty
  ;; All of these should exist
  (test-case (symbol-bound-to-procedure? 'make-empty-structure))
  (test-case (symbol-bound-to-procedure? 'copy-empty-structure))
  (test-case (symbol-bound-to-procedure? 'empty-structure?))

  (test-case (empty-structure? (make-empty-structure))))

(define-structure ship-defaults
  (x :default 3)
  (y :default 4.0)
  (dx :default #\a)
  (dy :default :foo))

(define-test structure-simple-defaults
  (test-case (symbol-bound-to-procedure? 'make-ship-defaults))
  (test-case (symbol-bound-to-procedure? 'copy-ship-defaults))
  (test-case (symbol-bound-to-procedure? 'ship-defaults?))

  (test-case (symbol-bound-to-procedure? 'set-ship-defaults-x!))
  (test-case (symbol-bound-to-procedure? 'set-ship-defaults-y!))
  (test-case (symbol-bound-to-procedure? 'set-ship-defaults-dx!))
  (test-case (symbol-bound-to-procedure? 'set-ship-defaults-dy!))

  (test-case (symbol-bound-to-procedure? 'ship-defaults-x))
  (test-case (symbol-bound-to-procedure? 'ship-defaults-y))
  (test-case (symbol-bound-to-procedure? 'ship-defaults-dx))
  (test-case (symbol-bound-to-procedure? 'ship-defaults-dy))

  (let ((ts (make-ship-defaults)))
    (test-case (equal? (ship-defaults-x ts) 3))
    (test-case (equal? (ship-defaults-y ts) 4.0))
    (test-case (equal? (ship-defaults-dx ts) #\a))
    (test-case (equal? (ship-defaults-dy ts) :foo))

    (set-ship-defaults-x! ts :foo)
    (set-ship-defaults-y! ts :bar)
    (set-ship-defaults-dx! ts :baz)
    (set-ship-defaults-dy! ts :blo)

    (let ((ts2 (make-ship-defaults)))
      (test-case (equal? (ship-defaults-x ts2) 3))
      (test-case (equal? (ship-defaults-y ts2) 4.0))
      (test-case (equal? (ship-defaults-dx ts2) #\a))
      (test-case (equal? (ship-defaults-dy ts2) :foo)))))

(define-structure default-tester
  (s-1 :default (begin (checkpoint 1) :foo))
  (s-2 :default (begin (checkpoint 2) :bar))
  (s-3 :default (begin (checkpoint 3) :baz)))

(define-test structure-evaluated-defaults
  (test-case/execution-order (1 2 3)
    (let ((dt (make-default-tester)))
      (test-case (eq? (default-tester-s-1 dt) :foo))
      (test-case (eq? (default-tester-s-2 dt) :bar))
      (test-case (eq? (default-tester-s-3 dt) :baz))))

  (test-case/execution-order (1 2 3)
    (let ((dt (make-structure-by-name 'default-tester)))
      (test-case (eq? (default-tester-s-1 dt) :foo))
      (test-case (eq? (default-tester-s-2 dt) :bar))
      (test-case (eq? (default-tester-s-3 dt) :baz))))

  (test-case/execution-order (1 2 3)
    (let ((dt (read-from-string "#S(default-tester)")))
      (test-case (eq? (default-tester-s-1 dt) :foo))
      (test-case (eq? (default-tester-s-2 dt) :bar))
      (test-case (eq? (default-tester-s-3 dt) :baz))))

  (test-case/execution-order ()
    (let ((dt (make-default-tester :s-1 1 :s-2 2 :s-3 3)))
      (test-case (eq? (default-tester-s-1 dt) 1))
      (test-case (eq? (default-tester-s-2 dt) 2))
      (test-case (eq? (default-tester-s-3 dt) 3))))

  (test-case/execution-order ()
    (let ((dt (make-structure-by-name 'default-tester :s-1 1 :s-2 2 :s-3 3)))
      (test-case (eq? (default-tester-s-1 dt) 1))
      (test-case (eq? (default-tester-s-2 dt) 2))
      (test-case (eq? (default-tester-s-3 dt) 3))))

  (test-case/execution-order ()
    (let ((dt (read-from-string "#S(default-tester :s-1 1 :s-2 2 :s-3 3)")))
      (test-case (eq? (default-tester-s-1 dt) 1))
      (test-case (eq? (default-tester-s-2 dt) 2))
      (test-case (eq? (default-tester-s-3 dt) 3))))
  )

(define-structure ship-fancy
  (x :default 3 :get "westing")
  (y :default 4.0 :set #f)
  (z :default 4 :get "northing" :set "raise-the-mainsail"))

(define-test test-structure-fancy
  (test-case (symbol-bound-to-procedure? 'make-ship-fancy))
  (test-case (symbol-bound-to-procedure? 'copy-ship-fancy))
  (test-case (symbol-bound-to-procedure? 'ship-fancy?))

  (test-case (symbol-bound-to-procedure? 'set-ship-fancy-x!))
  (test-case (symbol-bound-to-procedure? 'raise-the-mainsail))

  (test-case (symbol-bound-to-procedure? 'westing))
  (test-case (symbol-bound-to-procedure? 'ship-fancy-y))
  (test-case (symbol-bound-to-procedure? 'northing)))

(define-test structure-io
  (test-case (can-read/write-round-trip? (make-ship)))
  (test-case (can-read/write-round-trip? (make-empty-structure)))

  (let ((ts (make-ship)))
    (set-ship-x! ts 12)
    (set-ship-y! ts (make-ship))
    (test-case (can-read/write-round-trip? ts)))

  (let ((ts (read-from-string "#S(ship)")))
    (test-case (ship? ts))
    (test-case (eq? () (ship-x ts)))
    (test-case (eq? () (ship-y ts))))

  (let ((ts (read-from-string "#S(ship :x 3)")))
    (test-case (ship? ts))
    (test-case (eq? 3 (ship-x ts)))
    (test-case (eq? () (ship-y ts))))

  (let ((ts (read-from-string "#S(ship :x 3 :y 4)")))
    (test-case (ship? ts))
    (test-case (eq? 3 (ship-x ts)))
    (test-case (eq? 4 (ship-y ts))))

  (test-case (runtime-error? (read-from-string "#S(bad-structure-type)")))
  (test-case (runtime-error? (read-from-string "#S(12)")))
  (test-case (runtime-error? (read-from-string "#S()")))
  (test-case (runtime-error? (read-from-string "#S(ship :bad-slot 12)")))
  (test-case (runtime-error? (read-from-string "#S(ship :x 3 :bad-slot 12)")))
  (test-case (runtime-error? (read-from-string "#S(ship :bad-slot 12 :y 5)")))
  )

(define-test structure-fast-io
  (test-case (can-fast-io-round-trip? (make-ship)))
  (test-case (can-fast-io-round-trip? (make-empty-structure)))
  (test-case (can-fast-io-round-trip? (make-ship :x 3 :y 4)))

  (test-case (can-fast-io-round-trip? (make-ship :x 3 :y (make-ship :x 12 :y 23))))

  (let ((st (make-ship :x 12)))
    (set-ship-y! st st))

  (let ((st (make-ship)))
    (set-ship-x! st st)
    (set-ship-y! st st)))

(define-test define-structure-errors
  (test-case (eq? 'define-structure-test-structure
                  (eval '(define-structure define-structure-test-structure))))

  (test-case (eq? 'define-structure-test-structure
                  (eval '(define-structure define-structure-test-structure slot-1 slot-2 slot-3))))

  (test-case (runtime-error? (eval '(define-structure :keyword-type-name x y z))))
  (test-case (runtime-error? (eval '(define-structure #.(gensym "uninterned-sym") x y z))))
  (test-case (runtime-error? (eval '(define-structure 12 x y z))))
  (test-case (runtime-error? (eval '(define-structure #\a x y z))))

  (test-case (runtime-error? (eval '(define-structure test-struct 12 x y z))))
  (test-case (runtime-error? (eval '(define-structure "test-struct" x "string" y z))))

  (test-case (not (runtime-error? (eval '(define-structure test-struct x y z)))))
  (test-case (not (runtime-error? (eval '(define-structure test-struct "docs" x y z)))))

  (test-case (not (runtime-error? (eval '(define-structure test-market-structure)))))

  (test-case (runtime-error? (eval '(define-structure structure-name
                                     (slot :set)))))

  (test-case (runtime-error? (eval '(define-structure structure-name
                                     (slot :invalid-attr 12)))))

  (test-case (runtime-error? (eval '(define-structure structure-name
                                     (slot :set 12)))))

  (test-case (runtime-error? (eval '(define-structure structure-name
                                     (slot :get 12)))))

  (test-case (runtime-error? (eval '(define-structure test-structure
                                     duplicate-slot
                                     duplicate-slot))))

  (test-case (runtime-error? (eval '(define-structure test-structure
                                     x
                                     duplicate-slot
                                     duplicate-slot))))

  (test-case (runtime-error? (eval '(define-structure test-structure
                                     duplicate-slot
                                     duplicate-slot
                                     x))))

  (test-case (runtime-error? (eval '(define-structure test-structure
                                     x
                                     duplicate-slot
                                     y
                                     z
                                     duplicate-slot))))
  )



;(define-structure ship x y dx dy)
;(define-structure empty-structure)

(define-test structure-meta
  (test-case (equal? (structure-slots 'ship) '(:x :y :dx :dy)))
  (test-case (equal? (structure-slots 'empty-structure) '()))

  (test-case (equal? (structure-slots (make-ship)) '(:x :y :dx :dy)))
  (test-case (equal? (structure-slots (make-empty-structure)) '()))

  (test-case (ship? (make-structure-by-name 'ship)))
  (test-case (empty-structure? (make-structure-by-name 'empty-structure)))

  (test-case (runtime-error? (structure-has-slot? 12 :foo)))
  (test-case (runtime-error? (structure-has-slot? 'ship 12)))
  (test-case (runtime-error? (structure-has-slot? 'ship 'x)))

  (test-case (structure-has-slot? 'ship :x))
  (test-case (structure-has-slot? 'ship :y))
  (test-case (structure-has-slot? 'ship :dx))
  (test-case (structure-has-slot? 'ship :dy))
  (test-case (not (structure-has-slot? 'ship :foo)))

  (let ((s1 (make-ship)))
    (test-case (structure-has-slot? s1 :x))
    (test-case (structure-has-slot? s1 :y))
    (test-case (structure-has-slot? s1 :dx))
    (test-case (structure-has-slot? s1 :dy))
    (test-case (not (structure-has-slot? s1 :foo)))

    (set-structure-slot-by-name! s1 :x 3)
    (set-structure-slot-by-name! s1 :y 4)
    (set-structure-slot-by-name! s1 :dx 1)
    (set-structure-slot-by-name! s1 :dy -2)

    (test-case (runtime-error? (set-structure-slot-by-name! s1 :bad-slot 12)))
    (test-case (runtime-error? (set-structure-slot-by-name! s1 23 12)))

    (test-case (eq? (ship-x s1) 3))
    (test-case (eq? (ship-y s1) 4))
    (test-case (eq? (ship-dx s1) 1))
    (test-case (eq? (ship-dy s1) -2)))


  (let ((s1 (make-ship)))
    (slot-set! s1 :x 3)
    (slot-set! s1 :y 4)
    (slot-set! s1 :dx 1)
    (slot-set! s1 :dy -2)

    (test-case (runtime-error? (slot-set! 'ship :x 12)))
    (test-case (runtime-error? (slot-set! "ship" :x 12)))
    (test-case (runtime-error? (slot-set! s1 :bad-slot 12)))
    (test-case (runtime-error? (slot-set! s1 23 12)))

    (test-case (eq? (ship-x s1) 3))
    (test-case (eq? (ship-y s1) 4))
    (test-case (eq? (ship-dx s1) 1))
    (test-case (eq? (ship-dy s1) -2)))

  (let ((s1 (make-ship)))
    (set-ship-x! s1 3)
    (set-ship-y! s1 4)
    (set-ship-dx! s1 1)
    (set-ship-dy! s1 -2)

    (test-case (runtime-error? (structure-slot-by-name s1 :bad-slot)))
    (test-case (runtime-error? (structure-slot-by-name s1 23)))
    (test-case (runtime-error? (structure-slot-by-name 'ship :x)))
    (test-case (runtime-error? (structure-slot-by-name "ship" :x)))

    (test-case (eq? (structure-slot-by-name s1 :x) 3))
    (test-case (eq? (structure-slot-by-name s1 :y) 4))
    (test-case (eq? (structure-slot-by-name s1 :dx) 1))
    (test-case (eq? (structure-slot-by-name s1 :dy) -2))

    (test-case (eq? #f (slot-ref s1 :bad-slot)))
    (test-case (eq? :xyzzy (slot-ref s1 :bad-slot :xyzzy)))
    (test-case (runtime-error? (slot-ref s1 23)))
    (test-case (runtime-error? (slot-ref 'ship :x)))
    (test-case (runtime-error? (slot-ref "ship" :x)))

    (test-case (eq? (slot-ref s1 :x) 3))
    (test-case (eq? (slot-ref s1 :y) 4))
    (test-case (eq? (slot-ref s1 :dx) 1))
    (test-case (eq? (slot-ref s1 :dy) -2))
    ))



