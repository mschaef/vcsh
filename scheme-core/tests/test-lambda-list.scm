(define-package "test-lambda-list"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define (def-tll0-args)                (vector :def-tll0-args))
(define (def-tll0-args/r . r)          (vector :def-tll0-args/r r))
(define (def-tll1-arg a1)              (vector :def-tll1-arg a1))
(define (def-tll1-arg/r a1 . r)        (vector :def-tll1-arg/r a1 r))
(define (def-tll3-args a1 a2 a3)       (vector :def-tll3-args a1 a2 a3))
(define (def-tll3-args/r a1 a2 a3 . r) (vector :def-tll3-args/r a1 a2 a3 r))

(define (def-tll1-arg/mutable a1)
  (set! a1 :mutable-a1)
  (vector :def-tll1-arg/mutable a1))

(define (def-tll3-args/mutable-1 a1 a2 a3)
  (set! a1 :mutable-a1)
  (vector :def-tll3-args/mutable-1 a1 a2 a3))

(define (def-tll3-args/mutable-3 a1 a2 a3)
  (set! a3 :mutable-a3)
  (vector :def-tll3-args/mutable-3 a1 a2 a3))

(define-test lambda-list/plain-define
  (check (eq? (procedure-name def-tll0-args)   'def-tll0-args))
  (check (equal? (def-tll0-args) (vector :def-tll0-args)))
  (check (equal? (def-tll0-args 1) (vector :def-tll0-args)))
  (check (equal? (def-tll0-args 1 2 3) (vector :def-tll0-args)))

  (check (eq? (procedure-name def-tll0-args/r)   'def-tll0-args/r))
  (check (equal? (def-tll0-args/r) (vector :def-tll0-args/r '())))
  (check (equal? (def-tll0-args/r 1) (vector :def-tll0-args/r '(1))))
  (check (equal? (def-tll0-args/r 1 2 3) (vector :def-tll0-args/r '(1 2 3))))

  (check (eq? (procedure-name def-tll1-arg) 'def-tll1-arg))
  (check (runtime-error? (def-tll1-arg)))
  (check (equal? (def-tll1-arg 1) (vector :def-tll1-arg 1)))
  (check (equal? (def-tll1-arg 1 2 3) (vector :def-tll1-arg 1)))

  (check (eq? (procedure-name def-tll1-arg/r)  'def-tll1-arg/r))
  (check (runtime-error? (def-tll1-arg/r)))
  (check (equal? (def-tll1-arg/r 1) (vector :def-tll1-arg/r 1 ())))
  (check (equal? (def-tll1-arg/r 1 2 3) (vector :def-tll1-arg/r 1 '(2 3))))

  (check (eq? (procedure-name def-tll3-args)   'def-tll3-args))
  (check (runtime-error? (def-tll3-args)))
  (check (runtime-error? (def-tll3-args 1)))
  (check (equal? (def-tll3-args 1 2 3) (vector :def-tll3-args 1 2 3)))
  (check (equal? (def-tll3-args 1 2 3 4 5 6) (vector :def-tll3-args 1 2 3)))

  (check (eq? (procedure-name def-tll3-args/r)  'def-tll3-args/r))
  (check (runtime-error? (def-tll3-args/r)))
  (check (runtime-error? (def-tll3-args/r 1)))
  (check (equal? (def-tll3-args/r 1 2 3)
                 (vector :def-tll3-args/r 1 2 3 ())))
  (check (equal? (def-tll3-args/r 1 2 3 4 5 6)
                 (vector :def-tll3-args/r 1 2 3 '(4 5 6))))

  (check (equal? (def-tll1-arg/mutable 12)
                 (vector :def-tll1-arg/mutable :mutable-a1)))
  
  (check (equal? (def-tll3-args/mutable-1 1 2 3)
                 (vector :def-tll3-args/mutable-1 :mutable-a1 2 3)))
  
  (check (equal? (def-tll3-args/mutable-3 1 2 3)
                 (vector :def-tll3-args/mutable-3 1 2 :mutable-a3))))

(define-test lambda-list/plain-local-define
  (define (ldef-tll0-args)                (vector :ldef-tll0-args))
  (define (ldef-tll0-args/r . r)          (vector :ldef-tll0-args/r r))
  (define (ldef-tll1-arg a1)              (vector :ldef-tll1-arg a1))
  (define (ldef-tll1-arg/r a1 . r)        (vector :ldef-tll1-arg/r a1 r))
  (define (ldef-tll3-args a1 a2 a3)       (vector :ldef-tll3-args a1 a2 a3))
  (define (ldef-tll3-args/r a1 a2 a3 . r) (vector :ldef-tll3-args/r a1 a2 a3 r))


  (define (ldef-tll1-arg/mutable a1)
    (set! a1 :mutable-a1)
    (vector :ldef-tll1-arg/mutable a1))

  (define (ldef-tll3-args/mutable-1 a1 a2 a3)
    (set! a1 :mutable-a1)
    (vector :ldef-tll3-args/mutable-1 a1 a2 a3))

  (define (ldef-tll3-args/mutable-3 a1 a2 a3)
    (set! a3 :mutable-a3)
    (vector :ldef-tll3-args/mutable-3 a1 a2 a3))

  (check (eq? (procedure-name ldef-tll0-args)   'ldef-tll0-args))
  (check (equal? (ldef-tll0-args) (vector :ldef-tll0-args)))
  (check (equal? (ldef-tll0-args 1) (vector :ldef-tll0-args)))
  (check (equal? (ldef-tll0-args 1 2 3) (vector :ldef-tll0-args)))

  (check (eq? (procedure-name ldef-tll0-args/r)   'ldef-tll0-args/r))
  (check (equal? (ldef-tll0-args/r) (vector :ldef-tll0-args/r '())))
  (check (equal? (ldef-tll0-args/r 1) (vector :ldef-tll0-args/r '(1))))
  (check (equal? (ldef-tll0-args/r 1 2 3) (vector :ldef-tll0-args/r '(1 2 3))))

  (check (eq? (procedure-name ldef-tll1-arg) 'ldef-tll1-arg))
  (check (runtime-error? (ldef-tll1-arg)))
  (check (equal? (ldef-tll1-arg 1) (vector :ldef-tll1-arg 1)))
  (check (equal? (ldef-tll1-arg 1 2 3) (vector :ldef-tll1-arg 1)))

  (check (eq? (procedure-name ldef-tll1-arg/r)  'ldef-tll1-arg/r))
  (check (runtime-error? (ldef-tll1-arg/r)))
  (check (equal? (ldef-tll1-arg/r 1) (vector :ldef-tll1-arg/r 1 ())))
  (check (equal? (ldef-tll1-arg/r 1 2 3) (vector :ldef-tll1-arg/r 1 '(2 3))))

  (check (eq? (procedure-name ldef-tll3-args)   'ldef-tll3-args))
  (check (runtime-error? (ldef-tll3-args)))
  (check (runtime-error? (ldef-tll3-args 1)))
  (check (equal? (ldef-tll3-args 1 2 3) (vector :ldef-tll3-args 1 2 3)))
  (check (equal? (ldef-tll3-args 1 2 3 4 5 6) (vector :ldef-tll3-args 1 2 3)))

  (check (eq? (procedure-name ldef-tll3-args/r)  'ldef-tll3-args/r))
  (check (runtime-error? (ldef-tll3-args/r)))
  (check (runtime-error? (ldef-tll3-args/r 1)))
  (check (equal? (ldef-tll3-args/r 1 2 3)
                     (vector :ldef-tll3-args/r 1 2 3 ())))
  (check (equal? (ldef-tll3-args/r 1 2 3 4 5 6)
                     (vector :ldef-tll3-args/r 1 2 3 '(4 5 6))))

  (check (equal? (ldef-tll1-arg/mutable 12)
                     (vector :ldef-tll1-arg/mutable :mutable-a1)))

  (check (equal? (ldef-tll3-args/mutable-1 1 2 3)
                     (vector :ldef-tll3-args/mutable-1 :mutable-a1 2 3)))

  (check (equal? (ldef-tll3-args/mutable-3 1 2 3)
                     (vector :ldef-tll3-args/mutable-3 1 2 :mutable-a3))))

(define lam-tll0-args   (lambda ()             (vector :lam-tll0-args)))
(define lam-tll0-args/r (lambda r              (vector :lam-tll0-args/r r)))
(define lam-tll1-arg    (lambda (a1)           (vector :lam-tll1-arg a1)))
(define lam-tll1-arg/r  (lambda (a1 . r)       (vector :lam-tll1-arg/r a1 r)))
(define lam-tll3-args   (lambda (a1 a2 a3)     (vector :lam-tll3-args a1 a2 a3)))
(define lam-tll3-args/r (lambda (a1 a2 a3 . r) (vector :lam-tll3-args/r a1 a2 a3 r)))

(define-test lambda-list/plain-lambda
  (check (eq? (procedure-name lam-tll0-args)  #f))
  (check (equal? (lam-tll0-args) (vector :lam-tll0-args)))
  (check (equal? (lam-tll0-args 1) (vector :lam-tll0-args)))
  (check (equal? (lam-tll0-args 1 2 3) (vector :lam-tll0-args)))

  (check (eq? (procedure-name lam-tll0-args/r) #f))
  (check (equal? (lam-tll0-args/r) (vector :lam-tll0-args/r '())))
  (check (equal? (lam-tll0-args/r 1) (vector :lam-tll0-args/r '(1))))
  (check (equal? (lam-tll0-args/r 1 2 3) (vector :lam-tll0-args/r '(1 2 3))))

  (check (eq? (procedure-name lam-tll1-arg) #f))
  (check (runtime-error? (lam-tll1-arg)))
  (check (equal? (lam-tll1-arg 1) (vector :lam-tll1-arg 1)))
  (check (equal? (lam-tll1-arg 1 2 3) (vector :lam-tll1-arg 1)))

  (check (eq? (procedure-name lam-tll1-arg/r)  #f))
  (check (runtime-error? (lam-tll1-arg/r)))
  (check (equal? (lam-tll1-arg/r 1) (vector :lam-tll1-arg/r 1 ())))
  (check (equal? (lam-tll1-arg/r 1 2 3) (vector :lam-tll1-arg/r 1 '(2 3))))

  (check (eq? (procedure-name lam-tll3-args) #f))
  (check (runtime-error? (lam-tll3-args)))
  (check (runtime-error? (lam-tll3-args 1)))
  (check (equal? (lam-tll3-args 1 2 3) (vector :lam-tll3-args 1 2 3)))
  (check (equal? (lam-tll3-args 1 2 3 4 5 6) (vector :lam-tll3-args 1 2 3)))

  (check (eq? (procedure-name lam-tll3-args/r) #f))
  (check (runtime-error? (lam-tll3-args/r)))
  (check (runtime-error? (lam-tll3-args/r 1)))
  (check (equal? (lam-tll3-args/r 1 2 3)
                 (vector :lam-tll3-args/r 1 2 3 ())))
  (check (equal? (lam-tll3-args/r 1 2 3 4 5 6)
                 (vector :lam-tll3-args/r 1 2 3 '(4 5 6)))))
 
(define (def-tll1-arg/o :optional o1)  (vector :def-tll1-arg/o o1))
(define (def-tll2-args/o a1 :optional o1)  (vector :def-tll2-args/o a1 o1))
(define (def-tll3-args/o a1 a2 :optional o1)  (vector :def-tll3-args/o a1 a2 o1))
(define (def-tll4-args/o a1 a2 :optional o1 o2)  (vector :def-tll4-args/o a1 a2 o1 o2))

(define (def-tll1-arg/or :optional o1 . r)  (vector :def-tll1-arg/or o1 r))
(define (def-tll2-args/or a1 :optional o1 . r)  (vector :def-tll2-args/or a1 o1 r))
(define (def-tll3-args/or a1 a2 :optional o1 . r)  (vector :def-tll3-args/or a1 a2 o1 r))
(define (def-tll4-args/or a1 a2 :optional o1 o2 . r)  (vector :def-tll4-args/or a1 a2 o1 o2 r))

(define-test lambda-list/no-default-optionals-define
  (check (eq? (procedure-name def-tll1-arg/o) 'def-tll1-arg/o))
  (check (equal? (def-tll1-arg/o) (vector :def-tll1-arg/o ())))
  (check (equal? (def-tll1-arg/o 1) (vector :def-tll1-arg/o 1)))
  (check (equal? (def-tll1-arg/o 1 2) (vector :def-tll1-arg/o 1)))

  (check (eq? (procedure-name def-tll2-args/o) 'def-tll2-args/o))
  (check (runtime-error? (def-tll2-args/o)))
  (check (equal? (def-tll2-args/o 1) (vector :def-tll2-args/o 1 ())))
  (check (equal? (def-tll2-args/o 1 2) (vector :def-tll2-args/o 1 2)))
  (check (equal? (def-tll2-args/o 1 2 3) (vector :def-tll2-args/o 1 2)))

  (check (eq? (procedure-name def-tll3-args/o) 'def-tll3-args/o))
  (check (runtime-error? (def-tll3-args/o)))
  (check (runtime-error? (def-tll3-args/o 1)))
  (check (equal? (def-tll3-args/o 1 2) (vector :def-tll3-args/o 1 2 ())))
  (check (equal? (def-tll3-args/o 1 2 3) (vector :def-tll3-args/o 1 2 3)))
  (check (equal? (def-tll3-args/o 1 2 3 4) (vector :def-tll3-args/o 1 2 3)))

  (check (eq? (procedure-name def-tll4-args/o) 'def-tll4-args/o))
  (check (runtime-error? (def-tll4-args/o)))
  (check (runtime-error? (def-tll4-args/o 1)))
  (check (equal? (def-tll4-args/o 1 2) (vector :def-tll4-args/o 1 2 () ())))
  (check (equal? (def-tll4-args/o 1 2 3) (vector :def-tll4-args/o 1 2 3 ())))
  (check (equal? (def-tll4-args/o 1 2 3 4) (vector :def-tll4-args/o 1 2 3 4)))
  (check (equal? (def-tll4-args/o 1 2 3 4 5) (vector :def-tll4-args/o 1 2 3 4)))


  (check (eq? (procedure-name def-tll1-arg/or) 'def-tll1-arg/or))
  (check (equal? (def-tll1-arg/or) (vector :def-tll1-arg/or () ())))
  (check (equal? (def-tll1-arg/or 1) (vector :def-tll1-arg/or 1 ())))
  (check (equal? (def-tll1-arg/or 1 2) (vector :def-tll1-arg/or 1 '(2))))

  (check (eq? (procedure-name def-tll2-args/or) 'def-tll2-args/or))
  (check (runtime-error? (def-tll2-args/or)))
  (check (equal? (def-tll2-args/or 1) (vector :def-tll2-args/or 1 () ())))
  (check (equal? (def-tll2-args/or 1 2) (vector :def-tll2-args/or 1 2 ())))
  (check (equal? (def-tll2-args/or 1 2 3) (vector :def-tll2-args/or 1 2 '(3))))

  (check (eq? (procedure-name def-tll3-args/or) 'def-tll3-args/or))
  (check (runtime-error? (def-tll3-args/or)))
  (check (runtime-error? (def-tll3-args/or 1)))
  (check (equal? (def-tll3-args/or 1 2) (vector :def-tll3-args/or 1 2 () ())))
  (check (equal? (def-tll3-args/or 1 2 3) (vector :def-tll3-args/or 1 2 3 ())))
  (check (equal? (def-tll3-args/or 1 2 3 4) (vector :def-tll3-args/or 1 2 3 '(4))))

  (check (eq? (procedure-name def-tll4-args/or) 'def-tll4-args/or))
  (check (runtime-error? (def-tll4-args/or)))
  (check (runtime-error? (def-tll4-args/or 1)))
  (check (equal? (def-tll4-args/or 1 2) (vector :def-tll4-args/or 1 2 () () ())))
  (check (equal? (def-tll4-args/or 1 2 3) (vector :def-tll4-args/or 1 2 3 () ())))
  (check (equal? (def-tll4-args/or 1 2 3 4) (vector :def-tll4-args/or 1 2 3 4 ())))
  (check (equal? (def-tll4-args/or 1 2 3 4 5) (vector :def-tll4-args/or 1 2 3 4 '(5)))))

(define-test lambda-list/no-default-optionals-local-define
  (define (ldef-tll1-arg/o :optional o1)  (vector :ldef-tll1-arg/o o1))
  (define (ldef-tll2-args/o a1 :optional o1)  (vector :ldef-tll2-args/o a1 o1))
  (define (ldef-tll3-args/o a1 a2 :optional o1)  (vector :ldef-tll3-args/o a1 a2 o1))
  (define (ldef-tll4-args/o a1 a2 :optional o1 o2)  (vector :ldef-tll4-args/o a1 a2 o1 o2))

  (define (ldef-tll1-arg/or :optional o1 . r)  (vector :ldef-tll1-arg/or o1 r))
  (define (ldef-tll2-args/or a1 :optional o1 . r)  (vector :ldef-tll2-args/or a1 o1 r))
  (define (ldef-tll3-args/or a1 a2 :optional o1 . r)  (vector :ldef-tll3-args/or a1 a2 o1 r))
  (define (ldef-tll4-args/or a1 a2 :optional o1 o2 . r)  (vector :ldef-tll4-args/or a1 a2 o1 o2 r))


  (check (eq? (procedure-name ldef-tll1-arg/o) 'ldef-tll1-arg/o))
  (check (equal? (ldef-tll1-arg/o) (vector :ldef-tll1-arg/o ())))
  (check (equal? (ldef-tll1-arg/o 1) (vector :ldef-tll1-arg/o 1)))
  (check (equal? (ldef-tll1-arg/o 1 2) (vector :ldef-tll1-arg/o 1)))

  (check (eq? (procedure-name ldef-tll2-args/o) 'ldef-tll2-args/o))
  (check (runtime-error? (ldef-tll2-args/o)))
  (check (equal? (ldef-tll2-args/o 1) (vector :ldef-tll2-args/o 1 ())))
  (check (equal? (ldef-tll2-args/o 1 2) (vector :ldef-tll2-args/o 1 2)))
  (check (equal? (ldef-tll2-args/o 1 2 3) (vector :ldef-tll2-args/o 1 2)))

  (check (eq? (procedure-name ldef-tll3-args/o) 'ldef-tll3-args/o))
  (check (runtime-error? (ldef-tll3-args/o)))
  (check (runtime-error? (ldef-tll3-args/o 1)))
  (check (equal? (ldef-tll3-args/o 1 2) (vector :ldef-tll3-args/o 1 2 ())))
  (check (equal? (ldef-tll3-args/o 1 2 3) (vector :ldef-tll3-args/o 1 2 3)))
  (check (equal? (ldef-tll3-args/o 1 2 3 4) (vector :ldef-tll3-args/o 1 2 3)))

  (check (eq? (procedure-name ldef-tll4-args/o) 'ldef-tll4-args/o))
  (check (runtime-error? (ldef-tll4-args/o)))
  (check (runtime-error? (ldef-tll4-args/o 1)))
  (check (equal? (ldef-tll4-args/o 1 2) (vector :ldef-tll4-args/o 1 2 () ())))
  (check (equal? (ldef-tll4-args/o 1 2 3) (vector :ldef-tll4-args/o 1 2 3 ())))
  (check (equal? (ldef-tll4-args/o 1 2 3 4) (vector :ldef-tll4-args/o 1 2 3 4)))
  (check (equal? (ldef-tll4-args/o 1 2 3 4 5) (vector :ldef-tll4-args/o 1 2 3 4)))



  (check (eq? (procedure-name ldef-tll1-arg/or) 'ldef-tll1-arg/or))
  (check (equal? (ldef-tll1-arg/or) (vector :ldef-tll1-arg/or () ())))
  (check (equal? (ldef-tll1-arg/or 1) (vector :ldef-tll1-arg/or 1 ())))
  (check (equal? (ldef-tll1-arg/or 1 2) (vector :ldef-tll1-arg/or 1 '(2))))

  (check (eq? (procedure-name ldef-tll2-args/or) 'ldef-tll2-args/or))
  (check (runtime-error? (ldef-tll2-args/or)))
  (check (equal? (ldef-tll2-args/or 1) (vector :ldef-tll2-args/or 1 () ())))
  (check (equal? (ldef-tll2-args/or 1 2) (vector :ldef-tll2-args/or 1 2 ())))
  (check (equal? (ldef-tll2-args/or 1 2 3) (vector :ldef-tll2-args/or 1 2 '(3))))

  (check (eq? (procedure-name ldef-tll3-args/or) 'ldef-tll3-args/or))
  (check (runtime-error? (ldef-tll3-args/or)))
  (check (runtime-error? (ldef-tll3-args/or 1)))
  (check (equal? (ldef-tll3-args/or 1 2) (vector :ldef-tll3-args/or 1 2 () ())))
  (check (equal? (ldef-tll3-args/or 1 2 3) (vector :ldef-tll3-args/or 1 2 3 ())))
  (check (equal? (ldef-tll3-args/or 1 2 3 4) (vector :ldef-tll3-args/or 1 2 3 '(4))))

  (check (eq? (procedure-name ldef-tll4-args/or) 'ldef-tll4-args/or))
  (check (runtime-error? (ldef-tll4-args/or)))
  (check (runtime-error? (ldef-tll4-args/or 1)))
  (check (equal? (ldef-tll4-args/or 1 2) (vector :ldef-tll4-args/or 1 2 () () ())))
  (check (equal? (ldef-tll4-args/or 1 2 3) (vector :ldef-tll4-args/or 1 2 3 () ())))
  (check (equal? (ldef-tll4-args/or 1 2 3 4) (vector :ldef-tll4-args/or 1 2 3 4 ())))
  (check (equal? (ldef-tll4-args/or 1 2 3 4 5) (vector :ldef-tll4-args/or 1 2 3 4 '(5)))))

(define lam-tll1-arg/o (lambda (:optional o1)  (vector :lam-tll1-arg/o o1)))
(define lam-tll2-args/o (lambda (a1 :optional o1)  (vector :lam-tll2-args/o a1 o1)))
(define lam-tll3-args/o (lambda (a1 a2 :optional o1)  (vector :lam-tll3-args/o a1 a2 o1)))
(define lam-tll4-args/o (lambda (a1 a2 :optional o1 o2)  (vector :lam-tll4-args/o a1 a2 o1 o2)))

(define lam-tll1-arg/or (lambda (:optional o1 . r)  (vector :lam-tll1-arg/or o1 r)))
(define lam-tll2-args/or (lambda (a1 :optional o1 . r)  (vector :lam-tll2-args/or a1 o1 r)))
(define lam-tll3-args/or (lambda (a1 a2 :optional o1 . r)  (vector :lam-tll3-args/or a1 a2 o1 r)))
(define lam-tll4-args/or (lambda (a1 a2 :optional o1 o2 . r)  (vector :lam-tll4-args/or a1 a2 o1 o2 r)))

(define-test lambda-list/no-default-optionals-lambda
  (check (eq? (procedure-name lam-tll1-arg/o) #f))
  (check (equal? (lam-tll1-arg/o) (vector :lam-tll1-arg/o ())))
  (check (equal? (lam-tll1-arg/o 1) (vector :lam-tll1-arg/o 1)))
  (check (equal? (lam-tll1-arg/o 1 2) (vector :lam-tll1-arg/o 1)))

  (check (eq? (procedure-name lam-tll2-args/o) #f))
  (check (runtime-error? (lam-tll2-args/o)))
  (check (equal? (lam-tll2-args/o 1) (vector :lam-tll2-args/o 1 ())))
  (check (equal? (lam-tll2-args/o 1 2) (vector :lam-tll2-args/o 1 2)))
  (check (equal? (lam-tll2-args/o 1 2 3) (vector :lam-tll2-args/o 1 2)))

  (check (eq? (procedure-name lam-tll3-args/o) #f))
  (check (runtime-error? (lam-tll3-args/o)))
  (check (runtime-error? (lam-tll3-args/o 1)))
  (check (equal? (lam-tll3-args/o 1 2) (vector :lam-tll3-args/o 1 2 ())))
  (check (equal? (lam-tll3-args/o 1 2 3) (vector :lam-tll3-args/o 1 2 3)))
  (check (equal? (lam-tll3-args/o 1 2 3 4) (vector :lam-tll3-args/o 1 2 3)))

  (check (eq? (procedure-name lam-tll4-args/o) #f))
  (check (runtime-error? (lam-tll4-args/o)))
  (check (runtime-error? (lam-tll4-args/o 1)))
  (check (equal? (lam-tll4-args/o 1 2) (vector :lam-tll4-args/o 1 2 () ())))
  (check (equal? (lam-tll4-args/o 1 2 3) (vector :lam-tll4-args/o 1 2 3 ())))
  (check (equal? (lam-tll4-args/o 1 2 3 4) (vector :lam-tll4-args/o 1 2 3 4)))
  (check (equal? (lam-tll4-args/o 1 2 3 4 5) (vector :lam-tll4-args/o 1 2 3 4)))


  (check (eq? (procedure-name lam-tll1-arg/or) #f))
  (check (equal? (lam-tll1-arg/or) (vector :lam-tll1-arg/or () ())))
  (check (equal? (lam-tll1-arg/or 1) (vector :lam-tll1-arg/or 1 ())))
  (check (equal? (lam-tll1-arg/or 1 2) (vector :lam-tll1-arg/or 1 '(2))))

  (check (eq? (procedure-name lam-tll2-args/or) #f))
  (check (runtime-error? (lam-tll2-args/or)))
  (check (equal? (lam-tll2-args/or 1) (vector :lam-tll2-args/or 1 () ())))
  (check (equal? (lam-tll2-args/or 1 2) (vector :lam-tll2-args/or 1 2 ())))
  (check (equal? (lam-tll2-args/or 1 2 3) (vector :lam-tll2-args/or 1 2 '(3))))

  (check (eq? (procedure-name lam-tll3-args/or) #f))
  (check (runtime-error? (lam-tll3-args/or)))
  (check (runtime-error? (lam-tll3-args/or 1)))
  (check (equal? (lam-tll3-args/or 1 2) (vector :lam-tll3-args/or 1 2 () ())))
  (check (equal? (lam-tll3-args/or 1 2 3) (vector :lam-tll3-args/or 1 2 3 ())))
  (check (equal? (lam-tll3-args/or 1 2 3 4) (vector :lam-tll3-args/or 1 2 3 '(4))))

  (check (eq? (procedure-name lam-tll4-args/or) #f))
  (check (runtime-error? (lam-tll4-args/or)))
  (check (runtime-error? (lam-tll4-args/or 1)))
  (check (equal? (lam-tll4-args/or 1 2) (vector :lam-tll4-args/or 1 2 () () ())))
  (check (equal? (lam-tll4-args/or 1 2 3) (vector :lam-tll4-args/or 1 2 3 () ())))
  (check (equal? (lam-tll4-args/or 1 2 3 4) (vector :lam-tll4-args/or 1 2 3 4 ())))
  (check (equal? (lam-tll4-args/or 1 2 3 4 5) (vector :lam-tll4-args/or 1 2 3 4 '(5)))))

(define (def-tll1-arg/do :optional (o1 :o1))  (vector :def-tll1-arg/do o1))
(define (def-tll2-args/do a1 :optional (o1 :o1))  (vector :def-tll2-args/do a1 o1))
(define (def-tll3-args/do a1 a2 :optional (o1 :o1))  (vector :def-tll3-args/do a1 a2 o1))
(define (def-tll4-args/do a1 a2 :optional (o1 :o1) (o2 :o2))  (vector :def-tll4-args/do a1 a2 o1 o2))

(define (def-tll1-arg/dor :optional (o1 :o1) . r)  (vector :def-tll1-arg/dor o1 r))
(define (def-tll2-args/dor a1 :optional (o1 :o1) . r)  (vector :def-tll2-args/dor a1 o1 r))
(define (def-tll3-args/dor a1 a2 :optional (o1 :o1) . r)  (vector :def-tll3-args/dor a1 a2 o1 r))
(define (def-tll4-args/dor a1 a2 :optional (o1 :o1) (o2 :o2) . r)  (vector :def-tll4-args/dor a1 a2 o1 o2 r))

(define-test lambda-list/optionals-define
  (check (eq? (procedure-name def-tll1-arg/do) 'def-tll1-arg/do))
  (check (equal? (def-tll1-arg/do) (vector :def-tll1-arg/do :o1)))
  (check (equal? (def-tll1-arg/do 1) (vector :def-tll1-arg/do 1)))
  (check (equal? (def-tll1-arg/do 1 2) (vector :def-tll1-arg/do 1)))

  (check (eq? (procedure-name def-tll2-args/do) 'def-tll2-args/do))
  (check (runtime-error? (def-tll2-args/do)))
  (check (equal? (def-tll2-args/do 1) (vector :def-tll2-args/do 1 :o1)))
  (check (equal? (def-tll2-args/do 1 2) (vector :def-tll2-args/do 1 2)))
  (check (equal? (def-tll2-args/do 1 2 3) (vector :def-tll2-args/do 1 2)))

  (check (eq? (procedure-name def-tll3-args/do) 'def-tll3-args/do))
  (check (runtime-error? (def-tll3-args/do)))
  (check (runtime-error? (def-tll3-args/do 1)))
  (check (equal? (def-tll3-args/do 1 2) (vector :def-tll3-args/do 1 2 :o1)))
  (check (equal? (def-tll3-args/do 1 2 3) (vector :def-tll3-args/do 1 2 3)))
  (check (equal? (def-tll3-args/do 1 2 3 4) (vector :def-tll3-args/do 1 2 3)))

  (check (eq? (procedure-name def-tll4-args/do) 'def-tll4-args/do))
  (check (runtime-error? (def-tll4-args/do)))
  (check (runtime-error? (def-tll4-args/do 1)))
  (check (equal? (def-tll4-args/do 1 2) (vector :def-tll4-args/do 1 2 :o1 :o2)))
  (check (equal? (def-tll4-args/do 1 2 3) (vector :def-tll4-args/do 1 2 3 :o2)))
  (check (equal? (def-tll4-args/do 1 2 3 4) (vector :def-tll4-args/do 1 2 3 4)))
  (check (equal? (def-tll4-args/do 1 2 3 4 5) (vector :def-tll4-args/do 1 2 3 4)))



  (check (eq? (procedure-name def-tll1-arg/dor) 'def-tll1-arg/dor))
  (check (equal? (def-tll1-arg/dor) (vector :def-tll1-arg/dor :o1 ())))
  (check (equal? (def-tll1-arg/dor 1) (vector :def-tll1-arg/dor 1 ())))
  (check (equal? (def-tll1-arg/dor 1 2) (vector :def-tll1-arg/dor 1 '(2))))

  (check (eq? (procedure-name def-tll2-args/dor) 'def-tll2-args/dor))
  (check (runtime-error? (def-tll2-args/dor)))
  (check (equal? (def-tll2-args/dor 1) (vector :def-tll2-args/dor 1 :o1 ())))
  (check (equal? (def-tll2-args/dor 1 2) (vector :def-tll2-args/dor 1 2 ())))
  (check (equal? (def-tll2-args/dor 1 2 3) (vector :def-tll2-args/dor 1 2 '(3))))

  (check (eq? (procedure-name def-tll3-args/dor) 'def-tll3-args/dor))
  (check (runtime-error? (def-tll3-args/dor)))
  (check (runtime-error? (def-tll3-args/dor 1)))
  (check (equal? (def-tll3-args/dor 1 2) (vector :def-tll3-args/dor 1 2 :o1 ())))
  (check (equal? (def-tll3-args/dor 1 2 3) (vector :def-tll3-args/dor 1 2 3 ())))
  (check (equal? (def-tll3-args/dor 1 2 3 4) (vector :def-tll3-args/dor 1 2 3 '(4))))

  (check (eq? (procedure-name def-tll4-args/dor) 'def-tll4-args/dor))
  (check (runtime-error? (def-tll4-args/dor)))
  (check (runtime-error? (def-tll4-args/dor 1)))
  (check (equal? (def-tll4-args/dor 1 2) (vector :def-tll4-args/dor 1 2 :o1 :o2 ())))
  (check (equal? (def-tll4-args/dor 1 2 3) (vector :def-tll4-args/dor 1 2 3 :o2 ())))
  (check (equal? (def-tll4-args/dor 1 2 3 4) (vector :def-tll4-args/dor 1 2 3 4 ())))
  (check (equal? (def-tll4-args/dor 1 2 3 4 5) (vector :def-tll4-args/dor 1 2 3 4 '(5)))))

(define-test lambda-list/optionals-local-define
  (define (ldef-tll1-arg/do :optional (o1 :o1))  (vector :ldef-tll1-arg/do o1))
  (define (ldef-tll2-args/do a1 :optional (o1 :o1))  (vector :ldef-tll2-args/do a1 o1))
  (define (ldef-tll3-args/do a1 a2 :optional (o1 :o1))  (vector :ldef-tll3-args/do a1 a2 o1))
  (define (ldef-tll4-args/do a1 a2 :optional (o1 :o1) (o2 :o2))  (vector :ldef-tll4-args/do a1 a2 o1 o2))

  (define (ldef-tll1-arg/dor :optional (o1 :o1) . r)  (vector :ldef-tll1-arg/dor o1 r))
  (define (ldef-tll2-args/dor a1 :optional (o1 :o1) . r)  (vector :ldef-tll2-args/dor a1 o1 r))
  (define (ldef-tll3-args/dor a1 a2 :optional (o1 :o1) . r)  (vector :ldef-tll3-args/dor a1 a2 o1 r))
  (define (ldef-tll4-args/dor a1 a2 :optional (o1 :o1) (o2 :o2) . r)  (vector :ldef-tll4-args/dor a1 a2 o1 o2 r))


  (check (eq? (procedure-name ldef-tll1-arg/do) 'ldef-tll1-arg/do))
  (check (equal? (ldef-tll1-arg/do) (vector :ldef-tll1-arg/do :o1)))
  (check (equal? (ldef-tll1-arg/do 1) (vector :ldef-tll1-arg/do 1)))
  (check (equal? (ldef-tll1-arg/do 1 2) (vector :ldef-tll1-arg/do 1)))

  (check (eq? (procedure-name ldef-tll2-args/do) 'ldef-tll2-args/do))
  (check (runtime-error? (ldef-tll2-args/do)))
  (check (equal? (ldef-tll2-args/do 1) (vector :ldef-tll2-args/do 1 :o1)))
  (check (equal? (ldef-tll2-args/do 1 2) (vector :ldef-tll2-args/do 1 2)))
  (check (equal? (ldef-tll2-args/do 1 2 3) (vector :ldef-tll2-args/do 1 2)))

  (check (eq? (procedure-name ldef-tll3-args/do) 'ldef-tll3-args/do))
  (check (runtime-error? (ldef-tll3-args/do)))
  (check (runtime-error? (ldef-tll3-args/do 1)))
  (check (equal? (ldef-tll3-args/do 1 2) (vector :ldef-tll3-args/do 1 2 :o1)))
  (check (equal? (ldef-tll3-args/do 1 2 3) (vector :ldef-tll3-args/do 1 2 3)))
  (check (equal? (ldef-tll3-args/do 1 2 3 4) (vector :ldef-tll3-args/do 1 2 3)))

  (check (eq? (procedure-name ldef-tll4-args/do) 'ldef-tll4-args/do))
  (check (runtime-error? (ldef-tll4-args/do)))
  (check (runtime-error? (ldef-tll4-args/do 1)))
  (check (equal? (ldef-tll4-args/do 1 2) (vector :ldef-tll4-args/do 1 2 :o1 :o2)))
  (check (equal? (ldef-tll4-args/do 1 2 3) (vector :ldef-tll4-args/do 1 2 3 :o2)))
  (check (equal? (ldef-tll4-args/do 1 2 3 4) (vector :ldef-tll4-args/do 1 2 3 4)))
  (check (equal? (ldef-tll4-args/do 1 2 3 4 5) (vector :ldef-tll4-args/do 1 2 3 4)))



  (check (eq? (procedure-name ldef-tll1-arg/dor) 'ldef-tll1-arg/dor))
  (check (equal? (ldef-tll1-arg/dor) (vector :ldef-tll1-arg/dor :o1 ())))
  (check (equal? (ldef-tll1-arg/dor 1) (vector :ldef-tll1-arg/dor 1 ())))
  (check (equal? (ldef-tll1-arg/dor 1 2) (vector :ldef-tll1-arg/dor 1 '(2))))

  (check (eq? (procedure-name ldef-tll2-args/dor) 'ldef-tll2-args/dor))
  (check (runtime-error? (ldef-tll2-args/dor)))
  (check (equal? (ldef-tll2-args/dor 1) (vector :ldef-tll2-args/dor 1 :o1 ())))
  (check (equal? (ldef-tll2-args/dor 1 2) (vector :ldef-tll2-args/dor 1 2 ())))
  (check (equal? (ldef-tll2-args/dor 1 2 3) (vector :ldef-tll2-args/dor 1 2 '(3))))

  (check (eq? (procedure-name ldef-tll3-args/dor) 'ldef-tll3-args/dor))
  (check (runtime-error? (ldef-tll3-args/dor)))
  (check (runtime-error? (ldef-tll3-args/dor 1)))
  (check (equal? (ldef-tll3-args/dor 1 2) (vector :ldef-tll3-args/dor 1 2 :o1 ())))
  (check (equal? (ldef-tll3-args/dor 1 2 3) (vector :ldef-tll3-args/dor 1 2 3 ())))
  (check (equal? (ldef-tll3-args/dor 1 2 3 4) (vector :ldef-tll3-args/dor 1 2 3 '(4))))

  (check (eq? (procedure-name ldef-tll4-args/dor) 'ldef-tll4-args/dor))
  (check (runtime-error? (ldef-tll4-args/dor)))
  (check (runtime-error? (ldef-tll4-args/dor 1)))
  (check (equal? (ldef-tll4-args/dor 1 2) (vector :ldef-tll4-args/dor 1 2 :o1 :o2 ())))
  (check (equal? (ldef-tll4-args/dor 1 2 3) (vector :ldef-tll4-args/dor 1 2 3 :o2 ())))
  (check (equal? (ldef-tll4-args/dor 1 2 3 4) (vector :ldef-tll4-args/dor 1 2 3 4 ())))
  (check (equal? (ldef-tll4-args/dor 1 2 3 4 5) (vector :ldef-tll4-args/dor 1 2 3 4 '(5)))))

(define lam-tll1-arg/do (lambda (:optional (o1 :o1))  (vector :lam-tll1-arg/do o1)))
(define lam-tll2-args/do (lambda (a1 :optional (o1 :o1))  (vector :lam-tll2-args/do a1 o1)))
(define lam-tll3-args/do (lambda (a1 a2 :optional (o1 :o1))  (vector :lam-tll3-args/do a1 a2 o1)))
(define lam-tll4-args/do (lambda (a1 a2 :optional (o1 :o1) (o2 :o2))  (vector :lam-tll4-args/do a1 a2 o1 o2)))

(define lam-tll1-arg/dor (lambda (:optional (o1 :o1) . r)  (vector :lam-tll1-arg/dor o1 r)))
(define lam-tll2-args/dor (lambda (a1 :optional (o1 :o1) . r)  (vector :lam-tll2-args/dor a1 o1 r)))
(define lam-tll3-args/dor (lambda (a1 a2 :optional (o1 :o1) . r)  (vector :lam-tll3-args/dor a1 a2 o1 r)))
(define lam-tll4-args/dor (lambda (a1 a2 :optional (o1 :o1) (o2 :o2) . r)  (vector :lam-tll4-args/dor a1 a2 o1 o2 r)))

(define-test lambda-list/optionals-lambda
  (check (eq? (procedure-name lam-tll1-arg/do) #f))
  (check (equal? (lam-tll1-arg/do) (vector :lam-tll1-arg/do :o1)))
  (check (equal? (lam-tll1-arg/do 1) (vector :lam-tll1-arg/do 1)))
  (check (equal? (lam-tll1-arg/do 1 2) (vector :lam-tll1-arg/do 1)))

  (check (eq? (procedure-name lam-tll2-args/do) #f))
  (check (runtime-error? (lam-tll2-args/do)))
  (check (equal? (lam-tll2-args/do 1) (vector :lam-tll2-args/do 1 :o1)))
  (check (equal? (lam-tll2-args/do 1 2) (vector :lam-tll2-args/do 1 2)))
  (check (equal? (lam-tll2-args/do 1 2 3) (vector :lam-tll2-args/do 1 2)))

  (check (eq? (procedure-name lam-tll3-args/do) #f))
  (check (runtime-error? (lam-tll3-args/do)))
  (check (runtime-error? (lam-tll3-args/do 1)))
  (check (equal? (lam-tll3-args/do 1 2) (vector :lam-tll3-args/do 1 2 :o1)))
  (check (equal? (lam-tll3-args/do 1 2 3) (vector :lam-tll3-args/do 1 2 3)))
  (check (equal? (lam-tll3-args/do 1 2 3 4) (vector :lam-tll3-args/do 1 2 3)))

  (check (eq? (procedure-name lam-tll4-args/do) #f))
  (check (runtime-error? (lam-tll4-args/do)))
  (check (runtime-error? (lam-tll4-args/do 1)))
  (check (equal? (lam-tll4-args/do 1 2) (vector :lam-tll4-args/do 1 2 :o1 :o2)))
  (check (equal? (lam-tll4-args/do 1 2 3) (vector :lam-tll4-args/do 1 2 3 :o2)))
  (check (equal? (lam-tll4-args/do 1 2 3 4) (vector :lam-tll4-args/do 1 2 3 4)))
  (check (equal? (lam-tll4-args/do 1 2 3 4 5) (vector :lam-tll4-args/do 1 2 3 4)))


  (check (eq? (procedure-name lam-tll1-arg/dor) #f))
  (check (equal? (lam-tll1-arg/dor) (vector :lam-tll1-arg/dor :o1 ())))
  (check (equal? (lam-tll1-arg/dor 1) (vector :lam-tll1-arg/dor 1 ())))
  (check (equal? (lam-tll1-arg/dor 1 2) (vector :lam-tll1-arg/dor 1 '(2))))

  (check (eq? (procedure-name lam-tll2-args/dor) #f))
  (check (runtime-error? (lam-tll2-args/dor)))
  (check (equal? (lam-tll2-args/dor 1) (vector :lam-tll2-args/dor 1 :o1 ())))
  (check (equal? (lam-tll2-args/dor 1 2) (vector :lam-tll2-args/dor 1 2 ())))
  (check (equal? (lam-tll2-args/dor 1 2 3) (vector :lam-tll2-args/dor 1 2 '(3))))

  (check (eq? (procedure-name lam-tll3-args/dor) #f))
  (check (runtime-error? (lam-tll3-args/dor)))
  (check (runtime-error? (lam-tll3-args/dor 1)))
  (check (equal? (lam-tll3-args/dor 1 2) (vector :lam-tll3-args/dor 1 2 :o1 ())))
  (check (equal? (lam-tll3-args/dor 1 2 3) (vector :lam-tll3-args/dor 1 2 3 ())))
  (check (equal? (lam-tll3-args/dor 1 2 3 4) (vector :lam-tll3-args/dor 1 2 3 '(4))))

  (check (eq? (procedure-name lam-tll4-args/dor) #f))
  (check (runtime-error? (lam-tll4-args/dor)))
  (check (runtime-error? (lam-tll4-args/dor 1)))
  (check (equal? (lam-tll4-args/dor 1 2) (vector :lam-tll4-args/dor 1 2 :o1 :o2 ())))
  (check (equal? (lam-tll4-args/dor 1 2 3) (vector :lam-tll4-args/dor 1 2 3 :o2 ())))
  (check (equal? (lam-tll4-args/dor 1 2 3 4) (vector :lam-tll4-args/dor 1 2 3 4 ())))
  (check (equal? (lam-tll4-args/dor 1 2 3 4 5) (vector :lam-tll4-args/dor 1 2 3 4 '(5)))))

(define-test lambda-list/invalid-lambda-syntax

  (check (runtime-error? (eval '(define (test-fn :bad-type k)
                                      "Bad argument type."))))

  (check (runtime-error? (eval '(define (test-fn x :bad-type k)
                                      "Bad argument type."))))

  (check (runtime-error? (eval '(define (test-fn :bad-type k . r)
                                      "Bad argument type."))))

  (check (runtime-error? (eval '(define (test-fn x :bad-type k . r)
                                      "Bad argument type."))))

  (check (runtime-error? (eval '(define (test-fn . :bad-r-name)
                                      "no keywords as argument names."))))

  (check (runtime-error? (eval '(define (test-fn a1 . :bad-r-name)
                                      "no keywords as argument names."))))

  (check (runtime-error? (eval '(define (test-fn x a1 y . :bad-r-name)
                                      "no keywords as argument names."))))

  (check (runtime-error? (eval '(define (test-fn :keyword k :optional o)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn :keyword k :optional o1 o2)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn x :keyword k :optional o)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn x y :keyword k :optional o1 o2)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn x :keyword k :optional o . r)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn x y :keyword k :optional o1 o2 . r)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn :keyword :optional o)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn :keyword :optional o1 o2)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn x :keyword :optional o)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn x y :keyword :optional o1 o2)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn x :keyword :optional o . r)
                                       "no keywords before optionals"))))

  (check (runtime-error? (eval '(define (test-fn x y :keyword :optional o1 o2 . r)
                                       "no keywords before optionals"))))


  (check (runtime-error? (eval '(define (test-fn x :keyword :bad-keyword-name)
                                      "bad keyword name"))))

  (check (runtime-error? (eval '(define (test-fn x :keyword 12)
                                      "bad keyword name"))))

  (check (runtime-error? (eval '(define (test-fn x :keyword (12))
                                      "bad keyword name"))))

  (check (runtime-error? (eval '(define (test-fn x :keyword (:bad-name 12))
                                      "bad keyword name"))))

  (check (runtime-error? (eval '(define (test-fn x :keyword (12 :bad-name))
                                      "bad keyword name"))))

  (check (runtime-error? (eval '(define (test-fn x :keyword (:bad-name bad-name 1 sd))
                                      "too many keyword forms")))))

(define (def-tll-keyword-form-a :keyword x)              x)
(define (def-tll-keyword-form-b :keyword (x))            x)
(define (def-tll-keyword-form-c :keyword (:key-x x))     x)
(define (def-tll-keyword-form-d :keyword (x 42))         x)
(define (def-tll-keyword-form-e :keyword (:key-x x 42))  x)

;; validation of undefined keywords without rest arg
;; no validation of undefined keywords with rest arg
;; evaluation of keyword initialization forms
;; optional arguments actually consume arguments so they aren't considered as keywords

(define-test lambda-list/keyword-forms
  (check (equal? (def-tll-keyword-form-a :x 12) 12))
  (check (equal? (def-tll-keyword-form-a) ()))

  (check (equal? (def-tll-keyword-form-b :x 24) 24))
  (check (equal? (def-tll-keyword-form-b) ()))

  (check (equal? (def-tll-keyword-form-c :key-x 36) 36))
  (check (equal? (def-tll-keyword-form-c) ()))

  (check (equal? (def-tll-keyword-form-d :x 48) 48))
  (check (equal? (def-tll-keyword-form-d) 42))

  (check (equal? (def-tll-keyword-form-e :key-x 60) 60))
  (check (equal? (def-tll-keyword-form-e) 42)))

(define (def-tll-keyword-validation :keyword (k1 1) (k2 2) (k3 3))
  (vector k1 k2 k3))

(define (def-tll-keyword-no-validation :keyword (k1 1) (k2 2) (k3 3) . r)
  (vector k1 k2 k3 r))

(define-test lambda-list/keyword-validation
  (check (equal? (def-tll-keyword-validation) (vector 1 2 3)))
  (check (equal? (def-tll-keyword-no-validation) (vector 1 2 3 ())))

  (check (equal? (def-tll-keyword-validation :k1 'foo) (vector 'foo 2 3)))
  (check (equal? (def-tll-keyword-no-validation :k1 'foo) (vector 'foo 2 3 '(:k1 foo))))

  (check (equal? (def-tll-keyword-validation :k1 'foo :k2 'bar :k3 'baz)
                 (vector 'foo 'bar 'baz)))
  (check (equal? (def-tll-keyword-no-validation :k1 'foo)
                 (vector 'foo 2 3  '(:k1 foo))))

  (check (runtime-error? (def-tll-keyword-validation :k10 'foo)))
  (check (equal? (def-tll-keyword-no-validation :k10 'foo)
                 (vector 1 2 3 '(:k10 foo))))

  (check (runtime-error? (def-tll-keyword-validation :k10 'foo :k1 4 :k2 6 :k3 8)))
  (check (equal? (def-tll-keyword-no-validation :k10 'foo :k1 4 :k2 6 :k3 8)
                 (vector 4 6 8 '(:k10 foo :k1 4 :k2 6 :k3 8)))))
