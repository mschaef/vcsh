(define-package "test-boolean"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test boolean?
  (check (boolean? #f))
  (check (boolean? #t))
  (check (not (boolean? 1)))
  (check (not (boolean? 'symbol)))
  (check (not (boolean? #\a)))
  (check (not (boolean? [1 2 3 4 5]))))

(define-test and/truth-table
  (check (boolean? (and #t #f #t)))
  (check (boolean? (and #t #t #t)))
  (check (eq? (and) #t))
  (check (eq? (and #f) #f))
  (check (eq? (and #t) #t))
  (check (eq? (and #f #f) #f))
  (check (eq? (and #f #t) #f))
  (check (eq? (and #t #f) #f))
  (check (eq? (and #t #t) #t))
  (check (eq? (and #f #f #f) #f))
  (check (eq? (and #f #f #t) #f))
  (check (eq? (and #f #t #f) #f))
  (check (eq? (and #f #t #t) #f))
  (check (eq? (and #t #f #f) #f))
  (check (eq? (and #t #f #t) #f))
  (check (eq? (and #t #t #f) #f))
  (check (eq? (and #t #t #t) #t)))

(define-test and/return-value
  (check (eq? (and :foo) :foo))
  (check (eq? (and :foo :bar :baz) :baz)))

(define-test and/short-circuit
  (check
   (equal? '(1 2)
           (checkpoint-order-of 
            (and (checkpoint 1 #t)
                 (checkpoint 2 #t)))))
  
  (check
   (equal? '(1)
           (checkpoint-order-of 
            (and (checkpoint 1 #f)
                 (checkpoint 2 #t)))))
  
  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (and (checkpoint 1 #t)
                 (checkpoint 2 #t)
                 (checkpoint 3 #t)))))
  
  (check
   (equal? '(1)
           (checkpoint-order-of
            (and (checkpoint 1 #f)
                 (checkpoint 2 #f)
                 (checkpoint 3 #f))))))

(define-test and*/truth-table
  (check (boolean? (and* #t #f #t)))
  (check (boolean? (and* #t #t #t)))
  (check (eq? (and*) #t))
  (check (eq? (and* #f) #f))
  (check (eq? (and* #t) #t))
  (check (eq? (and* #f #f) #f))
  (check (eq? (and* #f #t) #f))
  (check (eq? (and* #t #f) #f))
  (check (eq? (and* #t #t) #t))
  (check (eq? (and* #f #f #f) #f))
  (check (eq? (and* #f #f #t) #f))
  (check (eq? (and* #f #t #f) #f))
  (check (eq? (and* #f #t #t) #f))
  (check (eq? (and* #t #f #f) #f))
  (check (eq? (and* #t #f #t) #f))
  (check (eq? (and* #t #t #f) #f))
  (check (eq? (and* #t #t #t) #t)))

(define-test and*/return-value
  (check (eq? (and* :foo) #t))
  (check (eq? (and* :foo :bar :baz) #t)))

(define-test and*/full-evaluation
  (check
   (equal? '(1 2)
           (checkpoint-order-of
            (and* (checkpoint 1 #t)
                  (checkpoint 2 #t)))))

  (check
   (equal? '(1 2)
           (checkpoint-order-of
            (and* (checkpoint 1 #f)
                  (checkpoint 2 #t)))))
  
  (check
   (equal? '(1 2 3)
             (checkpoint-order-of
              (and* (checkpoint 1 #t)
                    (checkpoint 2 #t)
                    (checkpoint 3 #t)))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (and* (checkpoint 1 #f)
                  (checkpoint 2 #f)
                  (checkpoint 3 #f))))))

(define-test or/truth-table
  (check (boolean? (or #f #f #f)))
  (check (boolean? (or #t #t #t)))
  (check (not (boolean? (or #f 4))))

  (check (eq? (or) #f))
  (check (eq? (or #f) #f))
  (check (eq? (or #t) #t))
  (check (eq? (or #f #f) #f))
  (check (eq? (or #f #t) #t))
  (check (eq? (or #t #f) #t))
  (check (eq? (or #t #t) #t))
  (check (eq? (or #f #f #f) #f))
  (check (eq? (or #f #f #t) #t))
  (check (eq? (or #f #t #f) #t))
  (check (eq? (or #f #t #t) #t))
  (check (eq? (or #t #f #f) #t))
  (check (eq? (or #t #f #t) #t))
  (check (eq? (or #t #t #f) #t))
  (check (eq? (or #t #t #t) #t)))

(define-test or/short-circuit
  (check
   (equal? '(:second-leg)
           (checkpoint-order-of
            (or #f
                (checkpoint :second-leg #t)))))

  (check
   (equal? ()
             (checkpoint-order-of
              (or #t
                  (checkpoint :second-leg #t)))))

  (check
   (equal? '(1 2 3)
             (checkpoint-order-of
              (or (checkpoint 1 #f)
                  (checkpoint 2 #f)
                  (checkpoint 3 #f)))))

  (check
   (equal? '(1)
             (checkpoint-order-of
              (or (checkpoint 1 #t)
                  (checkpoint 2 #f)
                  (checkpoint 3 #f)))))

  (check
   (equal? '(1)
             (checkpoint-order-of
              (or (checkpoint 1 #t)
                  (checkpoint 2 #t)
                  (checkpoint 3 #t))))))

(define-test or/return-value
  (check (eq? (or :foo) :foo))
    (check (eq? (or #f :foo) :foo))
    (check (eq? (or #f #f :foo) :foo)))

(define-test or*/truth-table
  (check (boolean? (or* #f #f #f)))
  (check (boolean? (or* #t #t #t)))

  (check (eq? (or*) #f))
  (check (eq? (or* #f) #f))
  (check (eq? (or* #t) #t))
  (check (eq? (or* #f #f) #f))
  (check (eq? (or* #f #t) #t))
  (check (eq? (or* #t #f) #t))
  (check (eq? (or* #t #t) #t))
  (check (eq? (or* #f #f #f) #f))
  (check (eq? (or* #f #f #t) #t))
  (check (eq? (or* #f #t #f) #t))
  (check (eq? (or* #f #t #t) #t))
  (check (eq? (or* #t #f #f) #t))
  (check (eq? (or* #t #f #t) #t))
  (check (eq? (or* #t #t #f) #t))
  (check (eq? (or* #t #t #t) #t)))

(define-test or*/full-evaluation
  (check
   (equal? '(:second-leg)
           (checkpoint-order-of
            (or* #f
                 (checkpoint :second-leg #t)))))

  (check
   (equal? '(:second-leg)
           (checkpoint-order-of
            (or* #t
                 (checkpoint :second-leg #t)))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (or* (checkpoint 1 #f)
                 (checkpoint 2 #f)
                 (checkpoint 3 #f)))))
  
  (check
   (equal? '(1 2 3)
             (checkpoint-order-of
              (or* (checkpoint 1 #t)
                   (checkpoint 2 #f)
                   (checkpoint 3 #f)))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (or* (checkpoint 1 #t)
                 (checkpoint 2 #t)
                 (checkpoint 3 #t))))))

(define-test or*/return-value
  (check (eq? (or* :foo) #t))
  (check (eq? (or* #f :foo) #t))
  (check (eq? (or* #f #f :foo) #t)))
