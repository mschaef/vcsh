(use-package! "unit-test")

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
