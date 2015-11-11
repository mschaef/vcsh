(use-package! "unit-test")

(define-test and/truth-table
  (test-case (boolean? (and #t #f #t)))
  (test-case (boolean? (and #t #t #t)))
  (test-case (eq? (and) #t))
  (test-case (eq? (and #f) #f))
  (test-case (eq? (and #t) #t))
  (test-case (eq? (and #f #f) #f))
  (test-case (eq? (and #f #t) #f))
  (test-case (eq? (and #t #f) #f))
  (test-case (eq? (and #t #t) #t))
  (test-case (eq? (and #f #f #f) #f))
  (test-case (eq? (and #f #f #t) #f))
  (test-case (eq? (and #f #t #f) #f))
  (test-case (eq? (and #f #t #t) #f))
  (test-case (eq? (and #t #f #f) #f))
  (test-case (eq? (and #t #f #t) #f))
  (test-case (eq? (and #t #t #f) #f))
  (test-case (eq? (and #t #t #t) #t)))

(define-test and/return-value
  (test-case (eq? (and :foo) :foo))
  (test-case (eq? (and :foo :bar :baz) :baz)))

(define-test and/short-circuit
  (test-case
   (equal? '(1 2)
           (checkpoint-order-of 
            (and (checkpoint 1 #t)
                 (checkpoint 2 #t)))))
  
  (test-case
   (equal? '(1)
           (checkpoint-order-of 
            (and (checkpoint 1 #f)
                 (checkpoint 2 #t)))))
  
  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (and (checkpoint 1 #t)
                 (checkpoint 2 #t)
                 (checkpoint 3 #t)))))
  
  (test-case
   (equal? '(1)
           (checkpoint-order-of
            (and (checkpoint 1 #f)
                 (checkpoint 2 #f)
                 (checkpoint 3 #f))))))

(define-test and*/truth-table
  (test-case (boolean? (and* #t #f #t)))
  (test-case (boolean? (and* #t #t #t)))
  (test-case (eq? (and*) #t))
  (test-case (eq? (and* #f) #f))
  (test-case (eq? (and* #t) #t))
  (test-case (eq? (and* #f #f) #f))
  (test-case (eq? (and* #f #t) #f))
  (test-case (eq? (and* #t #f) #f))
  (test-case (eq? (and* #t #t) #t))
  (test-case (eq? (and* #f #f #f) #f))
  (test-case (eq? (and* #f #f #t) #f))
  (test-case (eq? (and* #f #t #f) #f))
  (test-case (eq? (and* #f #t #t) #f))
  (test-case (eq? (and* #t #f #f) #f))
  (test-case (eq? (and* #t #f #t) #f))
  (test-case (eq? (and* #t #t #f) #f))
  (test-case (eq? (and* #t #t #t) #t)))

(define-test and*/return-value
  (test-case (eq? (and* :foo) #t))
  (test-case (eq? (and* :foo :bar :baz) #t)))

(define-test and*/full-evaluation
  (test-case
   (equal? '(1 2)
           (checkpoint-order-of
            (and* (checkpoint 1 #t)
                  (checkpoint 2 #t)))))

  (test-case
   (equal? '(1 2)
           (checkpoint-order-of
            (and* (checkpoint 1 #f)
                  (checkpoint 2 #t)))))
  
  (test-case
   (equal? '(1 2 3)
             (checkpoint-order-of
              (and* (checkpoint 1 #t)
                    (checkpoint 2 #t)
                    (checkpoint 3 #t)))))

  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (and* (checkpoint 1 #f)
                  (checkpoint 2 #f)
                  (checkpoint 3 #f))))))
