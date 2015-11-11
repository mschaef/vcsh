(use-package! "unit-test")

(define-test or/truth-table
  (test-case (boolean? (or #f #f #f)))
  (test-case (boolean? (or #t #t #t)))
  (test-case (not (boolean? (or #f 4))))

  (test-case (eq? (or) #f))
  (test-case (eq? (or #f) #f))
  (test-case (eq? (or #t) #t))
  (test-case (eq? (or #f #f) #f))
  (test-case (eq? (or #f #t) #t))
  (test-case (eq? (or #t #f) #t))
  (test-case (eq? (or #t #t) #t))
  (test-case (eq? (or #f #f #f) #f))
  (test-case (eq? (or #f #f #t) #t))
  (test-case (eq? (or #f #t #f) #t))
  (test-case (eq? (or #f #t #t) #t))
  (test-case (eq? (or #t #f #f) #t))
  (test-case (eq? (or #t #f #t) #t))
  (test-case (eq? (or #t #t #f) #t))
  (test-case (eq? (or #t #t #t) #t)))

(define-test or/short-circuit
  (test-case
   (equal? '(:second-leg)
           (checkpoint-order-of
            (or #f
                (checkpoint :second-leg #t)))))

  (test-case
   (equal? ()
             (checkpoint-order-of
              (or #t
                  (checkpoint :second-leg #t)))))

  (test-case
   (equal? '(1 2 3)
             (checkpoint-order-of
              (or (checkpoint 1 #f)
                  (checkpoint 2 #f)
                  (checkpoint 3 #f)))))

  (test-case
   (equal? '(1)
             (checkpoint-order-of
              (or (checkpoint 1 #t)
                  (checkpoint 2 #f)
                  (checkpoint 3 #f)))))

  (test-case
   (equal? '(1)
             (checkpoint-order-of
              (or (checkpoint 1 #t)
                  (checkpoint 2 #t)
                  (checkpoint 3 #t))))))

(define-test or/return-value
  (test-case (eq? (or :foo) :foo))
    (test-case (eq? (or #f :foo) :foo))
    (test-case (eq? (or #f #f :foo) :foo)))

(define-test or*/truth-table
  (test-case (boolean? (or* #f #f #f)))
  (test-case (boolean? (or* #t #t #t)))

  (test-case (eq? (or*) #f))
  (test-case (eq? (or* #f) #f))
  (test-case (eq? (or* #t) #t))
  (test-case (eq? (or* #f #f) #f))
  (test-case (eq? (or* #f #t) #t))
  (test-case (eq? (or* #t #f) #t))
  (test-case (eq? (or* #t #t) #t))
  (test-case (eq? (or* #f #f #f) #f))
  (test-case (eq? (or* #f #f #t) #t))
  (test-case (eq? (or* #f #t #f) #t))
  (test-case (eq? (or* #f #t #t) #t))
  (test-case (eq? (or* #t #f #f) #t))
  (test-case (eq? (or* #t #f #t) #t))
  (test-case (eq? (or* #t #t #f) #t))
  (test-case (eq? (or* #t #t #t) #t)))

(define-test or*/full-evaluation
  (test-case
   (equal? '(:second-leg)
           (checkpoint-order-of
            (or* #f
                 (checkpoint :second-leg #t)))))

  (test-case
   (equal? '(:second-leg)
           (checkpoint-order-of
            (or* #t
                 (checkpoint :second-leg #t)))))

  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (or* (checkpoint 1 #f)
                 (checkpoint 2 #f)
                 (checkpoint 3 #f)))))
  
  (test-case
   (equal? '(1 2 3)
             (checkpoint-order-of
              (or* (checkpoint 1 #t)
                   (checkpoint 2 #f)
                   (checkpoint 3 #f)))))

  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (or* (checkpoint 1 #t)
                 (checkpoint 2 #t)
                 (checkpoint 3 #t))))))

(define-test or*/return-value
  (test-case (eq? (or* :foo) #t))
  (test-case (eq? (or* #f :foo) #t))
  (test-case (eq? (or* #f #f :foo) #t)))
