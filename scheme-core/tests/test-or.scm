(use-package! "unit-test")

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
