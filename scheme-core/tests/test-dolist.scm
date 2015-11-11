(use-package! "unit-test")

(define-test dolist
  (test-case (not (runtime-error? (dolist (n ())
                                ))))
  
  (test-case (not (runtime-error? (dolist (n () 1)
                                ))))
  
  
  (test-case (not (runtime-error? (dolist (n ())
                                ))))
  
  (test-case (not (runtime-error? (dolist (n [1])
                                ))))
  
  (test-case (not (runtime-error? (dolist (n [1] 1)
                                ))))
  
  
  (test-case (eq? 1 (dolist (n () 1)
                  )))
  
  (test-case
   (equal? '(1 2)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n ())
              (checkpoint n))
            (checkpoint 2))))
  
  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n '(2))
              (checkpoint n))
            (checkpoint 3))))
  
  (test-case
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n '(2 3))
              (checkpoint n))
            (checkpoint 4))))
  
  (test-case
   (equal? '(1 2 3 4 5 6)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n '(2 3 4 5))
              (checkpoint n))
            (checkpoint 6))))
  
  (test-case (runtime-error? (dolist (n ((lambda () (error "Internal error")) '(1 2 3 4 5)))
                           (write n))))
  
  (test-case (runtime-error? (dolist (n ((lambda () '(1 2 3 4 5))))
                           (error "Internal error")
                           (write n))))
  
  (let ((q (gensym "dolist-test-symbol")))
    (test-case (eq? q (dolist (n '(1 2 3 4 5) q)
                    ()))))
  
  
  (test-case (eq? 1 (dolist (n [] 1)
                  )))
  
  (test-case
   (equal? '(1 2)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n [])
              (checkpoint n))
            (checkpoint 2))))
  
  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (dovec (n [2])
              (checkpoint n))
            (checkpoint 3))))
  
  (test-case
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (checkpoint 1)
            (dovec (n [2 3])
              (checkpoint n))
            (checkpoint 4))))
  
  (test-case
   (equal? '(1 2 3 4 5 6)
           (checkpoint-order-of
            (checkpoint 1)
            (dovec (n [2 3 4 5])
              (checkpoint n))
            (checkpoint 6))))
  
  (test-case (runtime-error? (dovec (n ((lambda () (error "Internal error")) [1 2 3 4 5]))
                               (write n))))
  
  (test-case (runtime-error? (dovec (n ((lambda () [1 2 3 4 5])))
                               (error "Internal error")
                               (write n))))
  
  (let ((q (gensym "dolist-test-symbol")))
    (test-case (eq? q (dolist (n [1 2 3 4 5] q)
                    ())))))
