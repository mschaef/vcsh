(use-package! "unit-test")

(define-test dolist
  (check (not (runtime-error? (dolist (n ())))))
  (check (not (runtime-error? (dolist (n () 1)))))
  (check (not (runtime-error? (dolist (n ())))))
  (check (not (runtime-error? (dolist (n [1])))))
  (check (not (runtime-error? (dolist (n [1] 1)))))
  (check (eq? 1 (dolist (n () 1))))
  
  (check
   (equal? '(1 2)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n ())
              (checkpoint n))
            (checkpoint 2))))
  
  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n '(2))
              (checkpoint n))
            (checkpoint 3))))
  
  (check
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n '(2 3))
              (checkpoint n))
            (checkpoint 4))))
  
  (check
   (equal? '(1 2 3 4 5 6)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n '(2 3 4 5))
              (checkpoint n))
            (checkpoint 6))))
  
  (check (runtime-error? (dolist (n ((lambda () (error "Internal error")) '(1 2 3 4 5)))
                           (write n))))
  
  (check (runtime-error? (dolist (n ((lambda () '(1 2 3 4 5))))
                           (error "Internal error")
                           (write n))))
  
  (let ((q (gensym "dolist-test-symbol")))
    (check (eq? q (dolist (n '(1 2 3 4 5) q)
                    ()))))
  
  
  (check (eq? 1 (dolist (n [] 1))))
  
  (check
   (equal? '(1 2)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n [])
              (checkpoint n))
            (checkpoint 2))))
  
  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (dovec (n [2])
              (checkpoint n))
            (checkpoint 3))))
  
  (check
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (checkpoint 1)
            (dovec (n [2 3])
              (checkpoint n))
            (checkpoint 4))))
  
  (check
   (equal? '(1 2 3 4 5 6)
           (checkpoint-order-of
            (checkpoint 1)
            (dovec (n [2 3 4 5])
              (checkpoint n))
            (checkpoint 6))))
  
  (check (runtime-error? (dovec (n ((lambda () (error "Internal error")) [1 2 3 4 5]))
                               (write n))))
  
  (check (runtime-error? (dovec (n ((lambda () [1 2 3 4 5])))
                               (error "Internal error")
                               (write n))))
  
  (let ((q (gensym "dolist-test-symbol")))
    (check (eq? q (dolist (n [1 2 3 4 5] q)
                    ())))))
