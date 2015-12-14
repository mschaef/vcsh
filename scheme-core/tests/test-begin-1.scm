(use-package! "unit-test")

(define-test begin-1/return-value
  (let ((test-value (gensym "test-value")))
    (check (equal? () (begin-1)))
    (check (equal? test-value (begin-1 test-value)))
    (check (equal? test-value (begin-1 test-value 1)))
    (check (equal? test-value (begin-1 test-value 1 2 3)))))

(define-test begin-1/execution-order
  (check
   (equal? '(1 2)
           (checkpoint-order-of
            (checkpoint 1)
            (begin-1)
            (checkpoint 2))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (begin-1
             (checkpoint 2))
            (checkpoint 3))))

  (check
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (checkpoint 1)
            (begin-1
             (checkpoint 2)
             (checkpoint 3))
            (checkpoint 4))))

  (check
   (equal? '(1 2 3 4 5)
           (checkpoint-order-of
            (checkpoint 1)
            (begin-1
             (checkpoint 2)
             (checkpoint 3)
             (checkpoint 4))
            (checkpoint 5)))))


