(use-package! "unit-test")

(define-test begin-1/return-value
  (let ((test-value (gensym "test-value")))
    (test-case (equal? () (begin-1)))
    (test-case (equal? test-value (begin-1 test-value)))
    (test-case (equal? test-value (begin-1 test-value 1)))
    (test-case (equal? test-value (begin-1 test-value 1 2 3)))))

(define-test begin-1/execution-order
  (test-case
   (equal? '(1 2)
           (checkpoint-order-of
            (checkpoint 1)
            (begin-1)
            (checkpoint 2))))

  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (begin-1
             (checkpoint 2))
            (checkpoint 3))))

  (test-case
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (checkpoint 1)
            (begin-1
             (checkpoint 2)
             (checkpoint 3))
            (checkpoint 4))))

  (test-case
   (equal? '(1 2 3 4 5)
           (checkpoint-order-of
            (checkpoint 1)
            (begin-1
             (checkpoint 2)
             (checkpoint 3)
             (checkpoint 4))
            (checkpoint 5)))))


