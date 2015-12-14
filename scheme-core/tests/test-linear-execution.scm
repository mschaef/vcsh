(use-package! "unit-test")

(define-test linear-execution
  (check
   (equal? '(1 2 3 4 5)
           (checkpoint-order-of
            (checkpoint 1)
            (checkpoint 2)
            (checkpoint 3)
            (checkpoint 4)
            (checkpoint 5)))))
