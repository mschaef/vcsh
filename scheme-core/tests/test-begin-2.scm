(use-package! "unit-test")

(define-test begin-2-return-value
  (let ((test-value (gensym "test-value")))
    (test-case (equal? () (begin-2)))
    (test-case (equal? () (begin-2 1)))
    (test-case (equal? 2 (begin-2 1 2)))

    (test-case (equal? test-value (begin-2 1 test-value)))
    (test-case (equal? test-value (begin-2 1 test-value 3)))
    (test-case (equal? test-value (begin-2 1 test-value 3 4 5)))))

(define-test begin-2-execution-order
  (test-case/execution-order 2
    (checkpoint 1)
    (begin-2)
    (checkpoint 2))

  (test-case/execution-order 3
    (checkpoint 1)
    (begin-2
     (checkpoint 2))
    (checkpoint 3))
  
  (test-case/execution-order 4
    (checkpoint 1)
    (begin-2
     (checkpoint 2)
     (checkpoint 3))
    (checkpoint 4))

  (test-case/execution-order 5
    (checkpoint 1)
    (begin-2
     (checkpoint 2)
     (checkpoint 3)
     (checkpoint 4))
    (checkpoint 5)))
