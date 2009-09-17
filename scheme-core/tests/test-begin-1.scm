(use-package! "unit-test")

(define-test begin-1
  (let ((test-value (gensym "test-value")))
    (test-case (equal? () (begin-1)))
    (test-case (equal? test-value (begin-1 test-value)))
    (test-case (equal? test-value (begin-1 test-value 1)))
    (test-case (equal? test-value (begin-1 test-value 1 2 3))))
  
  (test-case/execution-order 2
    (checkpoint 1)
    (begin-1)
    (checkpoint 2))

  (test-case/execution-order 3
    (checkpoint 1)
    (begin-1
     (checkpoint 2))
    (checkpoint 3))

  (test-case/execution-order 4
    (checkpoint 1)
    (begin-1
     (checkpoint 2)
     (checkpoint 3))
    (checkpoint 4))

  (test-case/execution-order 5
    (checkpoint 1)
    (begin-1
     (checkpoint 2)
     (checkpoint 3)
     (checkpoint 4))
    (checkpoint 5)))


