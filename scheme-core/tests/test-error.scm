(use-package! "unit-test")

(define-test error
  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (handler-bind ((runtime-error 
                            (lambda args
                              (throw 'test-escape-tag))))
              (checkpoint 1)
              (catch 'test-escape-tag
                (checkpoint 2)
                (error "foo")
                (checkpoint :does-not-run))
              (checkpoint 3))))))
