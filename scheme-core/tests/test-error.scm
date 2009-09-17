(use-package! "unit-test")

(define-test error
  (handler-bind ((runtime-error 
                  (lambda args (throw 'test-escape-tag))))
    (test-case/execution-order 3
      (checkpoint 1)
      (catch 'test-escape-tag
        (checkpoint 2)
        (error "foo")
        (checkpoint :does-not-run))
      (checkpoint 3))))
