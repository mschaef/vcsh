(use-package! "unit-test")

(define-test error-handler
  (test-case/execution-order 3
    (catch 'error-escape
      (handler-bind ((runtime-error (lambda () (throw 'error-escape))))
        (handler-bind ((runtime-error  (lambda () (checkpoint 2))))
          (checkpoint 1)
          (error "foo")
          (checkpoint #f))))
    (checkpoint 3))
  )