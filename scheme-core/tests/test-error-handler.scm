(use-package! "unit-test")

(define-test error-handler
  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (catch 'error-escape
              (handler-bind ((runtime-error (lambda () (throw 'error-escape))))
                (handler-bind ((runtime-error  (lambda () (checkpoint 2))))
                  (checkpoint 1)
                  (error "foo")
                  (checkpoint #f))))
            (checkpoint 3)))))
