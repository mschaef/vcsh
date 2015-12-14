(define-package "test-error"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test error
  (check
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
