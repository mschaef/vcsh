(use-package! "unit-test")

(define-test handler-bind

  (test-case (equal? (handler-bind ((signal-1 (lambda () :foo))) 12) 12))

  (test-case/execution-order 4
    (checkpoint 1)
    (catch 'error-escape
      (checkpoint 2)
      (signal 'unhandled-signal)
      (checkpoint 3)
    (checkpoint 4)))

  (test-case/execution-order 3
    (handler-bind ((signal-1 (lambda () (checkpoint 2))))
      (checkpoint 1)
      (signal 'signal-1)
      (checkpoint 3)))

  (test-case/execution-order 4
    (handler-bind ((signal-1 (lambda () (checkpoint 3))))
      (handler-bind ((signal-1 (lambda () (checkpoint 2))))
	(checkpoint 1)
	(signal 'signal-1)
	(checkpoint 4))))

  (test-case/execution-order (:begin :inner-begin :outer :inner-end :outer :end)
    (handler-bind ((signal-1 (lambda () 
			       (checkpoint :outer))))
      (handler-bind ((signal-1 (lambda () 
				 (checkpoint :inner-begin)
				 (signal 'signal-1)
				 (checkpoint :inner-end))))
	(checkpoint :begin)
	(signal 'signal-1)
	(checkpoint :end))))



  (test-case/execution-order 4
    (handler-bind ((condition-1 (lambda args (checkpoint 2)))
		   (condition-2 (lambda args (checkpoint 3))))
      (checkpoint 1)
      (signal 'condition-1)
      (signal 'condition-2)
      (checkpoint 4)))

    (test-case/execution-order (:begin 
			  :outer-handler-established 
			  :in-catch 
			  :inner-handler-established 
			  :outer-test-handler
			  :end)
    (checkpoint :begin)
    (handler-bind ((test-handler (lambda () (checkpoint :outer-test-handler))))
      (checkpoint :outer-handler-established)
      (catch 'drop-inner-handler
	(checkpoint :in-catch)
	(handler-bind ((test-handler (lambda () (checkpoint :inner-test-handler))))
	  (checkpoint :inner-handler-established)
	  (throw 'drop-inner-handler)
	  (checkpoint #f)))
      (signal 'test-handler)
      (checkpoint :end)))

  (test-case/execution-order (:begin 
			  :outer-handler-established 
			  :in-catch 
			  :inner-handler-established 
			  :unwinding
			  :outer-test-handler
			  :end)
    (checkpoint :begin)
    (handler-bind ((test-handler (lambda () (checkpoint :outer-test-handler))))
      (checkpoint :outer-handler-established)
      (catch 'drop-inner-handler
	(checkpoint :in-catch)
	(unwind-protect (lambda ()
			  (handler-bind ((test-handler (lambda () (checkpoint :inner-test-handler))))
			    (checkpoint :inner-handler-established)
			    (throw 'drop-inner-handler)
			    (checkpoint #f)))
			(lambda ()
			  (checkpoint :unwinding))))
      (signal 'test-handler)
      (checkpoint :end)))
	
  )

