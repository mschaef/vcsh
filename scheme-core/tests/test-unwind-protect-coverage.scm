(use-package! "unit-test")

(define-test unwind-protect-coverage
  ; No error
  (let ((after-evaluated? #f))
    (unwind-protect
     (lambda () )
     (lambda () (set! after-evaluated? #t) ))
    (test-case after-evaluated?))
  
  ;; No error - return value
  (let ((after-evaluated? #f))
    (test-case (eq? 'foo
                (unwind-protect
                 (lambda () 'foo)
                 (lambda () (set! after-evaluated? #t) ))))
    (test-case after-evaluated?))

  ; No error - return value (w/GC)
  (let ((after-evaluated? #f))
    (test-case (eq? 'foo
                (unwind-protect
                 (lambda () (gc) 'foo)
                 (lambda () (set! after-evaluated? #t)))))
    (test-case after-evaluated?))

  ; Error
  (let ((after-evaluated? #f))
    (catch-all
     (unwind-protect
      (lambda () (throw 'frobozzle))
      (lambda () (set! after-evaluated? #t) )))
    (test-case after-evaluated?))

  ; Nested - No Error
  (let ((sequence-number 0)
	(after-evaluated-1? #f)
	(after-evaluated-2? #f)
	(after-evaluated-3? #f))

    (catch-all
     (unwind-protect
      (lambda ()
	(unwind-protect
	 (lambda () 
	   (unwind-protect
	    (lambda () )
	    (lambda () 
	      (incr! sequence-number)
	      (set! after-evaluated-1? sequence-number))))
	 (lambda () 
	   (incr! sequence-number)
	   (set! after-evaluated-2? sequence-number))))
      (lambda () 
	(incr! sequence-number)
	(set! after-evaluated-3? sequence-number))))

    (test-case (eq? after-evaluated-1? 1))
    (test-case (eq? after-evaluated-2? 2))
    (test-case (eq? after-evaluated-3? 3)))
  
  ; Nested - Error
  (let ((sequence-number 0)
	(after-evaluated-1? #f)
	(after-evaluated-2? #f)
	(after-evaluated-3? #f))

    (catch-all
     (unwind-protect
      (lambda ()
	(unwind-protect
	 (lambda () 
	   (unwind-protect
	    (lambda () 
	      (set! sequence-number 10)
	      (throw 'foo) )
	    (lambda () 
	      (incr! sequence-number)
	      (set! after-evaluated-1? sequence-number))))
	 (lambda () 
	   (incr! sequence-number)
	   (set! after-evaluated-2? sequence-number))))
      (lambda () 
	(incr! sequence-number)
	(set! after-evaluated-3? sequence-number))))

    (test-case (eq? after-evaluated-1? 11))
    (test-case (eq? after-evaluated-2? 12))
    (test-case (eq? after-evaluated-3? 13)))

  (test-case/execution-order 4

    (let ((return-value (begin
			  (let ((catch-return-value (catch 'test-name
						      (checkpoint 1)
						      (unwind-protect
						       (lambda () 
							 (checkpoint 2))
						       (lambda () 
							 (checkpoint 3)
							 (throw 'test-name :test-return-value)
							 (checkpoint :unreached-1)))
						      (checkpoint :unreached-2))))
			  (checkpoint 4 catch-return-value)))))
      (test-case (equal? :test-return-value return-value)))))

