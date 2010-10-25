(use-package! "unit-test")

(define-test for-each-1
  (test-case (runtime-error? (for-each () '(1 2 3 4 5))))
  (test-case (not (runtime-error? (for-each () 'not-a-list))))
  (test-case (not (runtime-error? (for-each even? #(1 2 3 4 5)))))
  (test-case (not (runtime-error? (for-each even? '(1 2 3 4 5)))))

  (let ((xs '()))
    (for-each (lambda (x) (push! x xs)) '())
    (test-case (equal? xs '())))

  (let ((xs '()))
    (for-each (lambda (x) (push! x xs)) '(1))
    (test-case (equal? xs '(1))))

  (let ((xs '()))
    (for-each (lambda (x) (push! x xs)) '(1 2))
    (test-case (equal? xs '(2 1))))

  (let ((xs '()))
    (for-each (lambda (x) (push! x xs)) '(1 2 3 4 5))
    (test-case (equal? xs '(5 4 3 2 1))))


  (let ((xs ()))
    (for-each (lambda (x) (push! x xs)) ())
    (test-case (equal? xs '())))

  (let ((xs ()))
    (vector-for-each (lambda (x) (push! x xs)) #(1))
    (test-case (equal? xs '(1))))

  (let ((xs ()))
    (vector-for-each (lambda (x) (push! x xs)) #(1 2))
    (test-case (equal? xs '(2 1))))

  (let ((xs ()))
    (vector-for-each (lambda (x) (push! x xs)) #(1 2 3 4 5))
    (test-case (equal? xs '(5 4 3 2 1))))

  (test-case (not (runtime-error? (for-each (lambda (x) (error "Test Error")) '()))))
  (test-case (runtime-error? (for-each (lambda (x) (error "Test Error")) '(1))))

  (test-case/execution-order 3
    (checkpoint 1)
    (catch 'foo
      (for-each (lambda (x) 
	     (checkpoint 2) 
	     (throw 'foo)
	     (checkpoint :unreached))
	   '(1)))
    (checkpoint 3))

  )
