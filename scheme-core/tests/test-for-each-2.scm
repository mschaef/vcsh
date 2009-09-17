(use-package! "unit-test")

(define-test for-each-2
  (test-case (runtime-error? (for-each () '(1 2 3 4 5) '(1 2 3 4 5))))

  (test-case (not (runtime-error? (for-each + #(1 2 3 4 5) '(1 2 3 4 5)))))
  (test-case (not (runtime-error? (for-each + '(1 2 3 4 5) #(1 2 3 4 5)))))

  (test-case (not (runtime-error? (for-each () 'not-a-list ()))))
  (test-case (not (runtime-error? (for-each () () 'not-a-list))))

  (test-case (not (runtime-error? (for-each () 'not-a-list '(1 2)))))
  (test-case (not (runtime-error? (for-each () '(1 2) 'not-a-list))))
  (test-case (not (runtime-error? (for-each () 'not-a-list #(1 2)))))
  (test-case (not (runtime-error? (for-each () #(1 2) 'not-a-list))))

  (test-case (not (runtime-error? (for-each even? #(1 2 3 4 5)))))
  (test-case (not (runtime-error? (for-each even? '(1 2 3 4 5)))))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) '() '())
    (test-case (equal? xs '())))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) '(1) '(a))
    (test-case (equal? xs '((1 . a)))))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) '(1 2) '(a b))
    (test-case (equal? xs '((2 . b) (1 . a)))))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) '(1 2 3 4 5) '(a b c d e))
    (test-case (equal? xs '((5 . e) (4 . d) (3 . c) (2 . b) (1 . a)))))


  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) #() #())
    (test-case (equal? xs '())))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) #(1) #(a))
    (test-case (equal? xs '((1 . a)))))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) #(1 2) #(a b))
    (test-case (equal? xs '((2 . b) (1 . a)))))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) #(1 2 3 4 5) #(a b c d e))
    (test-case (equal? xs '((5 . e) (4 . d) (3 . c) (2 . b) (1 . a)))))

  (test-case (runtime-error? (for-each (lambda (x) (error "Test Error")) '(1) '(1))))
  (test-case (not (runtime-error? (for-each (lambda (x) (error "Test Error")) '(1) '()))))
  (test-case (not (runtime-error? (for-each (lambda (x) (error "Test Error")) '() '(1)))))
  (test-case (not (runtime-error? (for-each (lambda (x) (error "Test Error")) '() '()))))


  (test-case/execution-order 3
    (checkpoint 1)
    (catch 'foo
      (for-each (lambda (x y) 
	     (checkpoint 2) 
	     (throw 'foo)
	     (checkpoint :unreached))
	   '(1) '(2)))
    (checkpoint 3))

  )
