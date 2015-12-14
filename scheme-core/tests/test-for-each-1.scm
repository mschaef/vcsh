(use-package! "unit-test")

(define-test for-each-1
  (check (runtime-error? (for-each () '(1 2 3 4 5))))
  (check (not (runtime-error? (for-each () 'not-a-list))))
  (check (not (runtime-error? (for-each even? [1 2 3 4 5]))))
  (check (not (runtime-error? (for-each even? '(1 2 3 4 5)))))

  (let ((xs '()))
    (for-each (lambda (x) (push! x xs)) '())
    (check (equal? xs '())))

  (let ((xs '()))
    (for-each (lambda (x) (push! x xs)) '(1))
    (check (equal? xs '(1))))

  (let ((xs '()))
    (for-each (lambda (x) (push! x xs)) '(1 2))
    (check (equal? xs '(2 1))))

  (let ((xs '()))
    (for-each (lambda (x) (push! x xs)) '(1 2 3 4 5))
    (check (equal? xs '(5 4 3 2 1))))


  (let ((xs ()))
    (for-each (lambda (x) (push! x xs)) ())
    (check (equal? xs '())))

  (let ((xs ()))
    (vector-for-each (lambda (x) (push! x xs)) [1])
    (check (equal? xs '(1))))

  (let ((xs ()))
    (vector-for-each (lambda (x) (push! x xs)) [1 2])
    (check (equal? xs '(2 1))))

  (let ((xs ()))
    (vector-for-each (lambda (x) (push! x xs)) [1 2 3 4 5])
    (check (equal? xs '(5 4 3 2 1))))

  (check (not (runtime-error? (for-each (lambda (x) (error "Test Error")) '()))))
  (check (runtime-error? (for-each (lambda (x) (error "Test Error")) '(1))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (catch 'foo
              (for-each (lambda (x) 
                          (checkpoint 2) 
                          (throw 'foo)
                          (checkpoint :unreached))
                        '(1)))
            (checkpoint 3)))))
