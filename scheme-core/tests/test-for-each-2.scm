(use-package! "unit-test")

(define-test for-each-2
  (check (runtime-error? (for-each () '(1 2 3 4 5) '(1 2 3 4 5))))

  (check (not (runtime-error? (for-each + [1 2 3 4 5] '(1 2 3 4 5)))))
  (check (not (runtime-error? (for-each + '(1 2 3 4 5) [1 2 3 4 5]))))

  (check (not (runtime-error? (for-each () 'not-a-list ()))))
  (check (not (runtime-error? (for-each () () 'not-a-list))))

  (check (not (runtime-error? (for-each () 'not-a-list '(1 2)))))
  (check (not (runtime-error? (for-each () '(1 2) 'not-a-list))))
  (check (not (runtime-error? (for-each () 'not-a-list [1 2]))))
  (check (not (runtime-error? (for-each () [1 2] 'not-a-list))))

  (check (not (runtime-error? (for-each even? [1 2 3 4 5]))))
  (check (not (runtime-error? (for-each even? '(1 2 3 4 5)))))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) '() '())
    (check (equal? xs '())))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) '(1) '(a))
    (check (equal? xs '((1 . a)))))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) '(1 2) '(a b))
    (check (equal? xs '((2 . b) (1 . a)))))

  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) '(1 2 3 4 5) '(a b c d e))
    (check (equal? xs '((5 . e) (4 . d) (3 . c) (2 . b) (1 . a)))))


  (let ((xs '()))
    (for-each (lambda (x y) (push! (cons x y) xs)) [] [])
    (check (equal? xs '())))

  (let ((xs '()))
    (vector-for-each (lambda (x y) (push! (cons x y) xs)) [1] '[a])
    (check (equal? xs '((1 . a)))))

  (let ((xs '()))
    (vector-for-each (lambda (x y) (push! (cons x y) xs)) [1 2] '[a b])
    (check (equal? xs '((2 . b) (1 . a)))))

  (let ((xs '()))
    (vector-for-each (lambda (x y) (push! (cons x y) xs)) [1 2 3 4 5] '[a b c d e])
    (check (equal? xs '((5 . e) (4 . d) (3 . c) (2 . b) (1 . a)))))

  (check (runtime-error? (for-each (lambda (x) (error "Test Error")) '(1) '(1))))
  (check (not (runtime-error? (for-each (lambda (x) (error "Test Error")) '(1) '()))))
  (check (not (runtime-error? (for-each (lambda (x) (error "Test Error")) '() '(1)))))
  (check (not (runtime-error? (for-each (lambda (x) (error "Test Error")) '() '()))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (catch 'foo
              (for-each (lambda (x y) 
                          (checkpoint 2) 
                          (throw 'foo)
                          (checkpoint :unreached))
                        '(1) '(2)))
            (checkpoint 3)))))
