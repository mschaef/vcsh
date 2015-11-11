(use-package! "unit-test")

(define-test map-1
  (test-case (equal? '() (map even? '())))
  (test-case (list? (map identity '(1 2 3 4 5))))
  (test-case (equal? '(#f #t #f #t #f) (map even? '(1 2 3 4 5))))
  (test-case (runtime-error? (map () '(1 2 3 4 5))))
  (test-case (runtime-error? (map even? [1 2 3 4 5])))

  (test-case (equal? '() (map (lambda (x) (+ x 1)) '())))
  (test-case (equal? '(1) (map (lambda (x) (+ x 1)) '(0))))
  (test-case (equal? '(1 2) (map (lambda (x) (+ x 1)) '(0 1))))
  (test-case (equal? '(2 3 4 5 6) (map (lambda (x) (+ x 1)) '(1 2 3 4 5))))

  (test-case (not (runtime-error? (map (lambda (x) (error "Test Error")) '()))))
  (test-case (runtime-error? (map (lambda (x) (error "Test Error")) '(1))))

  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (catch 'foo
              (map (lambda (x) 
                     (checkpoint 2) 
                     (throw 'foo)
                     (checkpoint :unreached))
                   '(1)))
            (checkpoint 3)))))

(define-test map-2
  (define (c+ x y) (+ x y)) ; + as a closure, rather than a subr
  (test-case (equal? '() (map + '() '())))
  (test-case (list? (map + '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (test-case (equal? '(1 2 3 4 5) (map + '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (test-case (equal? '(1 2 3) (map + '(1 3 5 7 9) '(0 -1 -2))))
  (test-case (equal? '(1 2 3) (map + '(1 3 5) '(0 -1 -2 -3 -4))))

  (test-case (equal? '() (map c+ '() '())))
  (test-case (list? (map c+ '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (test-case (equal? '(1 2 3 4 5) (map c+ '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (test-case (equal? '(1 2 3) (map c+ '(1 3 5 7 9) '(0 -1 -2))))
  (test-case (equal? '(1 2 3) (map c+ '(1 3 5) '(0 -1 -2 -3 -4))))

  (test-case (list? (map identity '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (test-case (equal? '(1 3 5 7 9) (map identity '(1 3 5 7 9) '(0 -1 -2 -3 -4))))

  (test-case (runtime-error? (map ()  '(1 3 5 7 9) '(0 -1 -2 -3 -4))))

  (test-case (not (runtime-error? (map (lambda (x) (error "Test Error")) '() '()))))
  (test-case (not (runtime-error? (map (lambda (x) (error "Test Error")) '(1) '()))))
  (test-case (not (runtime-error? (map (lambda (x) (error "Test Error")) '() '(2)))))
  (test-case (runtime-error? (map (lambda (x) (error "Test Error")) '(1) '(2))))

  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (catch 'foo
              (map (lambda (x y)
                     (checkpoint 2) 
                     (throw 'foo)
                     (checkpoint :unreached))
                   '(1) '(20)))
            (checkpoint 3)))))

(define-test mapping
  (test-case (equal? (map identity '()) '()))
  (test-case (equal? (map error '()) '()))
  (test-case (equal? (map identity '(1 2 3 4 5)) '(1 2 3 4 5)))
  (test-case (equal? (map (lambda (x) (* x 2)) '(1 2 3 4 5)) '(2 4 6 8 10)))

  (test-case (equal? (append-map identity '()) '()))
  (test-case (equal? (append-map! identity '()) '()))
  (test-case (equal? (append-map error '()) '()))
  (test-case (equal? (append-map! error '()) '()))

  (test-case (equal? (append-map identity '(() ())) '()))
  (test-case (equal? (append-map! identity '(() ())) '()))

  (test-case (runtime-error? (append-map identity '(1 2 3 4 5))))
  (test-case (runtime-error? (append-map! identity '(1 2 3 4 5))))

  (test-case (equal? (append-map identity (map cons '(1 2 3 4 5)))  '(1 2 3 4 5)))
  (test-case (equal? (append-map! identity (map cons '(1 2 3 4 5))) '(1 2 3 4 5)))

  (test-case (equal? (append-map identity (map #L(list _ (* _ 2)) '(1 2 3 4 5)))  
                     '(1 2 2 4 3 6 4 8 5 10)))

  (test-case (equal? (append-map! identity (map #L(list _ (* _ 2)) '(1 2 3 4 5)))  
                     '(1 2 2 4 3 6 4 8 5 10)))

  (test-case (equal? (append-map #L2(list _0 _1) '(1 2 3 4 5) '(2 4 6 8 10))
                     '(1 2 2 4 3 6 4 8 5 10)))

  (test-case (equal? (append-map! #L2(list _0 _1) '(1 2 3 4 5) '(2 4 6 8 10))
                     '(1 2 2 4 3 6 4 8 5 10)))

  (test-case (equal? (append-map cons '(1 2 3 4 5)) '(1 2 3 4 5)))
  (test-case (equal? (append-map! cons '(1 2 3 4 5)) '(1 2 3 4 5)))

  (test-case (equal? (map + '(1 2 3) '(4 5 6)) '(5 7 9))))


 
(define-test map-pair-1
  (test-case (equal? '() (map-pair even? '())))
  (test-case (list? (map-pair identity '(1 2 3 4 5))))
  (test-case (equal? '(#f #t #f #t #f) (map-pair (lambda (x) (even? (car x))) '(1 2 3 4 5))))
  (test-case (runtime-error? (map-pair () '(1 2 3 4 5))))
  (test-case (runtime-error? (map-pair even? [1 2 3 4 5])))

  (test-case (equal? '() (map-pair identity '())))
  (test-case (equal? '((0)) (map-pair identity '(0))))
  (test-case (equal? '((0 1) (1)) (map-pair identity '(0 1))))
  (test-case (equal? '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5)) (map-pair identity '(1 2 3 4 5))))

  (test-case (not (runtime-error? (map-pair (lambda (x) (error "Test Error")) '()))))
  (test-case (runtime-error? (map-pair (lambda (x) (error "Test Error")) '(1))))

  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (catch 'foo
              (map-pair (lambda (x) 
                          (checkpoint 2) 
                          (throw 'foo)
                          (checkpoint :unreached))
                        '(1)))
            (checkpoint 3)))))

(define-test map-pair-2
  (define (c+ x y) (+ x y)) ; + as a closure, rather than a subr
  (test-case (equal? '() (map-pair + '() '())))
  (test-case (list? (map-pair cons '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (test-case (equal? '(((1 3 5 7 9) 0 -1 -2 -3 -4) ((3 5 7 9) -1 -2 -3 -4) ((5 7 9) -2 -3 -4) ((7 9) -3 -4) ((9) -4))
                 (map-pair cons '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (test-case (equal? ' (((1 3 5 7 9) 0 -1 -2) ((3 5 7 9) -1 -2) ((5 7 9) -2))
                 (map-pair cons '(1 3 5 7 9) '(0 -1 -2))))
  (test-case (equal? '(((1 3 5) 0 -1 -2 -3 -4) ((3 5) -1 -2 -3 -4) ((5) -2 -3 -4))
                 (map-pair cons '(1 3 5) '(0 -1 -2 -3 -4))))
  (test-case (list? (map-pair identity '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (test-case (equal? '((1 3 5 7 9) (3 5 7 9) (5 7 9) (7 9) (9)) (map-pair identity '(1 3 5 7 9) '(0 -1 -2 -3 -4))))

  (test-case (runtime-error? (map-pair ()  '(1 3 5 7 9) '(0 -1 -2 -3 -4))))

  (test-case (not (runtime-error? (map-pair (lambda (x) (error "Test Error")) '() '()))))
  (test-case (not (runtime-error? (map-pair (lambda (x) (error "Test Error")) '(1) '()))))
  (test-case (not (runtime-error? (map-pair (lambda (x) (error "Test Error")) '() '(2)))))
  (test-case (runtime-error? (map-pair (lambda (x) (error "Test Error")) '(1) '(2))))

  (test-case
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (catch 'foo
              (map-pair (lambda (x y)
                          (checkpoint 2) 
                          (throw 'foo)
                          (checkpoint :unreached))
                        '(1) '(20)))
            (checkpoint 3)))))
