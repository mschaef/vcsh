(use-package! "unit-test")

(define-test map-1
  (check (equal? '() (map even? '())))
  (check (list? (map identity '(1 2 3 4 5))))
  (check (equal? '(#f #t #f #t #f) (map even? '(1 2 3 4 5))))
  (check (runtime-error? (map () '(1 2 3 4 5))))
  (check (runtime-error? (map even? [1 2 3 4 5])))

  (check (equal? '() (map (lambda (x) (+ x 1)) '())))
  (check (equal? '(1) (map (lambda (x) (+ x 1)) '(0))))
  (check (equal? '(1 2) (map (lambda (x) (+ x 1)) '(0 1))))
  (check (equal? '(2 3 4 5 6) (map (lambda (x) (+ x 1)) '(1 2 3 4 5))))

  (check (not (runtime-error? (map (lambda (x) (error "Test Error")) '()))))
  (check (runtime-error? (map (lambda (x) (error "Test Error")) '(1))))

  (check
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
  (check (equal? '() (map + '() '())))
  (check (list? (map + '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (check (equal? '(1 2 3 4 5) (map + '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (check (equal? '(1 2 3) (map + '(1 3 5 7 9) '(0 -1 -2))))
  (check (equal? '(1 2 3) (map + '(1 3 5) '(0 -1 -2 -3 -4))))

  (check (equal? '() (map c+ '() '())))
  (check (list? (map c+ '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (check (equal? '(1 2 3 4 5) (map c+ '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (check (equal? '(1 2 3) (map c+ '(1 3 5 7 9) '(0 -1 -2))))
  (check (equal? '(1 2 3) (map c+ '(1 3 5) '(0 -1 -2 -3 -4))))

  (check (list? (map identity '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (check (equal? '(1 3 5 7 9) (map identity '(1 3 5 7 9) '(0 -1 -2 -3 -4))))

  (check (runtime-error? (map ()  '(1 3 5 7 9) '(0 -1 -2 -3 -4))))

  (check (not (runtime-error? (map (lambda (x) (error "Test Error")) '() '()))))
  (check (not (runtime-error? (map (lambda (x) (error "Test Error")) '(1) '()))))
  (check (not (runtime-error? (map (lambda (x) (error "Test Error")) '() '(2)))))
  (check (runtime-error? (map (lambda (x) (error "Test Error")) '(1) '(2))))

  (check
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
  (check (equal? (map identity '()) '()))
  (check (equal? (map error '()) '()))
  (check (equal? (map identity '(1 2 3 4 5)) '(1 2 3 4 5)))
  (check (equal? (map (lambda (x) (* x 2)) '(1 2 3 4 5)) '(2 4 6 8 10)))

  (check (equal? (append-map identity '()) '()))
  (check (equal? (append-map! identity '()) '()))
  (check (equal? (append-map error '()) '()))
  (check (equal? (append-map! error '()) '()))

  (check (equal? (append-map identity '(() ())) '()))
  (check (equal? (append-map! identity '(() ())) '()))

  (check (runtime-error? (append-map identity '(1 2 3 4 5))))
  (check (runtime-error? (append-map! identity '(1 2 3 4 5))))

  (check (equal? (append-map identity (map cons '(1 2 3 4 5)))  '(1 2 3 4 5)))
  (check (equal? (append-map! identity (map cons '(1 2 3 4 5))) '(1 2 3 4 5)))

  (check (equal? (append-map identity (map #L(list _ (* _ 2)) '(1 2 3 4 5)))  
                     '(1 2 2 4 3 6 4 8 5 10)))

  (check (equal? (append-map! identity (map #L(list _ (* _ 2)) '(1 2 3 4 5)))  
                     '(1 2 2 4 3 6 4 8 5 10)))

  (check (equal? (append-map #L2(list _0 _1) '(1 2 3 4 5) '(2 4 6 8 10))
                     '(1 2 2 4 3 6 4 8 5 10)))

  (check (equal? (append-map! #L2(list _0 _1) '(1 2 3 4 5) '(2 4 6 8 10))
                     '(1 2 2 4 3 6 4 8 5 10)))

  (check (equal? (append-map cons '(1 2 3 4 5)) '(1 2 3 4 5)))
  (check (equal? (append-map! cons '(1 2 3 4 5)) '(1 2 3 4 5)))

  (check (equal? (map + '(1 2 3) '(4 5 6)) '(5 7 9))))


 
(define-test map-pair-1
  (check (equal? '() (map-pair even? '())))
  (check (list? (map-pair identity '(1 2 3 4 5))))
  (check (equal? '(#f #t #f #t #f) (map-pair (lambda (x) (even? (car x))) '(1 2 3 4 5))))
  (check (runtime-error? (map-pair () '(1 2 3 4 5))))
  (check (runtime-error? (map-pair even? [1 2 3 4 5])))

  (check (equal? '() (map-pair identity '())))
  (check (equal? '((0)) (map-pair identity '(0))))
  (check (equal? '((0 1) (1)) (map-pair identity '(0 1))))
  (check (equal? '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5)) (map-pair identity '(1 2 3 4 5))))

  (check (not (runtime-error? (map-pair (lambda (x) (error "Test Error")) '()))))
  (check (runtime-error? (map-pair (lambda (x) (error "Test Error")) '(1))))

  (check
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
  (check (equal? '() (map-pair + '() '())))
  (check (list? (map-pair cons '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (check (equal? '(((1 3 5 7 9) 0 -1 -2 -3 -4) ((3 5 7 9) -1 -2 -3 -4) ((5 7 9) -2 -3 -4) ((7 9) -3 -4) ((9) -4))
                 (map-pair cons '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (check (equal? ' (((1 3 5 7 9) 0 -1 -2) ((3 5 7 9) -1 -2) ((5 7 9) -2))
                 (map-pair cons '(1 3 5 7 9) '(0 -1 -2))))
  (check (equal? '(((1 3 5) 0 -1 -2 -3 -4) ((3 5) -1 -2 -3 -4) ((5) -2 -3 -4))
                 (map-pair cons '(1 3 5) '(0 -1 -2 -3 -4))))
  (check (list? (map-pair identity '(1 3 5 7 9) '(0 -1 -2 -3 -4))))
  (check (equal? '((1 3 5 7 9) (3 5 7 9) (5 7 9) (7 9) (9)) (map-pair identity '(1 3 5 7 9) '(0 -1 -2 -3 -4))))

  (check (runtime-error? (map-pair ()  '(1 3 5 7 9) '(0 -1 -2 -3 -4))))

  (check (not (runtime-error? (map-pair (lambda (x) (error "Test Error")) '() '()))))
  (check (not (runtime-error? (map-pair (lambda (x) (error "Test Error")) '(1) '()))))
  (check (not (runtime-error? (map-pair (lambda (x) (error "Test Error")) '() '(2)))))
  (check (runtime-error? (map-pair (lambda (x) (error "Test Error")) '(1) '(2))))

  (check
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
