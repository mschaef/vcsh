(define-package "test-lists"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test make-list
  (check (runtime-error? (make-list :non-numeric 0)))
  (check (runtime-error? (make-list -1 0)))

  (check (equal? () (make-list 0 :foo)))
  (check (equal? '(:foo) (make-list 1 :foo)))
  (check (equal? '(:foo :foo :foo) (make-list 3 :foo)))

  (check (= 1000 (length (make-list 1000 ())))))

(define-test dolist
  (check (not (runtime-error? (dolist (n ())))))
  (check (not (runtime-error? (dolist (n () 1)))))
  (check (not (runtime-error? (dolist (n ())))))
  (check (not (runtime-error? (dolist (n [1])))))
  (check (not (runtime-error? (dolist (n [1] 1)))))
  (check (eq? 1 (dolist (n () 1))))
  
  (check
   (equal? '(1 2)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n ())
              (checkpoint n))
            (checkpoint 2))))
  
  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n '(2))
              (checkpoint n))
            (checkpoint 3))))
  
  (check
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n '(2 3))
              (checkpoint n))
            (checkpoint 4))))
  
  (check
   (equal? '(1 2 3 4 5 6)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n '(2 3 4 5))
              (checkpoint n))
            (checkpoint 6))))
  
  (check (runtime-error? (dolist (n ((lambda () (error "Internal error")) '(1 2 3 4 5)))
                           (write n))))
  
  (check (runtime-error? (dolist (n ((lambda () '(1 2 3 4 5))))
                           (error "Internal error")
                           (write n))))
  
  (let ((q (gensym "dolist-test-symbol")))
    (check (eq? q (dolist (n '(1 2 3 4 5) q)
                    ()))))
  
  
  (check (eq? 1 (dolist (n [] 1))))
  
  (check
   (equal? '(1 2)
           (checkpoint-order-of
            (checkpoint 1)
            (dolist (n [])
              (checkpoint n))
            (checkpoint 2))))
  
  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (checkpoint 1)
            (dovec (n [2])
              (checkpoint n))
            (checkpoint 3))))
  
  (check
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (checkpoint 1)
            (dovec (n [2 3])
              (checkpoint n))
            (checkpoint 4))))
  
  (check
   (equal? '(1 2 3 4 5 6)
           (checkpoint-order-of
            (checkpoint 1)
            (dovec (n [2 3 4 5])
              (checkpoint n))
            (checkpoint 6))))
  
  (check (runtime-error? (dovec (n ((lambda () (error "Internal error")) [1 2 3 4 5]))
                               (write n))))
  
  (check (runtime-error? (dovec (n ((lambda () [1 2 3 4 5])))
                               (error "Internal error")
                               (write n))))
  
  (let ((q (gensym "dolist-test-symbol")))
    (check (eq? q (dolist (n [1 2 3 4 5] q)
                    ())))))

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

(define-test any?
  (check (not (any? even? '())))
  (check (runtime-error? (any? even? 'foo)))
  (check (runtime-error? (any? even? 12)))
  (check (any? even? '(1 2 3 4 5)))
  (check (not (any? even? '(1 3 5 7 9))))

  (check (not (any? symbol? '(1 2 3 4 5))))
  (check (any? symbol? '(1 2 3 a)))
  (check (any? symbol? '(a 1 2 3)))
  (check (any? symbol? '(a b c d)))

  (check (eq? 'a (any? symbol? '(a b c d))))
  (check (eq? 'a (any? symbol? '(a 1 2 3))))
  (check (eq? 'a (any? symbol? '(1 2 3 a))))

  (check (eq? 'a (any? symbol? '(a)))))

(define-test every?
  (check (every? even? '()))
  (check (eq? #t (every? even? '())))
   
  (check (runtime-error? (every? even? 'foo)))
  (check (runtime-error? (every? even? 12)))

  (check (every? even? '(2 4 6 8 10)))
  (check (every? even? '(2)))
  (check (not (every? even? '(2 4 6 8 9))))
  (check (not (every? even? '(1 4 6 8 10))))
  (check (not (every? even? '(1 3 5 7 9))))
  (check (not (every? even? '(1))))

  (check (eq? (every? symbol? '(a b c d)) 'd))
  (check (eq? (every? symbol? '(a)) 'a)))

(define-test fold
  (check (runtime-error? (fold + :foo '(1 2 3))))
  (check (runtime-error? (fold + 0 '(:foo 1 2))))
  (check (runtime-error? (fold + 0 '(1 2 :foo))))

  (check (equal? (fold + '() '()) '()))
  (check (equal? (fold + 0 '(1 2 3)) 6))
  (check (equal? (fold cons '() '(1 2 3 4 5)) '(5 4 3 2 1)))
  (check (equal? (fold + :foo '()) :foo))
  (check (equal? (fold list '() '(1 2 3 4 5)) '(5 (4 (3 (2 (1 ()))))))))

(define-test fold-right
  (check (equal? (fold-right + '() '()) '()))
  (check (equal? (fold-right + 0 '(1 2 3)) 6))
  (check (equal? (fold-right cons '() '(1 2 3 4 5)) '(1 2 3 4 5)))
  (check (equal? (fold-right + :foo '()) :foo))
  (check (equal? (fold-right list '() '(1 2 3 4)) '(1 (2 (3 (4 ())))))))

(define-test last-pair

  (check (eq? 12 (last-pair 12)))
  (check (eq? 'a (last-pair 'a)))
  (check (eq? #\a (last-pair #\a)))

  (check (eq? () (last-pair ())))

  (let ((xs '(1)))
    (check (eq? xs (last-pair xs))))

  (let ((xs '(1 2 3)))
    (check (eq? (last-pair xs) (cddr xs))))

  (let ((xs '(1 2 3 . 4)))
    (check (eq? (last-pair xs) (cddr xs)))))

(define-test list-append
  (let ((l1 '(1 2 3))
        (l2 '(4 5 6))
        (l3 '(7))
        (l4 ())
        (l5 '(8 9 10)))

    (check (runtime-error? (append :not-a-list l1)))
    (check (runtime-error? (append l1 :not-a-list l1)))
    (check (runtime-error? (append l1 :not-a-list :not-a-list)))

    (check (equal? (append '(1 2 . 3) l2) '(1 2 4 5 6)))
    (check (equal? '(1 2 3 1 2 . 3) (append l1 '(1 2 . 3))))
    
    (check (eq? () (append l4)))
    (check (eq? () (append l4 l4)))
    (check (eq? () (append l4 l4 l4 l4 l4 l4 l4)))
    
    (check (equal? '(1 2 3 4 5 6) (append l1 l2)))

    (check (equal? '(1 2 3 4 5 6 7 8 9 10) (append l1 l2 l3 l4 l5)))

    (check (equal? '(1 2 3) (append l1 l4)))
    (check (equal? '(1 2 3) (append l4 l1)))

    (check (equal? l1 '(1 2 3)))
    (check (equal? l2 '(4 5 6)))
    (check (equal? l3 '(7)))
    (check (equal? l4 ()))
    (check (equal? l5 '(8 9 10)))))

(define-test list-copy

  (let ((xs (list)))
    (check (eq? () (list-copy xs))))
  
  (check (eq? :atom (list-copy :atom)))
  (check (eq? 12 (list-copy 12)))
  
  (let ((xs (list 1)))
    (check (not (eq? xs (list-copy xs))))
    (check (equal? xs (list-copy xs))))
  
  (let ((xs (list 1 2 3 4 5)))
    (check (not (eq? xs (list-copy xs))))
    (check (equal? xs (list-copy xs))))
  
  (let ((xs `(1 2 3 4 . 5)))
    (check (not (eq? xs (list-copy xs))))
    (check (equal? xs (list-copy xs)))))

(define-test list-duplicates
  (check (runtime-error? (duplicates :not-a-list)))
  (check (runtime-error? (duplicates '(1 . :minimal-improper-list))))
  (check (runtime-error? (duplicates '(1 2 3 4 5 . :longer-improper-list))))

  (check (equal? () (duplicates '())))
  (check (equal? () (duplicates '(0))))
  (check (equal? () (duplicates '(0 1))))

  (check (equal? '(1) (duplicates '(0 1 1))))

  (check (equal? '(1) (duplicates '(0 1 1 1))))

  (check (equal? '(1) (duplicates '(1 0 1 1 1 2 3 4 1))))

  (check (equal? '(y z) (duplicates '(x y z z y))))

  (check (equal? () (duplicates (iseq 1 10))))

  (check (equal? '(1) (duplicates (cons 1 (iseq 1 10)))))

  (check (equal? '(1) (duplicates (append (iseq 1 10) (cons 1))))))


;; (define-test delete-duplicates
;;   (check (runtime-error? (delete-duplicates :not-a-list)))
;;   (check (runtime-error? (delete-duplicates '(1 . :minimal-improper-list))))
;;   (check (runtime-error? (delete-duplicates '(1 2 3 4 5 . :longer-improper-list))))

;;   (check (equal? '() (delete-duplicates '())))
;;   (check (equal? '(0) (delete-duplicates '(0))))
;;   (check (equal? '(0 1) (delete-duplicates '(0 1))))

;;   (check (equal? '(0 1) (delete-duplicates '(0 1 1))))

;;   (check (equal? '(0 1) (delete-duplicates '(0 1 1 1))))

;;   (check (equal? '(1 0 2 3 4) (delete-duplicates '(1 0 1 1 1 2 3 4 1))))

;;   (let ((big-list (iseq 1 200)))
;;     (check (equal? big-list (delete-duplicates big-list)))

;;     (check (equal? big-list (cons 1 (delete-duplicates big-list))))

;;     (check (equal? big-list (delete-duplicates (append big-list (cons 1)))))))

(define-test test-list-index
  (check (runtime-error? (list-index (always #t) :not-a-list)))
  (check (runtime-error? (list-index (always #t) '[not a list either])))
  (check (runtime-error? (list-index (always #f) '(x y z z . y))))
  (check (runtime-error? (list-index 1 '(x y z z y))))

  (check (not (list-index even? '(1 3 5 7 9))))
  (check (= 0 (list-index even? '(2 1 3 5 7 9))))
  (check (= 5 (list-index even? '(1 3 5 7 9 2)))))

(define-test list-ref
  (check (eq? :a (list-ref '(:a :b :c :d) 0)))
  (check (eq? :b (list-ref '(:a :b :c :d) 1)))
  (check (eq? :c (list-ref '(:a :b :c :d) 2)))
  (check (eq? :d (list-ref '(:a :b :c :d) 3)))
  
  (check (runtime-error? (list-ref '() 0)))
  (check (runtime-error? (list-ref '() 1)))
  (check (runtime-error? (list-ref '(:a :b) 2)))
  (check (runtime-error? (list-ref '(:a :b) 10)))

  (check (runtime-error? (list-ref 'a 0)))
  (check (runtime-error? (list-ref 'a -1)))
  (check (not (runtime-error? (list-ref '(:a :b :c . :d) 0))))
  (check (not (runtime-error? (list-ref '(:a :b :c . :d) 1))))
  (check (not (runtime-error? (list-ref '(:a :b :c . :d) 2))))
  (check (runtime-error? (list-ref '(:a :b :c . :d) 3))))

(define-test list-set!
  (let ((xs (list-copy '(:a :b :c :d))))
    (check (equal? '(:a :b :c :d) (list-set! xs 0 :a)))
    (check (equal? '(:a :b :c :d) (list-set! xs 1 :b)))
    (check (equal? '(:a :b :c :d) (list-set! xs 2 :c)))
    (check (equal? '(:a :b :c :d) (list-set! xs 3 :d)))

    (check (equal? '(1 2 3 4)
                   (list-set! (list-set! (list-set! (list-set! xs 
                                                               0 1) 
                                                    1 2)
                                         2 3)
                              3 4)))

    (check (runtime-error? (list-set! '() 0 0)))
    (check (runtime-error? (list-set! '(1 2 3) -1 0)))
    (check (runtime-error? (list-set! '(1 2 3) 10 0)))
    (check (runtime-error? (list-set! '(1 2 . 3) 2 0)))))

(define-test list?-dotted-list?
  (check (list? '()))
  (check (list? '(1 2 3)))
  (check (not (list? '(1 2 . 4))))

  (check (not (dotted-list? '())))
  (check (not (dotted-list? '(1 2 3))))
  (check (dotted-list? '(1 2 . 4))))

(define-test reverse
  (check (runtime-error? (reverse :symbol)))

  (check (equal? () (reverse ())))

  (let ((xs (list 1)))
    (check (equal? '(1) (reverse xs)))
    (check (not (eq? xs (reverse xs)))))

  (let ((xs (list 1 2 3)))
    (check (equal? '(3 2 1) (reverse xs)))
    (check (not (eq? xs (reverse xs))))))

(define-test reverse!
  (check (runtime-error? (reverse! :symbol)))

  (check (equal? () (reverse! ())))

  (let ((xs (list 1)))
    (check (equal? '(1) (reverse! xs)))
    (check (eq? xs (reverse! xs))))

  (let ((xs (list 1 2 3)))
    (check (equal? '(3 2 1) (reverse! xs)))
    (check (eq? xs (reverse! xs)))))

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

(define-test empty-list
  (check (equal? ()
                 (delq :anything ()))))

(define-test item-at-beginning
  (check (equal? '(x y z z y)
                 (delq :foo '(:foo x y z z y))))
  (check (equal? '(x y z z y)
                 (delq :foo '(:foo :foo x y z z y)))))

(define-test item-in-middle
  (check (equal? '(x y z z y)
                 (delq :foo '(x :foo y z z y))))
  (check (equal? '(x y z z y)
                 (delq :foo '(x :foo :foo y z z y)))))

(define-test item-at-end
  (check (equal? '(x y z z y)
                 (delq :foo '(x y z z y :foo))))
  (check (equal? '(x y z z y)
                 (delq :foo '(x y z z y :foo :foo)))))

(define-test item-interleaved
  (check (equal? '(x y z z y)
                 (delq :foo '(:foo x :foo y :foo z :foo z :foo y :foo)))))

(define-test entire-list-contents
  (check (equal? ()
                 (delq :foo '(:foo))))
  (check (equal? ()
                 (delq :foo '(:foo :foo)))))

(define-test test-member-index
  (check (runtime-error? (member-index 'x :not-a-list)))
  (check (runtime-error? (member-index 'x '[not a list either])))
  (check (runtime-error? (member-index 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (= 0 (member-index 1 xs)))
    (check (= 1 (member-index :symbol xs)))
    (check (= 2 (member-index '(composite key) xs)))
    (check (= 3 (member-index () xs)))
    (check (= 4 (member-index "string" xs)))
    (check (not (member-index :not-a-member xs))))

  (let ((xs '(1000000)))
    (check (member-index 1000000 xs)))

  (let ((xs '(1 1000000)))
    (check (member-index 1000000 xs))))

(define-test test-member
  (check (runtime-error? (member 'x :not-a-list)))
  (check (runtime-error? (member 'x '[not a list either])))
  (check (runtime-error? (member 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (eq? xs (member 1 xs)))
    (check (eq? (cdr xs) (member :symbol xs)))
    (check (eq? (cddr xs) (member '(composite key) xs)))
    (check (eq? (cdddr xs) (member () xs)))
    (check (eq? (cddddr xs) (member "string" xs)))
    (check (not (member :not-a-member xs))))

  (let ((xs '(1000000)))
    (check (member 1000000 xs)))

  (let ((xs '(1 1000000)))
    (check (member 1000000 xs))))

(define-test test-memq-index
  (check (runtime-error? (memq-index 'x :not-a-list)))
  (check (runtime-error? (memq-index 'x '[not a list either])))
  (check (runtime-error? (memq-index 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (= 0 (memq-index 1 xs)))
    (check (= 1 (memq-index :symbol xs)))
    (check (not (memq-index '(composite key) xs)))
    (check (= 2 (memq-index (caddr xs) xs)))        
    (check (= 3 (memq-index () xs)))
    (check (not (memq-index :not-a-memq-index xs))))

    (let ((xs '(1000000000)))
      (check (= 0 (memq-index 1000000000 xs))))

    (let ((xs '(1 1000000000)))
      (check (= 1 (memq-index 1000000000 xs)))))

(define-test test-memq
  (check (runtime-error? (memq 'x :not-a-list)))
  (check (runtime-error? (memq 'x '[not a list either])))
  (check (runtime-error? (memq 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (eq? xs (memq 1 xs)))
    (check (eq? (cdr xs) (memq :symbol xs)))
    (check (not (memq '(composite key) xs)))
    (check (eq? (cddr xs) (memq (caddr xs) xs)))        
    (check (eq? (cdddr xs) (memq () xs)))
    (check (not (memq :not-a-memq xs))))

    (let ((xs '(1000000000)))
      (check (memq 1000000000 xs)))

    (let ((xs '(1 1000000)))
      (check (not (memq 1000000000 xs)))))

(define-test test-memv-index
  (check (runtime-error? (memv-index 'x :not-a-list)))
  (check (runtime-error? (memv-index 'x '[not a list either])))
  (check (runtime-error? (memv-index 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (= 0 (memv-index 1 xs)))
    (check (= 1 (memv-index :symbol xs)))
    (check (not (memv-index '(composite key) xs)))
    (check (= 2 (memv-index (caddr xs) xs)))        
    (check (= 3 (memv-index () xs)))
    (check (not (memv-index :not-a-memv-index xs)))
    (check (not (memv-index "string" xs))))

  (let ((xs '(1000000)))
    (check (memv-index 1000000 xs)))

  (let ((xs '(1 1000000)))
    (check (memv-index 1000000 xs))))

(define-test test-memv
  (check (runtime-error? (memv 'x :not-a-list)))
  (check (runtime-error? (memv 'x '[not a list either])))
  (check (runtime-error? (memv 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (eq? xs (memv 1 xs)))
    (check (eq? (cdr xs) (memv :symbol xs)))
    (check (not (memv '(composite key) xs)))
    (check (eq? (cddr xs) (memv (caddr xs) xs)))        
    (check (eq? (cdddr xs) (memv () xs)))
    (check (not (memv :not-a-memv xs)))
    (check (not (memv "string" xs))))

  (let ((xs '(1000000)))
    (check (memv 1000000 xs)))

  (let ((xs '(1 1000000)))
    (check (memv 1000000 xs))))

(define-test span
  (mvbind (t d) (span even? ())
    (check (equal? t ()))
    (check (equal? d ())))
    
  (check (runtime-error? (span even? :foo)))
  (check (runtime-error? (span :not-a-procecdure '(1 2 3))))

  (let ((xs '(2 4 8 1 2)))
    (mvbind (t d) (span even? xs)
      (check (equal? t '(2 4 8)))
      (check (equal? d '(1 2)))
      (check (not (eq? t xs)))
      (check (eq? d (cdddr xs)))))

  (let ((xs '(2 4 8 10 2)))
    (mvbind (t d) (span even? xs)
      (check (equal? t '(2 4 8 10 2)))
      (check (equal? d '()))
      (check (not (eq? t xs)))))

  (let ((xs '(21 41 81 11 21)))
    (mvbind (t d) (span even? xs)
      (check (equal? t '()))
      (check (equal? d '(21 41 81 11 21)))
      (check (eq? d xs))))
  
  (let ((xs '(2 4 8 1 . 2)))
    (mvbind (t d) (span even? xs)
      (check (equal? t '(2 4 8)))
      (check (equal? d '(1 . 2)))
      (check (not (eq? t xs)))
      (check (eq? d (cdddr xs)))))
  
  (check (runtime-error? (span (always #t) '(2 4 8 1 . 2)))))

(define-test span!
  (mvbind (t d) (span! even? ())
    (check (equal? t ()))
    (check (equal? d ())))
    
  (check (runtime-error? (span! even? :foo)))
  (check (runtime-error? (span! :not-a-procecdure `(1 2 3))))

  (let* ((xs `(2 4 8 1 2))
         (correct-d (cdddr xs)))
    (mvbind (t d) (span! even? xs)
      (check (equal? t `(2 4 8)))
      (check (equal? d `(1 2)))
      (check (eq? t xs))
      (check (eq? d correct-d))))

  (let ((xs `(2 4 8 10 2)))
    (mvbind (t d) (span! even? xs)
      (check (equal? t `(2 4 8 10 2)))
      (check (equal? d `()))
      (check (eq? t xs))))

  (let ((xs `(21 41 81 11 21)))
    (mvbind (t d) (span! even? xs)
      (check (equal? t `()))
      (check (equal? d `(21 41 81 11 21)))
      (check (eq? d xs))))
  
  (let* ((xs `(2 4 8 1 . 2))
         (correct-d (cdddr xs)))
    (mvbind (t d) (span! even? xs)
      (check (equal? t `(2 4 8)))
      (check (equal? d `(1 . 2)))
      (check (eq? t xs))
      (check (eq? d correct-d))))
  
  (check (runtime-error? (span! (always #t) `(2 4 8 1 . 2)))))

(define-test break
  (mvbind (t d) (break odd? ())
    (check (equal? t ()))
    (check (equal? d ())))
    
  (check (runtime-error? (break odd? :foo)))
  (check (runtime-error? (break :not-a-procecdure '(1 2 3))))

  (let ((xs '(2 4 8 1 2)))
    (mvbind (t d) (break odd? xs)
      (check (equal? t '(2 4 8)))
      (check (equal? d '(1 2)))
      (check (not (eq? t xs)))
      (check (eq? d (cdddr xs)))))

  (let ((xs '(2 4 8 10 2)))
    (mvbind (t d) (break odd? xs)
      (check (equal? t '(2 4 8 10 2)))
      (check (equal? d '()))
      (check (not (eq? t xs)))))

  (let ((xs '(21 41 81 11 21)))
    (mvbind (t d) (break odd? xs)
      (check (equal? t '()))
      (check (equal? d '(21 41 81 11 21)))
      (check (eq? d xs))))
  
  (let ((xs '(2 4 8 1 . 2)))
    (mvbind (t d) (break odd? xs)
      (check (equal? t '(2 4 8)))
      (check (equal? d '(1 . 2)))
      (check (not (eq? t xs)))
      (check (eq? d (cdddr xs)))))
  
  (check (runtime-error? (break (always #f) '(2 4 8 1 . 2)))))

(define-test break!
  (mvbind (t d) (break! odd? ())
    (check (equal? t ()))
    (check (equal? d ())))
    
  (check (runtime-error? (break! odd? :foo)))
  (check (runtime-error? (break! :not-a-procecdure '(1 2 3))))

  (let* ((xs '(2 4 8 1 2))
         (correct-d (cdddr xs)))
    (mvbind (t d) (break! odd? xs)
      (check (equal? t '(2 4 8)))
      (check (equal? d '(1 2)))
      (check (eq? t xs))
      (check (eq? d correct-d))))

  (let ((xs '(2 4 8 10 2)))
    (mvbind (t d) (break! odd? xs)
      (check (equal? t '(2 4 8 10 2)))
      (check (equal? d '()))
      (check (eq? t xs))))

  (let ((xs '(21 41 81 11 21)))
    (mvbind (t d) (break! odd? xs)
      (check (equal? t '()))
      (check (equal? d '(21 41 81 11 21)))
      (check (eq? d xs))))
  
  (let* ((xs '(2 4 8 1 . 2))
         (correct-d (cdddr xs)))
    (mvbind (t d) (break! odd? xs)
      (check (equal? t '(2 4 8)))
      (check (equal? d '(1 . 2)))
      (check (eq? t xs))
      (check (eq? d correct-d))))
  
  (check (runtime-error? (break! (always #f) '(2 4 8 1 . 2)))))

(define-test drop
  (let ((xs `(1 2 3 4 5)))
    (check (eq? xs (drop xs 0)))
    (check (eq? (cdr xs) (drop xs 1)))
    (check (eq? (cddr xs) (drop xs 2)))
    (check (eq? (cdddr xs) (drop xs 3)))
    (check (eq? (cddddr xs) (drop xs 4)))
    (check (eq? '() (drop xs 5))))
  (check (runtime-error? (drop '(1 . 2) 2)))
  (check (runtime-error? (drop :not-a-list 1)))
  (check (not (runtime-error? (drop :not-a-list 0)))))

(define-test drop-while
  (check (equal? (drop-while even? ()) ()))
  (check (runtime-error? (drop-while even? :foo)))
  (check (runtime-error? (drop-while :not-a-procecdure '(1 2 3))))

  (let ((xs '(1 2 3 4 5)))
    (check (equal? (drop-while even? xs) '(1 2 3 4 5)))
    (check (eq? (drop-while even? xs) xs)))

  (let ((xs '(2 4 8 1 2)))
    (check (equal? (drop-while even? xs) '(1 2)))
    (check (eq? (drop-while even? xs) (cdddr xs))))

  (check (equal? (drop-while even? '(2 4 8)) '()))

  (let ((xs '(2 4 8 1 . 2)))
    (check (equal? (drop-while even? xs) '(1 . 2)))
    (check (eq? (drop-while even? xs) (cdddr xs))))

  (check (runtime-error? (drop-while (always #t) '(2 4 8 1 . 2))))

  (check (equal? (drop-while even? '(1 3 5)) '(1 3 5))))

(define-test take-while
  (check (equal? (take-while even? ())))
  (check (runtime-error? (take-while even? :foo)))
  (check (runtime-error? (take-while :not-a-procecdure '(1 2 3))))

  (let ((xs '(2 4 8 1 2)))
    (check (equal? (take-while even? xs) '(2 4 8)))
    (check (not (eq? (take-while even? xs) xs))))

  (let ((xs '(2 4 8 1 2)))
    (check (equal? (take-while even? xs) '(2 4 8)))
    (check (not (eq? (take-while even? xs) xs))))

  (let ((xs '(2 4 8 1 . 2)))
    (check (equal? (take-while even? xs) '(2 4 8)))
    (check (not (eq? (take-while even? xs) xs))))

  (check (runtime-error? (take-while (always #t) '(2 4 8 1 . 2))))

  (check (equal? (take-while even? '(1 3 5)) '()))
  (check (equal? (take-while even? '(1 2 4)) '())))

(define-test take-while!
  (check (equal? (take-while! even? ()) ()))
  (check (runtime-error? (take-while! even? :foo)))
  (check (runtime-error? (take-while! :not-a-procecdure '(1 2 3))))

  (let* ((xs `(2 4 8 1 2))
         (res (take-while! even? xs)))
    (check (equal? res '(2 4 8)))
    (check (eq? res xs)))

  (let* ((xs `(2 4 8 1 2))
         (res (take-while! even? xs)))
    (check (equal? res '(2 4 8)))
    (check (eq? res xs)))

  (let* ((xs `(2 4 8 1 . 2))
         (res (take-while! even? xs)))
    (check (equal? res '(2 4 8)))
    (check (eq? res xs)))

  (check (equal? (take-while! (always #t) `(2 4 8 1 . 2))
                 `(2 4 8 1 . 2)))

  (check (equal? (take-while! even? `(1 3 5)) '()))
  (check (equal? (take-while! even? `(1 2 4)) '())))

(define-test take
  (check (equal? (take () 0) ()))
  (check (runtime-error? (take :foo 1)))
  (check (not (runtime-error? (take :foo 0))))
  (check (runtime-error? (take '() #\a)))
  (check (runtime-error? (take '(1 2 3) #\a)))
  
  (let ((xs `(1 2 3 4 5)))
    (check (equal? (take xs 0) '()))
    (check (not (eq? (take xs 0) xs)))
    (check (equal? (take xs 1) '(1)))
    (check (not (eq? (take xs 1) xs)))
    (check (equal? (take xs 2) '(1 2)))
    (check (not (eq? (take xs 2) xs)))
    (check (equal? (take xs 5) '(1 2 3 4 5)))
    (check (not (eq? (take xs 5) xs)))
    (check (runtime-error? (take xs 6))))

  (let ((xs `(1 2 3 4 . 5)))
    (check (equal? (take xs 0) '()))
    (check (not (eq? (take xs 0) xs)))
    (check (equal? (take xs 1) '(1)))
    (check (not (eq? (take xs 1) xs)))
    (check (equal? (take xs 2) '(1 2)))
    (check (not (eq? (take xs 2) xs)))
    (check (equal? (take xs 4) '(1 2 3 4)))
    (check (not (eq? (take xs 4) xs)))
    (check (runtime-error? (take xs 5)))))

(define-test take!
  (check (not (runtime-error? (take! :foo 0))))
  (check (runtime-error? (take! `() #\a)))
  (check (runtime-error? (take! `(1 2 3) #\a)))
  
  (let* ((xs `(1 2 3 4 5))
         (res (take! xs 0)))
    (check (equal? res '()))
    (check (eq? res ())))
  (let* ((xs `(1 2 3 4 5))
         (res (take! xs 1)))
    (check (equal? res '(1)))
    (check (eq? res xs)))
  (let* ((xs `(1 2 3 4 5))
         (res (take! xs 2)))
    (check (equal? res '(1 2)))
    (check (eq? res xs)))
  (let* ((xs `(1 2 3 4 5))
         (res (take! xs 5)))
    (check (equal? res '(1 2 3 4 5)))
    (check (eq? res xs)))
  
  (let ((xs `(1 2 3 4 5)))
    (check (runtime-error? (take! xs 6))))
  
  (let* ((xs `(1 2 3 4 . 5))
         (res (take! xs 0)))
    (check (equal? res '())))

  (let* ((xs `(1 2 3 4 . 5))
         (res (take! xs 1)))
    (check (equal? res '(1)))
    (check (eq? res xs)))
  (let* ((xs `(1 2 3 4 . 5))
         (res (take! xs 2)))
    (check (equal? res '(1 2)))
    (check (eq? res xs)))
  (let* ((xs `(1 2 3 4 . 5))
         (res (take! xs 4)))
    (check (equal? res '(1 2 3 4)))
    (check (eq? res xs)))
  (let ((xs `(1 2 3 4 . 5)))
    (check (runtime-error? (take! xs 5)))))

(define-test qsort

  (check (runtime-error? (qsort :not-a-list)))
  (check (runtime-error? (qsort (list 1 2 3))))
  (check (runtime-error? (qsort (list 1 2 3) :not-a-function)))
  (check (runtime-error? (qsort (list 1 2 3) < :not-a-function)))
  (check (runtime-error? (qsort (cons 1 (cons 2 3)) <)))


  (check (equal? () (qsort () <)))
  (check (equal? (list 1) (qsort (list 1) <)))
  (check (equal? (list 1 2 3) (qsort (list 1 2 3) <)))
  (check (equal? (list 1 1 2 3) (qsort (list 1 1 2 3) <)))
  (check (equal? (list 3 2 1) (qsort (list 1 2 3) >)))

  (let ((xs (list 54 49 75 69 11 25 66 64 28 18 73 67 42 9 31 61 2 91 81 80)))
    (check (equal? (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91)
                   (qsort xs <)))
    (check (equal? (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91)
                   (qsort (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91) <)))

    (check (equal? (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91)
                   (qsort  (list 91 81 80 75 73 69 67 66 64 61 54 49 42 31 28 25 18 11 9 2) <)))

    (check (equal? xs
                   (list 54 49 75 69 11 25 66 64 28 18 73 67 42 9 31 61 2 91 81 80))))

  (let ((xs (list 1 2 3)))
    (check (not (eq? xs (qsort xs <)))))

  (let ((xs (list "it" "was" "the" "best" "of" "times"
                  "it" "was" "the" "worst" "of" "times")))
    (check (equal? (list  "best" "it" "it" "of" "of" "the" "the"
                          "times" "times" "was" "was" "worst")
                   (qsort xs string<)))

    (check (every? #L(memq _ xs) (qsort xs string<))))
  
  (let ((xs (map cons (list "it" "was" "the" "best" "of" "times"
                            "it" "was" "the" "worst" "of" "times"))))
    (check (equal? (map cons (list  "best" "it" "it" "of" "of" "the" "the"
                                    "times" "times" "was" "was" "worst"))
                        (qsort xs string< car)))
    
    (check (every? #L(memq _ xs) (qsort xs string< car)))))

(define-test p-list-fold
  (define (fold/->a-list name value list)
    "This is a procedure that can be passed into p-list-fold to convert p-list
     name/value pairs into pairs for a-list. Note that the resulting a-list is
     in reverse order."
    (cons (cons name value) list))

  (check (eq? :test (p-list-fold + :test ())))
  (check (runtime-error? (p-list-fold + 0 '(:non-numeric :non-numeric))))
  (check (runtime-error? (p-list-fold (lambda (n v xs) (error "foo")) () '(1 2))))
  (check (not (runtime-error? (p-list-fold (lambda (n v xs) (error "foo")) () ()))))
  (check (runtime-error? (p-list-fold fold/->a-list () :not-a-list)))

  (check (equal? '((:baz . 3) (:bar . 2) (:foo . 1))
                     (p-list-fold fold/->a-list () '(:foo 1 :bar 2 :baz 3))))

  (check (equal? '((:baz) (:bar . 2) (:foo . 1))
                       (p-list-fold fold/->a-list () '(:foo 1 :bar 2 :baz))))

  (check (equal? () (p-list-fold fold/->a-list () ()))))

(define-test pair-fold-right
  (check (equal? (pair-fold-right + :foo '()) :foo))
  (check (equal? (pair-fold-right cons '() '(a b c)) '((a b c) (b c) (c)))))

(define-test pair-fold
  (check (equal? (pair-fold + '() '()) '()))
  (check (equal? (pair-fold cons '() '(1 2 3 4 5)) '((5) (4 5) (3 4 5) (2 3 4 5) (1 2 3 4 5))))
  (check (equal? (pair-fold + :foo '()) :foo)))

(define-test pop!
  (let* ((x '(:foo :bar :baz))
         (y x))
    (check (eq? (pop! x) :foo))
    (check (equal? x (cdr y)))
    (check (eq? (pop! x) :bar))
    (check (equal? x (cddr y)))
    (check (eq? (pop! x) :baz))
    (check (equal? x (cdddr y)))
    (check (equal? y '(:foo :bar :baz)))))

(define-test queue
  (check (runtime-error? (q-items :not-a-queue)))
  (check (runtime-error? (q-enqueue! 'q :not-a-queue)))
  (check (runtime-error? (q-dequeue! :not-a-queue)))
  (check (runtime-error? (q-empty? :not-a-queue)))

  (check (runtime-error? (q-dequeue! (make-queue))))

  (check (not (queue? :not-a-queue)))
  (check (queue? (make-queue)))

  (let* ((q (make-queue))
         (q0 q))

    (check (equal? () (q-items q)))
    (check (q-empty? q))

    (q-enqueue! 1 q)

    (check (eq? q q0))    
    (check (queue? q))
    (check (equal? '(1) (q-items q)))
    (check (eq? (q-items q) (q-items q)))

    (check (eq? 1 (q-dequeue! q)))
    (check (queue? q))
    (check (equal? () (q-items q)))
    (check (q-empty? q))
    
    (q-enqueue! 1 q)
    (q-enqueue! 2 q)

    (check (eq? q q0))    
    (check (queue? q))
    (check (equal? '(1 2) (q-items q)))
    (check (eq? (q-items q) (q-items q)))

    (check (eq? 1 (q-dequeue! q)))
    (check (eq? 2 (q-dequeue! q)))

    (check (queue? q))
    (check (equal? () (q-items q)))    
    (check (q-empty? q))))
