(define-package "test-vector"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test make-vector/error
  (check (runtime-error? (make-vector 'a '())))
  (check (runtime-error? (make-vector -1 '()))))

(define-test make-vector/result-type
  (check (vector? (make-vector 0 :test-sym)))
  (check (vector? (make-vector 1 :test-sym)))
  (check (vector? (make-vector 10 :test-sym))))

(define-test make-vector/result-length
  (check (= 0 (length (make-vector 0 :test-sym))))
  (check (= 1 (length (make-vector 1 :test-sym))))
  (check (= 10 (length (make-vector 10 :test-sym)))))

(define-test make-vector/result-values
  (let ((vec (make-vector 3 :test-sym)))
    (check (and (eq? (vector-ref vec 0) :test-sym)
                (eq? (vector-ref vec 1) :test-sym)
                (eq? (vector-ref vec 2) :test-sym)))))

(define-test vector-copy
  (check (runtime-error? (vector-copy 'not-a-vector)))

  (let* ((a (vector))
         (b (vector-copy a)))
    (check (not (eq? a b)))
    (check (equal? a b))
    (check (= 0 (length b)))
    (check (equal? [] b)))

  (let* ((a (vector 1))
         (b (vector-copy a)))
    (check (not (eq? a b)))
    (check (equal? a b))
    (check (= 1 (length b)))
    (check (equal? [1] b)))

  (let* ((a (vector 1 2 3))
	 (b (vector-copy a)))
    (check (not (eq? a b)))
    (check (equal? a b))
    (check (= 3 (length b)))
    (check (equal? [1 2 3] b))))
    
(define-test vector-fill!
  (check (runtime-error? (vector-fill! 'not-a-vector 12)))
  (check (not (runtime-error? (vector-fill! [] #f))))
  (check (not (runtime-error? (vector-fill! [0] #f))))
  (check (not (runtime-error? (vector-fill! [0 1 2 3] #f))))

  (let* ((a (vector))
	 (b (vector-fill! a :test)))
    (check (vector? b))
    (check (= (length b) 0))
    (check (eq? a b))
    (check (equal? [] b)))

  (let* ((a (vector 1))
	 (b (vector-fill! a :test)))
    (check (vector? b))
    (check (= (length b) 1))
    (check (eq? a b))
    (check (equal? [:test] b)))

  (let* ((a (vector 1 2 3))
	 (b (vector-fill! a :test)))
    (check (vector? b))
    (check (= (length b) 3))
    (check (eq? a b))
    (check (equal? [:test :test :test] b))))
	 
(define-test vector-ref
  (let ((a (vector :a :b :c :d :e)))
    (check (runtime-error? (vector-ref '(1 2 3 4) 2)))
    (check (runtime-error? (vector-ref a 'sym)))
    (check (runtime-error? (vector-ref a -1)))    
    (check (eq? :a (vector-ref a 0)))
    (check (eq? :a (vector-ref a 0.0)))
    (check (eq? :b (vector-ref a 1)))
    (check (eq? :c (vector-ref a 2)))
    (check (eq? :d (vector-ref a 3.0)))
    (check (eq? :e (vector-ref a 4)))
    (check (runtime-error? (vector-ref a 5)))
    (check (not (runtime-error? (vector-ref a -5 :default))))
    (check (eq? :default (vector-ref a -5 :default)))
    (check (not (runtime-error? (vector-ref a -5.0 :default))))
    (check (eq? :default (vector-ref a -5.0 :default)))
    (check (not (runtime-error? (vector-ref a 5 :default))))
    (check (eq? :default (vector-ref a 5 :default)))
    (check (not (runtime-error? (vector-ref a -5.0 :default))))
    (check (not (runtime-error? (vector-ref a 5.0 :default))))
    (check (eq? :default (vector-ref a -5.0 :default)))
    (check (eq? :default (vector-ref a 5.0 :default)))))


(define-test vector-resize
  (check (runtime-error? (vector-resize 'not-a-vector 12 #f)))
  (check (runtime-error? (vector-resize [1 2] 'not-a-number #f)))
  (check (runtime-error? (vector-resize [1 2] -1 #f)))

  (let* ((a (vector))
         (b (vector-resize a 0 #f)))
    (check (not (eq? a b)))
    (check (= 0 (length b)))
    (check (equal? [] b)))

  (let* ((a (vector))
         (b (vector-resize a 5 #f)))
    (check (not (eq? a b)))
    (check (= 5 (length b)))
    (check (equal? [#f #f #f #f #f] b)))

  (let* ((a (vector #f #f #f #f #f))
         (b (vector-resize a 0 #f)))
    (check (not (eq? a b)))
    (check (= 0 (length b)))
    (check (equal? [] b)))

  (let* ((a (vector #f #f #f #f #f))
         (b (vector-resize a 3 #f)))
    (check (not (eq? a b)))
    (check (= 3 (length b)))
    (check (equal? [#f #f #f] b)))

  (let* ((a (vector #f #f #f #f #f))
         (b (vector-resize a 7 :new-element)))
    (check (not (eq? a b)))
    (check (= 7 (length b)))
    (check (equal? [#f #f #f #f #f :new-element :new-element] b))))

(define-test vector-set!
  (let ((a (vector :a :b :c :d :e)))
    (check (runtime-error? (vector-set! '(1 2 3 4) 2 :foo)))
    (check (runtime-error? (vector-set! a 'sym :foo)))
    (check (runtime-error? (vector-set! a -1 :foo)))
    (check (runtime-error? (vector-set! a 5 :foo)))
    (check (runtime-error? (vector-set! a -1.0 :foo)))
    (check (runtime-error? (vector-set! a 5.0 :foo)))
    (check (equal? [:a :b :c :d :e] a)) ; None of the sets should've been destructive...
    (check (not (runtime-error? (vector-set! a 0 :a-0))))
    (check (eq? :a-0 (vector-ref a 0)))
    (check (not (runtime-error? (vector-set! a 1 :b-0))))
    (check (eq? :b-0 (vector-ref a 1)))
    (check (not (runtime-error? (vector-set! a 2 :c-0))))
    (check (eq? :c-0 (vector-ref a 2)))
    (check (not (runtime-error? (vector-set! a 3 :d-0))))
    (check (eq? :d-0 (vector-ref a 3)))
    (check (not (runtime-error? (vector-set! a 4 :e-0))))
    (check (eq? :e-0 (vector-ref a 4)))
    (check (equal? [:a-0 :b-0 :c-0 :d-0 :e-0] a))
    (check (eq? a (vector-set! a 0 0)))
    (check (not (runtime-error? (vector-set! a 0.0 :a-1))))
    (check (eq? :a-1 (vector-ref a 0)))
    (check (not (runtime-error? (vector-set! a 4.0 :e-1))))
    (check (eq? :e-1 (vector-ref a 4)))))

(define-test vector->list
  (check (runtime-error? (vector->list 'not-a-vector)))
  (check (not (runtime-error? (vector->list []))))
  (check (not (runtime-error? (vector->list [0]))))
  (check (not (runtime-error? (vector->list [0 1 2]))))
  (let ((a (vector->list [])))
    (check (null? a)))

  (let ((a (vector->list [0])))
    (check (list? a))
    (check (= 1 (length a)))
    (check (equal? '(0) a)))

  (let ((a (vector->list [0 1 2])))
    (check (list? a))
    (check (= 3 (length a)))
    (check (equal? '(0 1 2) a))))

(define-test vector
  (let ((a (vector)))
    (check (vector? a))
    (check (= 0 (length a))))
  (let ((a (vector 1 2 3)))
    (check (vector? a))
    (check (= 3 (length a)))
    (check (= (vector-ref a 0) 1))
    (check (= (vector-ref a 1) 2))
    (check (= (vector-ref a 2) 3))))

(define-test vector-literal
  (let ((value 42))
    (check (equal? [42 42] [value value])))

  (let ((create-vector-fn (lambda () [])))
    (check (not (eq? (create-vector-fn) (create-vector-fn))))))

(define-test vector?
  (check (vector? [1 2 3]))
  (check (vector? (vector 1 2 3)))
  (check (vector? (vector-set! [1 2 3] 1 1)))
  (check (vector? (make-vector 10)))
  (check (not (vector? 1)))
  (check (not (vector? (lambda (x) (+ x 1)))))
  (check (not (vector? 'symbol)))
  (check (not (vector? #t)))
  (check (not (vector? #\t)))
  (check (not (vector? "String")))
  (check (not (vector? '(1 2 3)))))

(define-test list->vector
  (check (runtime-error? (list->vector 'foo)))
  (check (runtime-error? (list->vector '(1 2 . improper-list))))
  (check (not (runtime-error? (list->vector '()))))
  (check (not (runtime-error? (list->vector '(1)))))
  (check (not (runtime-error? (list->vector '(1 2 3)))))

  (let ((a (list->vector '())))
    (check (vector? a))
    (check (= 0 (length a))))

  (let ((a (list->vector '(1))))
    (check (vector? a))
    (check (= 1 (length a)))
    (check (equal? [1] a)))

  (let ((a (list->vector '(1 2 3))))
    (check (vector? a))
    (check (= 3 (length a)))
    (check (equal? [1 2 3] a))))

(define-test vector-literal
  (define (test-vec) [1 2 3])

  (check (= 3 (length (test-vec))))
  (check (equal? (test-vec) (vector 1 2 3)))
  (check (not (eq? (test-vec) (test-vec))))

  (check (equal? [ 1 ] [ (identity 1)]))
  (check (equal? [ 1 ] [ (identity-macro 1)])))
