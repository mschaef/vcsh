(define-package "test-hash"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test hash-set!
  (let ((h (make-identity-hash)))
    (check (runtime-error? (hash-set! :not-a-hash 'foo 'bar)))

    (check (eq? h (hash-set! h :foo-1 :bar-1)))
    (check (eq? h (hash-set! h :foo-2 :bar-2)))
    (check (eq? h (hash-set! h :foo-1 :bar-3)))

    (check (eq? :bar-3 (hash-ref h :foo-1 :not-found)))
    (check (eq? :bar-2 (hash-ref h :foo-2 :not-found))))

  (let ((h (make-hash)))
    (check (eq? h (hash-set! h :foo-1 :bar-1)))
    (check (eq? h (hash-set! h :foo-2 :bar-2)))
    (check (eq? h (hash-set! h :foo-1 :bar-3)))

    (check (eq? :bar-3 (hash-ref h :foo-1 :not-found)))
    (check (eq? :bar-2 (hash-ref h :foo-2 :not-found)))))

(define-test hash-has?
  (let ((h (make-identity-hash)))
    (check (not (hash-has? h 'foo)))
    (hash-set! h 'foo 'bar)
    (check (eq? h (hash-has? h 'foo))))

  (let ((h (make-hash)))
    (check (not (hash-has? h 'foo)))
    (hash-set! h 'foo 'bar)
    (check (eq? h (hash-has? h 'foo)))))

(define-test hash-ref
  (define (populate-hash! h)
    (hash-set! h 'a 0)
    (hash-set! h 'b 1)
    (hash-set! h 'c 2)
    (hash-set! h 'd 3)
    (hash-set! h 'e 4))

  (check (runtime-error? (hash-has? :not-a-hash-table 11)))
  (check (runtime-error? (hash-ref  :not-a-hash-table 11)))
  (check (runtime-error? (hash-ref*  :not-a-hash-table 11)))

  (let ((h (make-hash))
        (h2 (make-hash)))

    (check (can-read/write-round-trip? h))

    (check (eq? (type-of h) 'hash))
    (check (hash? h))
    
    (check (not (hash-ref h 'foo)))

    (populate-hash! h)
    (populate-hash! h2)
    (check (equal? h h2))

    (check (eq? (hash-has? h 'a) h))
    (check (eq? (hash-has? h 'b) h))
    (check (eq? (hash-has? h 'c) h))
    (check (eq? (hash-has? h 'd) h))
    (check (eq? (hash-has? h 'e) h))

    (check (eq? (hash-ref h 'a) 0))
    (check (eq? (hash-ref h 'b) 1))
    (check (eq? (hash-ref h 'c) 2))
    (check (eq? (hash-ref h 'd) 3))
    (check (eq? (hash-ref h 'e) 4))
    (check (eq? (hash-ref h :not-there) #f))

    (check (equal? (hash-ref* h 'a) '(a . 0)))
    (check (equal? (hash-ref* h 'b) '(b . 1)))
    (check (equal? (hash-ref* h 'c) '(c . 2)))
    (check (equal? (hash-ref* h 'd) '(d . 3)))
    (check (equal? (hash-ref* h 'e) '(e . 4)))
    (check (equal? (hash-ref* h :not-there) #f))

     ; test hash key removal
    (check (eq? (hash-remove! h 'c) h))
    (check (not (hash-has? h 'c)))

    (check (eq? (hash-ref h 'a) 0))
    (check (eq? (hash-ref h 'b) 1))
    (check (eq? (hash-ref h 'c) #f))
    (check (eq? (hash-ref h 'd) 3))
    (check (eq? (hash-ref h 'e) 4))

    (check (eq? (hash-remove! h 'a) h))
    (check (not (hash-has? h 'a)))
    (check (not (hash-has? h 'c)))

    (check (eq? (hash-ref h 'a) #f))
    (check (eq? (hash-ref h 'b) 1))
    (check (eq? (hash-ref h 'c) #f))
    (check (eq? (hash-ref h 'd) 3))
    (check (eq? (hash-ref h 'e) 4))

    (check (eq? (hash-remove! h 'e) h))
    (check (not (hash-has? h 'a)))
    (check (not (hash-has? h 'e)))
    (check (not (hash-has? h 'j)))

    (check (eq? (hash-ref h 'a) #f))
    (check (eq? (hash-ref h 'b) 1))
    (check (eq? (hash-ref h 'c) #f))
    (check (eq? (hash-ref h 'd) 3))
    (check (eq? (hash-ref h 'e) #f))))

(define-test hash-ref-star
  (let ((h (make-hash))
	(k '(h e l l o - w o r l d)))
    (hash-set! h k :test-symbol)
   
    (let ((k/v (hash-ref* h '(h e l l o - w o r l d))))
      (check (pair? k/v))
      (check (eq? (car k/v) k))
      (check (eq? (cdr k/v) :test-symbol)))))

(define-test hash-length
  (let ((h/eq    (make-identity-hash))
        (h/equal (make-hash)))
    (check (= 0 (length h/eq)))
    (check (= 0 (length h/equal)))
    
    (hash-set! h/eq    'x 1)
    (hash-set! h/equal 'x 1)

    (check (= 1 (length h/eq)))
    (check (= 1 (length h/equal)))

    (hash-set! h/eq    'x 2)
    (hash-set! h/equal 'x 2)

    (check (= 1 (length h/eq)))
    (check (= 1 (length h/equal)))

    (hash-set! h/eq    'y 3)
    (hash-set! h/equal 'y 3)

    (check (= 2 (length h/eq)))
    (check (= 2 (length h/equal)))

    (hash-remove! h/eq    'y)
    (hash-remove! h/equal 'y)

    (check (= 1 (length h/eq)))
    (check (= 1 (length h/equal)))

    (hash-set! h/eq 'foo 12)
    (hash-set! h/equal 'foo 12)

    (check (= 2 (length h/eq)))
    (check (= 2 (length h/equal)))

    (hash-clear! h/eq)
    (hash-clear! h/equal)

    (check (= 0 (length h/eq)))
    (check (= 0 (length h/equal)))))

(define-test hash-clear!
  (let ((h (make-hash)))
    (check (runtime-error? (hash-clear! :not-a-hash)))
    (hash-set! h 'foo 'bar)

    (check (hash-has? h 'foo))

    (hash-clear! h)

    (check (not (hash-has? h 'foo)))
    (check (= 0 (length h)))))

(define-test list->hash
  (check (runtime-error? (list->hash :not-a-list)))

  (check (hash? (list->hash ())))
  (check (not (identity-hash? (list->hash ()))))

  (check (not (identity-hash? (list->hash '(c d)))))

  (let ((h (list->hash '(a b c d))))
    (check (hash-has? h 'a))
    (check (eq? (hash-ref h 'a) 'b))
    
    (check (hash-has? h 'c))
    (check (eq? (hash-ref h 'c) 'd)))

  (check (runtime-error? (list->hash '(a b c . d))))
  (check (runtime-error? (list->hash '(:invalid-binding)))))

(define-test hash-big
  (let ((h (make-hash)))
    (dotimes (ii 100000)
      (hash-set! h ii ii))

    (let ((big-hash-has-all-elements? #t))
      (dotimes (ii 100000)
        (unless (and (hash-has? h ii)
                     (= (hash-ref h ii) ii))
          (set! big-hash-has-all-elements? #f)))

      (check big-hash-has-all-elements?))))

(define-test identity-hash?
  (check (identity-hash? (make-identity-hash)))
  (check (not (identity-hash? (make-hash))))
  (check (not (identity-hash? {})))

  (check (not (identity-hash? 0)))
  (check (not (identity-hash? 0.0)))
  (check (not (identity-hash? 0.0+0.0i)))
  (check (not (identity-hash? #f)))
  (check (not (identity-hash? 'a))))

(define-test hash-copy
  (let* ((a (identity-hash :a 1 :b 2 :c 3 :d 4))
         (b (hash-copy a))
         (c '{a 1 b 2 c 3 d 4})
         (d (hash-copy c)))
    (check (not (eq? a b)))
    (check (equal? a b))

    (check (not (eq? c d)))
    (check (equal? c d))))

(define-test hash-equal?
  (let ((h1 (identity-hash :a :b))
        (h1a (identity-hash :a :b))
        (h2 (identity-hash :a :b :c :d))
        (h2a (identity-hash :a :b :c :d)))

    (check (equal? h1 h1))
    (check (equal? h1 h1a))
    (check (equal? h1a h1))

    (check (equal? h2 h2))
    (check (equal? h2 h2a))
    (check (equal? h2a h2))

    (check (not (equal? h1 h2)))
    (check (not (equal? h2 h1))))

  (let ((h1 {"a" "b"})
        (h1a {"a" "b"})
        (h2 {"a" "b" "c" "d"})
        (h2a {"a" "b" "c" "d"}))

    (check (equal? h1 h1))
    (check (equal? h1 h1a))
    (check (equal? h1a h1))

    (check (equal? h2 h2))
    (check (equal? h2 h2a))
    (check (equal? h2a h2))

    (check (not (equal? h1 h2)))
    (check (not (equal? h2 h1)))))

(define-test hash-table-complex-key
  (let ((h (make-hash))
        (h2 (make-hash))
        (k  '(h e l l o - w o r l d))
        (k2 [3 1 4 1 5 9 2 6 5 2 5]))

    (hash-set! h   k  'foo)
    (hash-set! h   k2 'bar)
    (hash-set! h2  k  'foo)
    (hash-set! h2  k2 'bar)

    (check (eq? (hash-ref h k ) 'foo))
    (check (eq? (hash-ref h k2) 'bar))

    (check (eq? (hash-ref h  k ) 'foo))
    (check (eq? (hash-ref h  k2) 'bar))
    (check (eq? (hash-ref h2 k ) 'foo))
    (check (eq? (hash-ref h2 k2) 'bar))

    (check (equal? h h2))

    (check (eq? (hash-remove! h k) h))

    (check (eq? (hash-ref h  k ) #f))
    (check (eq? (hash-ref h  k2) 'bar))
    (check (eq? (hash-ref h2 k ) 'foo))
    (check (eq? (hash-ref h2 k2) 'bar))

    (check (not (equal? h h2)))))

(define-test hash-sxhash
  (check (= (sxhash {}) (sxhash {})))
  (check (not (= (sxhash-identity {}) (sxhash-identity {}))))

  (check (= (sxhash 42) (sxhash 42)))
  (check (= (sxhash-identity 42) (sxhash-identity 42))))

(define-test hash-subr-keys
  (let ((h (make-hash)))
    (hash-set! h car 'car)
    (hash-set! h cdr 'cdr)

    (check (eq? (hash-ref h car) 'car))
    (check (eq? (hash-ref h cdr) 'cdr))))

(define (can-retrieve-hash-key? key)
  (equal? :test-value (hash-ref {key :test-value} key)))

(define-test hash-flonum-keys
  (check (can-retrieve-hash-key? #inan   ))
  (check (can-retrieve-hash-key? #ineginf))
  (check (can-retrieve-hash-key? #iposinf))
  (check (can-retrieve-hash-key? -1e19   ))
  (check (can-retrieve-hash-key? -1e9    ))
  (check (can-retrieve-hash-key? -19     ))
  (check (can-retrieve-hash-key? -1.9    ))
  (check (can-retrieve-hash-key? -1.1    ))
  (check (can-retrieve-hash-key? -1.0    ))
  (check (can-retrieve-hash-key? -1e-9   ))
  (check (can-retrieve-hash-key? -0.1    ))
  (check (can-retrieve-hash-key?  0.0    ))
  (check (can-retrieve-hash-key?  0.1    ))
  (check (can-retrieve-hash-key?  1e-9   ))
  (check (can-retrieve-hash-key?  1.1    ))
  (check (can-retrieve-hash-key?  1.9    ))
  (check (can-retrieve-hash-key?  19     ))
  (check (can-retrieve-hash-key?  1e19   )))

(define-test hash-literal
  (let ((key :xyzzy)
        (value 42))
    (check (equal? {:xyzzy 42} {key value})))

  (let ((create-hash-fn (lambda () {})))
    (check (not (eq? (create-hash-fn) (create-hash-fn))))))


(define-test eq-hash-table

  (let ((h1 (make-identity-hash))
        (h2 (make-hash))
        (a '(1 2 3 4 5))
        (b '(1 2 3 4 5))
        (c '(1 2 3 4 5)))
    (hash-set! h1 a 1)
    (hash-set! h1 b 2)
    (hash-set! h1 c 3)
    
    (hash-set! h2 a 1)
    (hash-set! h2 b 2)
    (hash-set! h2 c 3)
    
    (check (eq? (hash-ref h1 a) 1))
    (check (eq? (hash-ref h1 b) 2))
    (check (eq? (hash-ref h1 c) 3))
    
    (check (eq? (hash-ref h2 a) 3))
    (check (eq? (hash-ref h2 b) 3))
    (check (eq? (hash-ref h2 c) 3))
    

    (hash-remove! h1 a)
    (check (eq? (hash-ref h1 a) #f))
    (check (eq? (hash-ref h1 b) 2))
    (check (eq? (hash-ref h1 c) 3))

    (hash-remove! h1 b)
    (check (eq? (hash-ref h1 a) #f))
    (check (eq? (hash-ref h1 b) #f))
    (check (eq? (hash-ref h1 c) 3))

    (hash-remove! h1 c)
    (check (eq? (hash-ref h1 a) #f))
    (check (eq? (hash-ref h1 b) #f))
    (check (eq? (hash-ref h1 c) #f))))

(define-test hash-commutative
  ;; Test commutiativity of internal hash operations. These two
  ;; hash tables are semantically the same, but because 0 and 8
  ;; hash to the same bucket, they wind up having different internal
  ;; representations. They should both be equal? and have the same sxhash
  (let ((a {0 'a 8 'b})
        (b {8 'b 0 'a}))
    (check (not (equal? (scheme::%hash-binding-vector a)
                        (scheme::%hash-binding-vector b))))

    (check (equal? a b))
    (check (equal? (sxhash a) (sxhash b)))))
