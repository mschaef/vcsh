(use-package! "unit-test")

(define-test hash-set!
  (let ((h (make-identity-hash)))
    (test-case (runtime-error? (hash-set! :not-a-hash 'foo 'bar)))

    (test-case (eq? h (hash-set! h :foo-1 :bar-1)))
    (test-case (eq? h (hash-set! h :foo-2 :bar-2)))
    (test-case (eq? h (hash-set! h :foo-1 :bar-3)))

    (test-case (eq? :bar-3 (hash-ref h :foo-1 :not-found)))
    (test-case (eq? :bar-2 (hash-ref h :foo-2 :not-found))))

  (let ((h (make-hash)))
    (test-case (eq? h (hash-set! h :foo-1 :bar-1)))
    (test-case (eq? h (hash-set! h :foo-2 :bar-2)))
    (test-case (eq? h (hash-set! h :foo-1 :bar-3)))

    (test-case (eq? :bar-3 (hash-ref h :foo-1 :not-found)))
    (test-case (eq? :bar-2 (hash-ref h :foo-2 :not-found)))))

(define-test hash-has?
  (let ((h (make-identity-hash)))
    (test-case (not (hash-has? h 'foo)))
    (hash-set! h 'foo 'bar)
    (test-case (eq? h (hash-has? h 'foo))))

  (let ((h (make-hash)))
    (test-case (not (hash-has? h 'foo)))
    (hash-set! h 'foo 'bar)
    (test-case (eq? h (hash-has? h 'foo)))))

(define-test hash-ref
  (define (populate-hash! h)
    (hash-set! h 'a 0)
    (hash-set! h 'b 1)
    (hash-set! h 'c 2)
    (hash-set! h 'd 3)
    (hash-set! h 'e 4))

  (test-case (runtime-error? (hash-has? :not-a-hash-table 11)))
  (test-case (runtime-error? (hash-ref  :not-a-hash-table 11)))
  (test-case (runtime-error? (hash-ref*  :not-a-hash-table 11)))

  (let ((h (make-hash))
        (h2 (make-hash)))

    (test-case (can-read/write-round-trip? h))

    (test-case (eq? (type-of h) 'hash))
    (test-case (hash? h))
    
    (test-case (not (hash-ref h 'foo)))

    (populate-hash! h)
    (populate-hash! h2)
    (test-case (equal? h h2))

    (test-case (eq? (hash-has? h 'a) h) )
    (test-case (eq? (hash-has? h 'b) h))
    (test-case (eq? (hash-has? h 'c) h))
    (test-case (eq? (hash-has? h 'd) h))
    (test-case (eq? (hash-has? h 'e) h))

    (test-case (eq? (hash-ref h 'a) 0))
    (test-case (eq? (hash-ref h 'b) 1))
    (test-case (eq? (hash-ref h 'c) 2))
    (test-case (eq? (hash-ref h 'd) 3))
    (test-case (eq? (hash-ref h 'e) 4))
    (test-case (eq? (hash-ref h :not-there) #f))

    (test-case (equal? (hash-ref* h 'a) '(a . 0)))
    (test-case (equal? (hash-ref* h 'b) '(b . 1)))
    (test-case (equal? (hash-ref* h 'c) '(c . 2)))
    (test-case (equal? (hash-ref* h 'd) '(d . 3)))
    (test-case (equal? (hash-ref* h 'e) '(e . 4)))
    (test-case (equal? (hash-ref* h :not-there) #f))

     ; test hash key removal
    (test-case (eq? (hash-remove! h 'c) h))
    (test-case (not (hash-has? h 'c)))

    (test-case (eq? (hash-ref h 'a) 0))
    (test-case (eq? (hash-ref h 'b) 1))
    (test-case (eq? (hash-ref h 'c) #f))
    (test-case (eq? (hash-ref h 'd) 3))
    (test-case (eq? (hash-ref h 'e) 4))

    (test-case (eq? (hash-remove! h 'a) h))
    (test-case (not (hash-has? h 'a)))
    (test-case (not (hash-has? h 'c)))

    (test-case (eq? (hash-ref h 'a) #f))
    (test-case (eq? (hash-ref h 'b) 1))
    (test-case (eq? (hash-ref h 'c) #f))
    (test-case (eq? (hash-ref h 'd) 3))
    (test-case (eq? (hash-ref h 'e) 4))

    (test-case (eq? (hash-remove! h 'e) h))
    (test-case (not (hash-has? h 'a)))
    (test-case (not (hash-has? h 'e)))
    (test-case (not (hash-has? h 'j)))

    (test-case (eq? (hash-ref h 'a) #f))
    (test-case (eq? (hash-ref h 'b) 1))
    (test-case (eq? (hash-ref h 'c) #f))
    (test-case (eq? (hash-ref h 'd) 3))
    (test-case (eq? (hash-ref h 'e) #f))
    ))

(define-test hash-ref-star
  (let ((h (make-hash))
	(k '(h e l l o - w o r l d)))
    (hash-set! h k :test-symbol)
   
    (let ((k/v (hash-ref* h '(h e l l o - w o r l d))))
      (test-case (pair? k/v))
      (test-case (eq? (car k/v) k))
      (test-case (eq? (cdr k/v) :test-symbol)))))

(define-test hash-length
  (let ((h/eq    (make-identity-hash))
        (h/equal (make-hash)))
    (test-case (= 0 (length h/eq)))
    (test-case (= 0 (length h/equal)))
    
    (hash-set! h/eq    'x 1)
    (hash-set! h/equal 'x 1)

    (test-case (= 1 (length h/eq)))
    (test-case (= 1 (length h/equal)))

    (hash-set! h/eq    'x 2)
    (hash-set! h/equal 'x 2)

    (test-case (= 1 (length h/eq)))
    (test-case (= 1 (length h/equal)))

    (hash-set! h/eq    'y 3)
    (hash-set! h/equal 'y 3)

    (test-case (= 2 (length h/eq)))
    (test-case (= 2 (length h/equal)))

    (hash-remove! h/eq    'y)
    (hash-remove! h/equal 'y)

    (test-case (= 1 (length h/eq)))
    (test-case (= 1 (length h/equal)))

    (hash-set! h/eq 'foo 12)
    (hash-set! h/equal 'foo 12)

    (test-case (= 2 (length h/eq)))
    (test-case (= 2 (length h/equal)))

    (hash-clear! h/eq)
    (hash-clear! h/equal)

    (test-case (= 0 (length h/eq)))
    (test-case (= 0 (length h/equal)))
    ))

(define-test hash-clear!
  (let ((h (make-hash)))
    (test-case (runtime-error? (hash-clear! :not-a-hash)))
    (hash-set! h 'foo 'bar)

    (test-case (hash-has? h 'foo))

    (hash-clear! h)

    (test-case (not (hash-has? h 'foo)))
    (test-case (= 0 (length h)))
    ))

(define-test list->hash
  (test-case (runtime-error? (list->hash :not-a-list)))

  (test-case (hash? (list->hash ())))
  (test-case (not (identity-hash? (list->hash ()))))

  (test-case (not (identity-hash? (list->hash '(c d)))))

  (let ((h (list->hash '(a b c d))))
    (test-case (hash-has? h 'a))
    (test-case (eq? (hash-ref h 'a) 'b))
    
    (test-case (hash-has? h 'c))
    (test-case (eq? (hash-ref h 'c) 'd)))

  (test-case (runtime-error? (list->hash '(a b c . d))))
  (test-case (runtime-error? (list->hash '(:invalid-binding)))))

(define-test hash-big
  (let ((h (make-hash)))
    (dotimes (ii 100000)
      (hash-set! h ii ii))

    (let ((big-hash-has-all-elements? #t))
      (dotimes (ii 100000)
        (unless (and (hash-has? h ii)
                     (= (hash-ref h ii) ii))
          (set! big-hash-has-all-elements? #f)))

      (test-case big-hash-has-all-elements?))))

(define-test identity-hash?
  (test-case (identity-hash? (make-identity-hash)))
  (test-case (not (identity-hash? (make-hash))))
  (test-case (not (identity-hash? {})))

  (test-case (not (identity-hash? 0)))
  (test-case (not (identity-hash? 0.0)))
  (test-case (not (identity-hash? 0.0+0.0i)))
  (test-case (not (identity-hash? #f)))
  (test-case (not (identity-hash? 'a))))

(define-test hash-copy
  (let* ((a (identity-hash :a 1 :b 2 :c 3 :d 4))
         (b (hash-copy a))
         (c {a 1 b 2 c 3 d 4})
         (d (hash-copy c)))
    (test-case (not (eq? a b)))
    (test-case (equal? a b))

    (test-case (not (eq? c d)))
    (test-case (equal? c d))))

(define-test hash-equal?
  (let ((h1 (identity-hash :a :b))
        (h1a (identity-hash :a :b))
        (h2 (identity-hash :a :b :c :d))
        (h2a (identity-hash :a :b :c :d)))

    (test-case (equal? h1 h1))
    (test-case (equal? h1 h1a))
    (test-case (equal? h1a h1))

    (test-case (equal? h2 h2))
    (test-case (equal? h2 h2a))
    (test-case (equal? h2a h2))

    (test-case (not (equal? h1 h2)))
    (test-case (not (equal? h2 h1))))

  (let ((h1 {"a" "b"})
        (h1a {"a" "b"})
        (h2 {"a" "b" "c" "d"})
        (h2a {"a" "b" "c" "d"}))

    (test-case (equal? h1 h1))
    (test-case (equal? h1 h1a))
    (test-case (equal? h1a h1))

    (test-case (equal? h2 h2))
    (test-case (equal? h2 h2a))
    (test-case (equal? h2a h2))

    (test-case (not (equal? h1 h2)))
    (test-case (not (equal? h2 h1)))))

(define-test hash-table-complex-key
  (let ((h (make-hash))
        (h2 (make-hash))
        (k  '(h e l l o - w o r l d))
        (k2 [3 1 4 1 5 9 2 6 5 2 5]))

    (hash-set! h   k  'foo)
    (hash-set! h   k2 'bar)
    (hash-set! h2  k  'foo)
    (hash-set! h2  k2 'bar)

    (test-case (eq? (hash-ref h k ) 'foo))
    (test-case (eq? (hash-ref h k2) 'bar))

    (test-case (eq? (hash-ref h  k ) 'foo))
    (test-case (eq? (hash-ref h  k2) 'bar))
    (test-case (eq? (hash-ref h2 k ) 'foo))
    (test-case (eq? (hash-ref h2 k2) 'bar))

    (test-case (equal? h h2))

    (test-case (eq? (hash-remove! h k) h))

    (test-case (eq? (hash-ref h  k ) #f))
    (test-case (eq? (hash-ref h  k2) 'bar))
    (test-case (eq? (hash-ref h2 k ) 'foo))
    (test-case (eq? (hash-ref h2 k2) 'bar))

    (test-case (not (equal? h h2)))))


(define-test hash-sxhash
  (let ((h/eq (make-identity-hash))
        (h/equal (make-hash)))

    (test-case (runtime-error? (sxhash 'foo :not-a-hash)))
    (test-case (not (= (sxhash "foo" h/eq) (sxhash "foo" h/equal))))))
    
(define-test hash-subr-keys
  (let ((h (make-hash)))
    (hash-set! h car 'car)
    (hash-set! h cdr 'cdr)

    (test-case (eq? (hash-ref h car) 'car))
    (test-case (eq? (hash-ref h cdr) 'cdr))))
    
(define-test hash-flonum-keys
  (let ((h (make-hash)))
    (hash-set! h #inan    #inan   )
    (hash-set! h #ineginf #ineginf)
    (hash-set! h #iposinf #iposinf)
    (hash-set! h -1e19    -1e19   )
    (hash-set! h -1e9     -1e9    )
    (hash-set! h -19      -19     )
    (hash-set! h -1.9     -1.9    )
    (hash-set! h -1.1     -1.1    )
    (hash-set! h -1.0     -1.0    )
    (hash-set! h -1e-9    -1e-9   )
    (hash-set! h -0.1     -0.1    )
    (hash-set! h  0.0      0.0    )
    (hash-set! h  0.1      0.1    )
    (hash-set! h  1e-9     1e-9   )
    (hash-set! h  1.1      1.1    )      
    (hash-set! h  1.9      1.9    )
    (hash-set! h  19       19     )
    (hash-set! h  1e19     1e19   )

    (test-case (equal? (hash-ref h #inan   )  #inan  ))
    (test-case (equal? (hash-ref h #ineginf) #ineginf))
    (test-case (equal? (hash-ref h #iposinf) #iposinf))
    (test-case (equal? (hash-ref h -1e19   ) -1e19   ))
    (test-case (equal? (hash-ref h -1e9    ) -1e9    ))
    (test-case (equal? (hash-ref h -19     ) -19     ))
    (test-case (equal? (hash-ref h -1.9    ) -1.9    ))
    (test-case (equal? (hash-ref h -1.1    ) -1.1    ))
    (test-case (equal? (hash-ref h -1.0    ) -1.0    ))
    (test-case (equal? (hash-ref h -1e-9   ) -1e-9   ))
    (test-case (equal? (hash-ref h -0.1    ) -0.1    ))
    (test-case (equal? (hash-ref h  0.0    )  0.0    ))
    (test-case (equal? (hash-ref h  0.1    )  0.1    ))
    (test-case (equal? (hash-ref h  1e-9   )  1e-9   ))
    (test-case (equal? (hash-ref h  1.1    )  1.1    ))
    (test-case (equal? (hash-ref h  1.9    )  1.9    ))
    (test-case (equal? (hash-ref h  19     )  19     ))
    (test-case (equal? (hash-ref h  1e19   )  1e19   ))
    ))
