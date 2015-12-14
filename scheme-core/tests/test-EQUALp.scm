(use-package! "unit-test")

(define-test EQUAL?-atom
  (check (EQUAL? 1 1))
  (check (EQUAL? 1.0 1.0))
  (check (EQUAL? #\a #\a))
  (check (EQUAL? "" ""))
  (check (EQUAL? "hello" "hello"))
  (check (EQUAL? 'a 'a))
  (check (EQUAL? :key1 :key1))
  (check (EQUAL? #t #t))
  (check (EQUAL? #f #f))
  (check (EQUAL? () ()))
  (check (not (EQUAL? 1 2)))
  (check (not (EQUAL? 1.0 1.2)))
  (check (not (EQUAL? #\a #\b)))
  (check (not (EQUAL? "" "foo")))
  (check (not (EQUAL? "foo" "")))
  (check (not (EQUAL? "hello" "world")))
  (check (not (EQUAL? 'a 'b)))
  (check (not (EQUAL? :key1 :key3)))
  (check (not (EQUAL? #f #t)))
  (check (not (EQUAL? () 12))))

;;; lists

(define-test EQUAL?-simple-list
  (check (EQUAL? '(1) '(1)))
  (check (EQUAL? '(1 2) '(1 2)))
  (check (EQUAL? '(1 . 2) '(1 . 2)))
  (check (EQUAL? '(1 2 3) '(1 2 3)))
  (check (EQUAL? '(1 2 3 . 4) '(1 2 3 . 4)))
  (check (EQUAL? '(1 2 3 4 5 6 7 8 9 10) '(1 2 3 4 5 6 7 8 9 10)))
  (check (not (EQUAL? '(1) '(2))))
  (check (not (EQUAL? '(1 2) '(3 2))))
  (check (not (EQUAL? '(1 2) '(1 3))))
  (check (not (EQUAL? '(1 . 2) '(3 . 2))))
  (check (not (EQUAL? '(1 . 3) '(1 . 2))))
  (check (not (EQUAL? '(1 2 3) '(2 2 3))))
  (check (not (EQUAL? '(1 2 3) '(1 3 3))))
  (check (not (EQUAL? '(1 2 3) '(1 2 4))))
  (check (not (EQUAL? '(1 2 3 4 5 6 7 8 9 11) '(1 2 3 4 5 6 7 8 9 10)))))

(define-test EQUAL?-shared-list
  (let ((a '(1 2 3)))
    (let ((l1-a (list a a))
	  (l1-b (list a a))
	  (l2-a '((1 2 3) (1 2 3)))
	  (l2-b '((1 2 3) (1 2 3))))
      (check (EQUAL? l1-a l1-a))
      (check (EQUAL? l1-a l1-b))
      (check (EQUAL? l2-a l2-a))
      (check (EQUAL? l2-a l2-b))
      (check (not (EQUAL? l1-a l2-a)))
      (check (not (EQUAL? l2-a l1-a))))
    (let ((l1-a (list a '(1 2 3) a))
	  (l1-b (list a '(1 2 3) a))
	  (l2-a '((1 2 3) (1 2 3) (1 2 3)))
	  (l2-b '((1 2 3) (1 2 3) (1 2 3))))
      (check (EQUAL? l1-a l1-a))
      (check (EQUAL? l1-a l1-b))
      (check (EQUAL? l2-a l2-a))
      (check (EQUAL? l2-a l2-b))
      (check (not (EQUAL? l1-a l2-a)))
      (check (not (EQUAL? l2-a l1-a))))
    (let ((l3-a (cons a a))
	  (l3-b (cons a a))
	  (l4-a '((1 2 3) (1 2 3)))
	  (l4-b '((1 2 3) (1 2 3))))
      (check (EQUAL? l3-a l3-a))
      (check (EQUAL? l3-a l3-b))
      (check (EQUAL? l4-a l4-a))
      (check (EQUAL? l4-a l4-b))
      (check (not (EQUAL? l3-a l4-a)))
      (check (not (EQUAL? l4-a l3-a)))))
  (let ((l1-a (circular-list 1 2 3))
	(l1-b (circular-list 1 2 3))
	(l2-a (circular-list 1 2 3 1 2 3))
	(l2-b (circular-list 1 2 3 1 2 3)))
    (check (EQUAL? l1-a l1-a))
    (check (EQUAL? l1-a l1-b))
    (check (EQUAL? l2-a l2-a))
    (check (EQUAL? l2-a l2-b))
    (check (not (EQUAL? l1-a l2-a)))
    (check (not (EQUAL? l2-a l1-a)))))

;;; vectors

(define-test EQUAL?-simple-vector
  (check (EQUAL? [1] [1]))
  (check (EQUAL? [1 2] [1 2]))
  (check (EQUAL? [1 2 3] [1 2 3]))
  (check (EQUAL? [1 2 3 4 5 6 7 8 9 10] [1 2 3 4 5 6 7 8 9 10]))
  (check (not (EQUAL? [1] [2])))
  (check (not (EQUAL? [1 2] [3 2])))
  (check (not (EQUAL? [1 2] [1 3])))
  (check (not (EQUAL? [1 2 3] [2 2 3])))
  (check (not (EQUAL? [1 2 3] [1 3 3])))
  (check (not (EQUAL? [1 2 3] [1 2 4])))
  (check (not (EQUAL? [1 2 3 4 5 6 7 8 9 11] [1 2 3 4 5 6 7 8 9 10]))))

(define-test EQUAL?-shared-vector
  (let ((l1-a (make-vector 10 #f))
	(l2-a (make-vector 10 #f)))
    (vector-set! l1-a 9 l1-a)
    (vector-set! l2-a 9 l2-a)
    (check (EQUAL? l1-a l2-a))
    (check (not (EQUAL? l1-a [#f #f #f #f #f #f #f #f #f #f]))))
  (let ((l1-a (make-vector 10 #f))
	(l2-a (make-vector 10 #f)))
    (vector-set! l1-a 0 l1-a)
    (vector-set! l2-a 0 l2-a)
    (check (EQUAL? l1-a l2-a))
    (check (not (EQUAL? l1-a [#f #f #f #f #f #f #f #f #f #f]))))
  (let ((l1-a (make-vector 10 #f))
	(l2-a (make-vector 10 #f)))
    (let loop ((i 0))
      (unless (>= i 10)
	(vector-set! l1-a i l1-a)
	(vector-set! l2-a i l2-a)
	(loop (+ i 1)))) 
    (check (EQUAL? l1-a l2-a))
    (check (not (EQUAL? l1-a [#f #f #f #f #f #f #f #f #f #f]))))
  (let ((a [1 2 3]))
    (let ((v1-a (vector a a))
	  (v1-b (vector a a))
	  (v2-a [[1 2 3] [1 2 3]])
	  (v2-b [[1 2 3] [1 2 3]]))
      (check (EQUAL? v1-a v1-a))
      (check (EQUAL? v1-a v1-b))
      (check (EQUAL? v2-a v2-a))
      (check (EQUAL? v2-a v2-b))
      (check (not (EQUAL? v1-a v2-a)))
      (check (not (EQUAL? v2-a v1-a))))
    (let ((v1-a (list a '(1 2 3) a))
	  (v1-b (list a '(1 2 3) a))
	  (v2-a [[1 2 3] [1 2 3] [1 2 3]])
	  (v2-b [[1 2 3] [1 2 3] [1 2 3]]))
      (check (EQUAL? v1-a v1-a))
      (check (EQUAL? v1-a v1-b))
      (check (EQUAL? v2-a v2-a))
      (check (EQUAL? v2-a v2-b))
      (check (not (EQUAL? v1-a v2-a)))
      (check (not (EQUAL? v2-a v1-a))))))

;;; hashes

(define-test EQUAL?-simple-hash
  (check (EQUAL? (identity-hash) (identity-hash)))
  (check (EQUAL? {} {}))
  (check (EQUAL? (identity-hash :a 1) (identity-hash :a 1)))
  (check (EQUAL? '{a 1} '{a 1}))
  (check (EQUAL? (identity-hash :a 1 :b 12) (identity-hash :a 1 :b 12)))
  (check (EQUAL? '{a 1 b 12} '{a 1 b 12}))
  (check (EQUAL? '{2943 a 2321 b} '{2321 b 2943 a}))
  (check (EQUAL? '{(h e l l o - w o r l d) 123
                       (f r o b o z z l e) 23
                       [1 2 3 4 2 2 3] 23}
                     '{(f r o b o z z l e) 23
                       [1 2 3 4 2 2 3] 23
                       (h e l l o - w o r l d) 123})))

(define-test EQUAL?-shared-hash
  ; Keys with shared structure
  (let ((a '(100000 200000 300000))
        (b '(400000 500000 600000))
        (h1 (make-hash))
        (h2 (make-hash)))
    (hash-set! h1 (cons 0 a) :test-symbol-1)
    (hash-set! h2 '(0 100000 200000 300000) :test-symbol-1)
    (check (equal? h1 h2))
    (check (EQUAL? h1 h2))
    (hash-set! h1 (cons 1 a) :test-symbol-2)
    (hash-set! h2 '(1 100000 200000 300000) :test-symbol-2)
    (check (equal? h1 h2))
    (check (not (EQUAL? h1 h2))))
  ; Values with shared structure
  (let ((a '(100000 200000 300000))
        (b '(400000 500000 600000))
        (h1 (make-hash))
        (h2 (make-hash)))
    (hash-set! h1 :test-symbol-1 (cons 0 a))
    (hash-set! h2 :test-symbol-1 '(0 100000 200000 300000))
    (check (equal? h1 h2))
    (check (EQUAL? h1 h2))
    (hash-set! h1 :test-symbol-2 (cons 1 a))
    (hash-set! h2 :test-symbol-2 '(1 100000 200000 300000))
    (check (equal? h1 h2))
    (check (not (EQUAL? h1 h2))))
  ; Circular Structure
  (let ((h1 (make-identity-hash))
        (h2 (make-identity-hash)))
    (hash-set! h1 :foo (circular-list 1))
    (hash-set! h2 :foo (circular-list 1))
    (check (EQUAL? h1 h2))
    (hash-set! h2 :foo (circular-list 1 1 1))
    (check (not (EQUAL? h1 h2)))))

;;; structures

(define-structure EQUAL?-test-null-structure)

(define-structure EQUAL?-test-structure x y z)
(define-structure EQUAL?-test-structure/2 x y z)

(define-test EQUAL?-simple-structure
  (let ((snull (make-EQUAL?-test-null-structure)))
    (check (not (EQUAL? snull :not-snull)))
    (check (EQUAL? snull snull))
    (check (EQUAL? snull (make-EQUAL?-test-null-structure)))
    (check (not (EQUAL? snull (make-EQUAL?-test-structure)))))

  (let ((s1 (make-EQUAL?-test-structure))
        (s2 (make-EQUAL?-test-structure)))

    (check (EQUAL? s1 s1))
    (check (EQUAL? s1 s2))
    (check (EQUAL? s2 s1))

    (check (not (EQUAL? s1 :keyword)))
    (check (not (EQUAL? :keyword s1)))
    (check (not (EQUAL? s1 "string")))
    (check (not (EQUAL? "string" s1)))
    (check (not (EQUAL? s1 52)))
    (check (not (EQUAL? 42 s1)))
    (check (not (EQUAL? s1 (make-EQUAL?-test-structure/2))))
    (check (not (EQUAL? (make-EQUAL?-test-structure/2) s1)))

    (let ((value "new-value"))
      (set-EQUAL?-test-structure-x! s1 value)

      (check (not (EQUAL? s1 s2)))
      (check (not (EQUAL? s2 s1)))

      (set-EQUAL?-test-structure-x! s2 value)
      (check (EQUAL? s1 s2))
      (check (EQUAL? s2 s1)))

    (let ((value :another-new-value))
      (set-EQUAL?-test-structure-y! s1 value)

      (check (not (EQUAL? s1 s2)))
      (check (not (EQUAL? s2 s1)))

      (set-EQUAL?-test-structure-y! s2 value)
      (check (EQUAL? s1 s2))
      (check (EQUAL? s2 s1)))

    (let ((value 42))
      (set-EQUAL?-test-structure-z! s1 value)

      (check (not (EQUAL? s1 s2)))
      (check (not (EQUAL? s2 s1)))

      (set-EQUAL?-test-structure-z! s2 value)
      (check (EQUAL? s1 s2))
      (check (EQUAL? s2 s1)))))


(define-test EQUAL?-shared-structure
  (let ((snull (make-EQUAL?-test-null-structure)))
    (check (not (EQUAL? snull :not-snull)))
    (check (EQUAL? snull snull))
    (check (EQUAL? snull (make-EQUAL?-test-null-structure)))
    (check (not (EQUAL? snull (make-EQUAL?-test-structure)))))

  (let ((s1 (make-EQUAL?-test-structure))
        (s2 (make-EQUAL?-test-structure))
        (l1 '(1 2 3))
        (l2 '(1 2 3)))
    (set-EQUAL?-test-structure-x! s1 l1)
    (set-EQUAL?-test-structure-y! s1 l1)

    (set-EQUAL?-test-structure-x! s2 l1)
    (set-EQUAL?-test-structure-y! s2 l2)

    (check (equal? s1 s2))
    (check (not (EQUAL? s1 s2)))

    (set-EQUAL?-test-structure-y! s2 l1)

    (check (equal? s1 s2))
    (check (EQUAL? s1 s2)))

  (let ((s1 (make-EQUAL?-test-structure))
        (s2 (make-EQUAL?-test-structure)))

    (set-EQUAL?-test-structure-x! s1 (circular-list 1 2 3))
    (set-EQUAL?-test-structure-x! s2 (circular-list 1 2 3))

    (check (EQUAL? s1 s2))

    (set-EQUAL?-test-structure-y! s1 (circular-list 1 2 3))
    (set-EQUAL?-test-structure-y! s2 (circular-list 1 2 3))
    (set-EQUAL?-test-structure-z! s1 (circular-list 1 2 3))
    (set-EQUAL?-test-structure-z! s2 (circular-list 1 2 3))

    (check (EQUAL? s1 s2)))

  (let ((s1 (make-EQUAL?-test-structure))
        (s2 (make-EQUAL?-test-structure))
        (s3 (make-EQUAL?-test-structure)))

    (set-EQUAL?-test-structure-x! s1 s1)
    (set-EQUAL?-test-structure-x! s2 s2)

    (check (EQUAL? s1 s2))

    (set-EQUAL?-test-structure-y! s1 s1)
    (set-EQUAL?-test-structure-y! s2 s2)

    (check (EQUAL? s1 s2))

    (set-EQUAL?-test-structure-z! s1 s1)
    (set-EQUAL?-test-structure-z! s2 s2)

    (check (EQUAL? s1 s2))

    (set-EQUAL?-test-structure-x! s2 s3)
    (set-EQUAL?-test-structure-x! s3 s2)

    (check (not (EQUAL? s1 s2)))))

