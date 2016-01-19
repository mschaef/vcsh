(define-package "test-fast-io"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test fast-io-atoms
  (check (can-fast-io-round-trip? '()))
  (check (can-fast-io-round-trip? #t))
  (check (can-fast-io-round-trip? #f))
  (check (can-fast-io-round-trip? :keyword))
  (check (can-fast-io-round-trip? 'symbol))
  (check (can-fast-io-round-trip? #\nul))
  (check (can-fast-io-round-trip? #\a))
  (check (can-fast-io-round-trip? ""))
  (check (can-fast-io-round-trip? "string"))
  (check (can-fast-io-round-trip? (system-info :most-negative-fixnum)))
  (check (can-fast-io-round-trip? -3000000000000))
  (check (can-fast-io-round-trip? -1000000000000))
  (check (can-fast-io-round-trip? -4294967296))
  (check (can-fast-io-round-trip? -4294967295))
  (check (can-fast-io-round-trip? -3000000000))
  (check (can-fast-io-round-trip? -2147483648))
  (check (can-fast-io-round-trip? -2147483647))
  (check (can-fast-io-round-trip? -1000000000))
  (check (can-fast-io-round-trip? (system-info :most-negative-fix32)))  
  (check (can-fast-io-round-trip? -65536))
  (check (can-fast-io-round-trip? -65535))
  (check (can-fast-io-round-trip? -32768))
  (check (can-fast-io-round-trip? -32767))
  (check (can-fast-io-round-trip? -10000))
  (check (can-fast-io-round-trip? -128))
  (check (can-fast-io-round-trip? -127))
  (check (can-fast-io-round-trip? -1))
  (check (can-fast-io-round-trip? 0))
  (check (can-fast-io-round-trip? 1))
  (check (can-fast-io-round-trip? 127))  
  (check (can-fast-io-round-trip? 128))
  (check (can-fast-io-round-trip? 10000))
  (check (can-fast-io-round-trip? 32767))
  (check (can-fast-io-round-trip? 32768))
  (check (can-fast-io-round-trip? 65535))
  (check (can-fast-io-round-trip? 65536))
  (check (can-fast-io-round-trip? (system-info :most-positive-fix32)))
  (check (can-fast-io-round-trip? 1000000000))
  (check (can-fast-io-round-trip? 2147483647))
  (check (can-fast-io-round-trip? 2147483648))
  (check (can-fast-io-round-trip? 3000000000))
  (check (can-fast-io-round-trip? 4294967295))
  (check (can-fast-io-round-trip? 4294967296))
  (check (can-fast-io-round-trip? 1000000000000))
  (check (can-fast-io-round-trip? 3000000000000))
  (check (can-fast-io-round-trip? (system-info :most-positive-fixnum)))
  (check (can-fast-io-round-trip? -1.0))
  (check (can-fast-io-round-trip? 0.0))
  (check (can-fast-io-round-trip? 1.0))
  (check (can-fast-io-round-trip? #inan))
  (check (can-fast-io-round-trip? #iposinf))
  (check (can-fast-io-round-trip? #ineginf))
  (check (can-fast-io-round-trip? (system-info :most-negative-flonum)))
  (check (can-fast-io-round-trip? (system-info :most-positive-flonum)))
  (check (can-fast-io-round-trip? (system-info :flonum-epsilon)))
  (check (can-fast-io-round-trip? (- (system-info :flonum-epsilon))))
  (check (can-fast-io-round-trip? -1-2i))
  (check (can-fast-io-round-trip? 1+2i))
  (check (can-fast-io-round-trip? 0+0i)))

(define-test fast-io-circular-structure
  (check (can-fast-io-round-trip? (let ((x (cons 's1 's1))) (set-cdr! x x) x)))
  (check (can-fast-io-round-trip? (let ((x (cons 's1 's1))) (set-car! x x) x)))
  (check (can-fast-io-round-trip? (cons 's2 (let ((x (cons 's1 's1))) (set-cdr! x x) x))))
  (check (can-fast-io-round-trip? (cons 's2 (let ((x (cons 's1 's1))) (set-car! x x) x))))

  (check (can-fast-io-round-trip? (cons :foo (let ((x (cons 's1 's1))) (set-cdr! x x) x))))
  (check (can-fast-io-round-trip? (cons :foo (let ((x (cons 's1 's1))) (set-car! x x) x))))
  (check (can-fast-io-round-trip? (cons :foo (cons 's2 (let ((x (cons 's1 's1))) (set-cdr! x x) x)))))
  (check (can-fast-io-round-trip? (cons :foo (cons 's2 (let ((x (cons 's1 's1))) (set-car! x x) x)))))

  (check (can-fast-io-round-trip? (circular-list 1 2 3 4 5 6 7 8 9 10)))

  (check (can-fast-io-round-trip? (let ((a (circular-list #f #f))
                                        (b (circular-list 1 2 3)))
                                    (set-car! a b)
                                    (set-car! (cdr a) (cdr b))
                                    a))))

(define-test fast-io-hashes
  (check (can-fast-io-round-trip? (identity-hash)))
  (check (can-fast-io-round-trip? (identity-hash :a 1)))
  (check (can-fast-io-round-trip? (identity-hash :a 1 :b 2)))
  (check (can-fast-io-round-trip? (identity-hash :a 1 :b 2 :c 3 :d 4)))

  (check (can-fast-io-round-trip? {}))
  (check (can-fast-io-round-trip? '{a 1}))
  (check (can-fast-io-round-trip? '{a 1 b 2}))
  (check (can-fast-io-round-trip? '{a 1 b 2 c 3 d 4})))





(define-test fast-io-lists-and-vectors
  (check (can-fast-io-round-trip? '()))
  (check (can-fast-io-round-trip? '(1)))
  (check (can-fast-io-round-trip? '(1 2)))
  (check (can-fast-io-round-trip? '(1 . 2)))
  (check (can-fast-io-round-trip? '(1 2 3)))
  (check (can-fast-io-round-trip? '(1 2 3 4 5 6 7 8 9 10)))
  (check (can-fast-io-round-trip? (make-list 260 #f)))

  (check (can-fast-io-round-trip? []))
  (check (can-fast-io-round-trip? [1]))
  (check (can-fast-io-round-trip? [1 2]))
  (check (can-fast-io-round-trip? [1 2 3]))
  (check (can-fast-io-round-trip? [1 2 3 4 5 6 7 8 9 10]))
  (check (can-fast-io-round-trip? (make-vector 260 #f)))

  (check (can-fast-io-round-trip? '(())))
  (check (can-fast-io-round-trip? '((1))))
  (check (can-fast-io-round-trip? '((1 2))))
  (check (can-fast-io-round-trip? '((1 . 2))))
  (check (can-fast-io-round-trip? '((1 2 3))))
  (check (can-fast-io-round-trip? '((1 2 3 4 5 6 7 8 9 10))))
  (check (can-fast-io-round-trip? (cons (make-list 260 #f))))

  (check (can-fast-io-round-trip? [[]]))
  (check (can-fast-io-round-trip? [[1]]))
  (check (can-fast-io-round-trip? [[1 2]]))
  (check (can-fast-io-round-trip? [[1 2 3]]))
  (check (can-fast-io-round-trip? [[1 2 3 4 5 6 7 8 9 10]]))
  (check (can-fast-io-round-trip? (vector (make-vector 260 #f))))

  (check (can-fast-io-round-trip? '([])))
  (check (can-fast-io-round-trip? '([1])))
  (check (can-fast-io-round-trip? '([1 2])))
  (check (can-fast-io-round-trip? '([1 2 3])))
  (check (can-fast-io-round-trip? '([1 2 3 4 5 6 7 8 9 10])))
  (check (can-fast-io-round-trip? (list (make-vector 260 #f))))

  (check (can-fast-io-round-trip? (vector '())))
  (check (can-fast-io-round-trip? (vector '(1))))
  (check (can-fast-io-round-trip? (vector '(1 2))))
  (check (can-fast-io-round-trip? (vector '(1 . 2))))
  (check (can-fast-io-round-trip? (vector '(1 2 3))))
  (check (can-fast-io-round-trip? (vector '(1 2 3 4 5 6 7 8 9 10))))

  (check (can-fast-io-round-trip? [[1] [2 3] [4 5 6] [7 8 9 10] [11 12 13 14 15]]))
  (check (can-fast-io-round-trip? '((1) (2 3) (4 5 6) (7 8 9 10) (11 12 13 14 15)))))



(define (nested-circular-lists count depth)
  (let recur ((count count) (depth depth))
    (let ((sublists ()))
      (repeat count
        (push! (if (= depth 0)
                   ()
                   (recur count (- depth 1)))
               sublists))
      (set-cdr! (last-pair sublists) sublists)
      sublists))) 

(define-test fast-io-shared-structure
  (check (can-fast-io-round-trip? '(s1 s1)))
  (check (can-fast-io-round-trip? '(s1 . s1)))
  (check (can-fast-io-round-trip? '(s1 s1 s1)))
  (check (can-fast-io-round-trip? '(s1 s1 . s1)))
  (check (can-fast-io-round-trip? '((s1 . s1) . s1)))
  (check (can-fast-io-round-trip? '(((s1 . s1) . s1) . s1)))
  (check (can-fast-io-round-trip? '(s1 s1 s2 s2)))
  (check (can-fast-io-round-trip? '(s1 s2 s1 s1)))
  (check (can-fast-io-round-trip? '(s2 s1 s1 s2)))
  (check (can-fast-io-round-trip? (make-list 300 's1)))
  (check (can-fast-io-round-trip? (let ((a '(s1 s2 s3))) (cons a a))))
  (check (can-fast-io-round-trip? (let ((a '(s1 s2 s3))) (list a a))))
  (check (can-fast-io-round-trip? (let ((a '(s1 s2 s3))) (cons a (cons a a)))))
  (check (can-fast-io-round-trip? (let ((a '(s1 s2 s3))) (cons a (cons a (cdr a))))))
  

  (let ((s1 '(1 . 1))
	(s2 '(2 . 2))
	(s3 '(3 . 3)))
    (check (can-fast-io-round-trip? `(,s1 ,s1)))
    (check (can-fast-io-round-trip? `(,s1 . ,s1)))
    (check (can-fast-io-round-trip? `(,s1 ,s1 ,s1)))
    (check (can-fast-io-round-trip? `(,s1 ,s1 . ,s1)))
    (check (can-fast-io-round-trip? `((,s1 . ,s1) . ,s1)))
    (check (can-fast-io-round-trip? `(((,s1 . ,s1) . ,s1) . ,s1)))
    (check (can-fast-io-round-trip? `(,s1 ,s1 ,s2 ,s2)))
    (check (can-fast-io-round-trip? `(,s1 ,s2 ,s1 ,s1)))
    (check (can-fast-io-round-trip? `(,s2 ,s1 ,s1 ,s2)))
    (check (can-fast-io-round-trip? (make-list 300 s1)))
    (check (can-fast-io-round-trip? (let ((a `(,s1 ,s2 ,s3))) (cons a a))))
    (check (can-fast-io-round-trip? (let ((a `(,s1 ,s2 ,s3))) (list a a))))
    (check (can-fast-io-round-trip? (let ((a `(,s1 ,s2 ,s3))) (cons a (cons a a)))))
    (check (can-fast-io-round-trip? (let ((a `(,s1 ,s2 ,s3))) (cons a (cons a (cdr a)))))))
  (let* ((xs (iseq 0 100))
         (ys (map (lambda (x) (list x x)) xs)))
    (check (can-fast-io-round-trip? (append xs xs)))
    (check (can-fast-io-round-trip? (append ys ys)))))

;; (check (can-fast-io-round-trip? (nested-circular-lists 3 9))) REVISIT: Fix this... see fasl.cpp for why this will throw an assertation error.



(define (make-all-byte-string)
  (let ((p (open-output-string)))
   (set-port-translate-mode! p #f)
   (let loop ((i 0))
     (cond ((> i 255) (get-output-string p))
           (#t 
            (display (integer->char i) p)
            (loop (+ i 1)))))))
      
(define (make-all-byte-combo-string)
  (let ((p (open-output-string)))
   (set-port-translate-mode! p #f)
   (let loop ((i 0) (j 0))
       (cond ((> i 255) (loop 0 (+ j 1)))
             ((> j 255) (get-output-string p))
             (#t 
               (display (integer->char i) p)
               (display (integer->char j) p)
               (loop (+ i 1) j))))))

(define-test fast-io-strings-and-characters
  (check (can-fast-io-round-trip? ""))
  (check (can-fast-io-round-trip? "a"))
  (check (can-fast-io-round-trip? "\n"))
  (check (can-fast-io-round-trip? "\n\r"))
  (check (can-fast-io-round-trip? "\r\n"))
  (check (can-fast-io-round-trip? "\r"))
  (check (can-fast-io-round-trip? (make-all-byte-string)))
  (check (can-fast-io-round-trip? (make-all-byte-combo-string)))

  (dotimes (n (char->integer (system-info :most-positive-character))) 
    (check (can-fast-io-round-trip? (integer->char n)))))

(define-test fast-io-subrs
  (check (can-fast-io-round-trip? car)))

(define-test fast-io-symbols
    (check (can-fast-io-round-trip? 'x))
    (let* ((a (gensym "test-symbol"))
	   (b (fast-io-round-trip a)))
      (check (symbol? b))
      (check (equal? (symbol-package a) (symbol-package b)))
      (check (equal? (symbol-name a) (symbol-name b)))

      (let* ((as (list a a))
             (bs (fast-io-round-trip as)))
        (check (eq? (first as) (second as))) ; This might as well be an 'assert' for the next case
        (check (eq? (first bs) (second bs)))))
    
    (check (can-fast-io-round-trip? :keyword-symbol)))
