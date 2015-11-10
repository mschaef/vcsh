(use-package! "unit-test")

(define-test quasiquote-list
  (let ((a 1) 
        (x 2)
        (b '(2 3 4))
        (c '(5 6 (7 8))))
    
    (test-case (equal? `(a)  '(a)))
    (test-case (equal? `(b)  '(b)))
    (test-case (equal? `(@b) '(@b)))
    (test-case (equal? `(c)  '(c)))
    (test-case (equal? `(@c) '(@c)))

    (test-case (equal? `(,a) '(1)))
    (test-case (equal? `(,b) '((2 3 4))))
    (test-case (equal? `(,@b) '(2 3 4)))
    (test-case (equal? `(,c) '((5 6 (7 8)))))
    (test-case (equal? `(,@c) '(5 6 (7 8))))

    (test-case (equal? `(a ,a)   '(a  1)))
    (test-case (equal? `(b ,b )  '(b  (2 3 4))))
    (test-case (equal? `(@b ,@b) '(@b 2 3 4)))
    (test-case (equal? `(c ,c)   '(c  (5 6 (7 8)))))
    (test-case (equal? `(@c ,@c) '(@c 5 6 (7 8))))

    (test-case (equal? `(,a a)   '(1 a)))
    (test-case (equal? `(,b b )  '((2 3 4) b)))
    (test-case (equal? `(,@b @b) '(2 3 4 @b)))
    (test-case (equal? `(,c c)   '((5 6 (7 8)) c)))
    (test-case (equal? `(,@c @c) '(5 6 (7 8) @c)))

    (test-case (equal? `((,a))  '((1))))
    (test-case (equal? `((,b))  '(((2 3 4)))))
    (test-case (equal? `((,@b)) '((2 3 4))))
    (test-case (equal? `((,c))  '(((5 6 (7 8))))))
    (test-case (equal? `((,@c)) '((5 6 (7 8)))))

    (test-case (equal? `(,a ,a)   '(1 1)))
    (test-case (equal? `(,b ,b)   '((2 3 4) (2 3 4))))
    (test-case (equal? `(,@b ,@b) '(2 3 4 2 3 4)))
    (test-case (equal? `(,c ,c)   '((5 6 (7 8)) (5 6 (7 8)))))
    (test-case (equal? `(,@c ,@c) '(5 6 (7 8) 5 6 (7 8))))

    (test-case (equal? `(,a . ,x) '(1 . 2)))
    (test-case (equal? `(,a . ,b) '(1 2 3 4)))))

(define-test quasiquote-vector
  (let ((x 42)
        (xs '(1 2 3)))
    (test-case (equal? [42] `[,x]))
    (test-case (equal? [1 2 3] `[,@xs]))
    (test-case (equal? '[(1 2 3)] `[,xs]))))

(define-test quasiquote-hash
  (let ((x 42)
        (xs '(1 2)))
    (test-case (equal? '{42 (1 2)} `{,x ,xs}))))
