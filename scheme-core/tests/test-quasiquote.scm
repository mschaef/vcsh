(use-package! "unit-test")

(define-test quasiquote-list
  (let ((a 1) 
        (x 2)
        (b '(2 3 4))
        (c '(5 6 (7 8))))
    
    (check (equal? `(a)  '(a)))
    (check (equal? `(b)  '(b)))
    (check (equal? `(@b) '(@b)))
    (check (equal? `(c)  '(c)))
    (check (equal? `(@c) '(@c)))

    (check (equal? `(,a) '(1)))
    (check (equal? `(,b) '((2 3 4))))
    (check (equal? `(,@b) '(2 3 4)))
    (check (equal? `(,c) '((5 6 (7 8)))))
    (check (equal? `(,@c) '(5 6 (7 8))))

    (check (equal? `(a ,a)   '(a  1)))
    (check (equal? `(b ,b )  '(b  (2 3 4))))
    (check (equal? `(@b ,@b) '(@b 2 3 4)))
    (check (equal? `(c ,c)   '(c  (5 6 (7 8)))))
    (check (equal? `(@c ,@c) '(@c 5 6 (7 8))))

    (check (equal? `(,a a)   '(1 a)))
    (check (equal? `(,b b )  '((2 3 4) b)))
    (check (equal? `(,@b @b) '(2 3 4 @b)))
    (check (equal? `(,c c)   '((5 6 (7 8)) c)))
    (check (equal? `(,@c @c) '(5 6 (7 8) @c)))

    (check (equal? `((,a))  '((1))))
    (check (equal? `((,b))  '(((2 3 4)))))
    (check (equal? `((,@b)) '((2 3 4))))
    (check (equal? `((,c))  '(((5 6 (7 8))))))
    (check (equal? `((,@c)) '((5 6 (7 8)))))

    (check (equal? `(,a ,a)   '(1 1)))
    (check (equal? `(,b ,b)   '((2 3 4) (2 3 4))))
    (check (equal? `(,@b ,@b) '(2 3 4 2 3 4)))
    (check (equal? `(,c ,c)   '((5 6 (7 8)) (5 6 (7 8)))))
    (check (equal? `(,@c ,@c) '(5 6 (7 8) 5 6 (7 8))))

    (check (equal? `(,a . ,x) '(1 . 2)))
    (check (equal? `(,a . ,b) '(1 2 3 4)))))

(define-test quasiquote-vector
  (let ((x 42)
        (xs '(1 2 3)))
    (check (equal? [42] `[,x]))
    (check (equal? [1 2 3] `[,@xs]))
    (check (equal? '[(1 2 3)] `[,xs]))))

(define-test quasiquote-hash
  (let ((x 42)
        (xs '(1 2)))
    (check (equal? '{42 (1 2)} `{,x ,xs}))))
