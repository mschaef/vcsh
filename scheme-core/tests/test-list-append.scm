(use-package! "unit-test")

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
