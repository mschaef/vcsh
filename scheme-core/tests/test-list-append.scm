(use-package! "unit-test")

(define-test list-append
  (let ((l1 '(1 2 3))
        (l2 '(4 5 6))
        (l3 '(7))
        (l4 ())
        (l5 '(8 9 10)))

    (test-case (runtime-error? (append :not-a-list l1)))
    (test-case (runtime-error? (append l1 :not-a-list l1)))
    (test-case (runtime-error? (append l1 :not-a-list :not-a-list)))

    (test-case (equal? (append '(1 2 . 3) l2) '(1 2 4 5 6)))
    (test-case (equal? '(1 2 3 1 2 . 3) (append l1 '(1 2 . 3))))
    
    (test-case (eq? () (append l4)))
    (test-case (eq? () (append l4 l4)))
    (test-case (eq? () (append l4 l4 l4 l4 l4 l4 l4)))
    
    (test-case (equal? '(1 2 3 4 5 6) (append l1 l2)))

    (test-case (equal? '(1 2 3 4 5 6 7 8 9 10) (append l1 l2 l3 l4 l5)))

    (test-case (equal? '(1 2 3) (append l1 l4)))
    (test-case (equal? '(1 2 3) (append l4 l1)))

    (test-case (equal? l1 '(1 2 3)))
    (test-case (equal? l2 '(4 5 6)))
    (test-case (equal? l3 '(7)))
    (test-case (equal? l4 ()))
    (test-case (equal? l5 '(8 9 10)))))
