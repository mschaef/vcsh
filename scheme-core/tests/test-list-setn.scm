(use-package! "unit-test")

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
