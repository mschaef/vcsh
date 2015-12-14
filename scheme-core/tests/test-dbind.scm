(define-package "test-dbind"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test dbind
  (dbind x '(1 2 3)
    (check (equal? x '(1 2 3))))

  (dbind (x) '((1 2 3))
    (check (equal? x '(1 2 3))))

  (dbind (x . y) '(1 2 3)
    (check (equal? x 1))
    (check (equal? y '(2 3))))

  (dbind (x y z) '(1 2 3)
    (check (equal? x 1))
    (check (equal? y 2))
    (check (equal? z 3)))

  (dbind (x y (z)) '(1 2 (3))
    (check (equal? x 1))
    (check (equal? y 2))
    (check (equal? z 3)))

  (dbind (x y . z) '(1 2 3)
    (check (equal? x 1))
    (check (equal? y 2))
    (check (equal? z '(3))))

  (dbind [x y z] [1 2 3]
    (check (equal? x 1))
    (check (equal? y 2))
    (check (equal? z 3)))

  (dbind [[a b c] [x y z]] [[1 2 3] [4 5 6]]
    (check (equal? a 1))
    (check (equal? b 2))
    (check (equal? c 3))
    (check (equal? x 4))
    (check (equal? y 5))
    (check (equal? z 6))))


