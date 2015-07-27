(use-package! "unit-test")

(define-test dbind
  (dbind x '(1 2 3)
    (test-case (equal? x '(1 2 3))))

  (dbind (x) '((1 2 3))
    (test-case (equal? x '(1 2 3))))

  (dbind (x . y) '(1 2 3)
    (test-case (equal? x 1))
    (test-case (equal? y '(2 3))))

  (dbind (x y z) '(1 2 3)
    (test-case (equal? x 1))
    (test-case (equal? y 2))
    (test-case (equal? z 3)))

  (dbind (x y (z)) '(1 2 (3))
    (test-case (equal? x 1))
    (test-case (equal? y 2))
    (test-case (equal? z 3)))

  (dbind (x y . z) '(1 2 3)
    (test-case (equal? x 1))
    (test-case (equal? y 2))
    (test-case (equal? z '(3))))

  (dbind [x y z] [1 2 3]
    (test-case (equal? x 1))
    (test-case (equal? y 2))
    (test-case (equal? z 3)))

  (dbind [[a b c] [x y z]] [[1 2 3] [4 5 6]]
    (test-case (equal? a 1))
    (test-case (equal? b 2))
    (test-case (equal? c 3))
    (test-case (equal? x 4))
    (test-case (equal? y 5))
    (test-case (equal? z 6))))


