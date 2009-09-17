(use-package! "unit-test")

(define-test list-let
  (list-let x '(1 2 3)
    (test-case (equal? x '(1 2 3))))
  (list-let (x) '((1 2 3))
    (test-case (equal? x '(1 2 3))))
  (list-let (x . y) '(1 2 3)
    (test-case (equal? x 1))
    (test-case (equal? y '(2 3))))
  (list-let (x y z) '(1 2 3)
    (test-case (equal? x 1))
    (test-case (equal? y 2))
    (test-case (equal? z 3))))


