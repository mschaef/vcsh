(use-package! "unit-test")

(define-test drop-while
  (check (equal? (drop-while even? ()) ()))
  (check (runtime-error? (drop-while even? :foo)))
  (check (runtime-error? (drop-while :not-a-procecdure '(1 2 3))))

  (let ((xs '(1 2 3 4 5)))
    (check (equal? (drop-while even? xs) '(1 2 3 4 5)))
    (check (eq? (drop-while even? xs) xs)))

  (let ((xs '(2 4 8 1 2)))
    (check (equal? (drop-while even? xs) '(1 2)))
    (check (eq? (drop-while even? xs) (cdddr xs))))

  (check (equal? (drop-while even? '(2 4 8)) '()))

  (let ((xs '(2 4 8 1 . 2)))
    (check (equal? (drop-while even? xs) '(1 . 2)))
    (check (eq? (drop-while even? xs) (cdddr xs))))

  (check (runtime-error? (drop-while (always #t) '(2 4 8 1 . 2))))

  (check (equal? (drop-while even? '(1 3 5)) '(1 3 5))))

