(use-package! "unit-test")

(define-test drop-while
  (test-case (equal? (drop-while even? ()) ()))
  (test-case (runtime-error? (drop-while even? :foo)))
  (test-case (runtime-error? (drop-while :not-a-procecdure '(1 2 3))))

  (let ((xs '(1 2 3 4 5)))
    (test-case (equal? (drop-while even? xs) '(1 2 3 4 5)))
    (test-case (eq? (drop-while even? xs) xs)))

  (let ((xs '(2 4 8 1 2)))
    (test-case (equal? (drop-while even? xs) '(1 2)))
    (test-case (eq? (drop-while even? xs) (cdddr xs))))

  (test-case (equal? (drop-while even? '(2 4 8)) '()))

  (let ((xs '(2 4 8 1 . 2)))
    (test-case (equal? (drop-while even? xs) '(1 . 2)))
    (test-case (eq? (drop-while even? xs) (cdddr xs))))

  (test-case (runtime-error? (drop-while (always #t) '(2 4 8 1 . 2))))

  (test-case (equal? (drop-while even? '(1 3 5)) '(1 3 5))))

