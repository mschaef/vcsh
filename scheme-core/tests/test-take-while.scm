(use-package! "unit-test")

(define-test take-while
  (check (equal? (take-while even? ())))
  (check (runtime-error? (take-while even? :foo)))
  (check (runtime-error? (take-while :not-a-procecdure '(1 2 3))))

  (let ((xs '(2 4 8 1 2)))
    (check (equal? (take-while even? xs) '(2 4 8)))
    (check (not (eq? (take-while even? xs) xs))))

  (let ((xs '(2 4 8 1 2)))
    (check (equal? (take-while even? xs) '(2 4 8)))
    (check (not (eq? (take-while even? xs) xs))))

  (let ((xs '(2 4 8 1 . 2)))
    (check (equal? (take-while even? xs) '(2 4 8)))
    (check (not (eq? (take-while even? xs) xs))))

  (check (runtime-error? (take-while (always #t) '(2 4 8 1 . 2))))

  (check (equal? (take-while even? '(1 3 5)) '()))
  (check (equal? (take-while even? '(1 2 4)) '())))

