(use-package! "unit-test")

(define-test take-while!
  (check (equal? (take-while! even? ()) ()))
  (check (runtime-error? (take-while! even? :foo)))
  (check (runtime-error? (take-while! :not-a-procecdure '(1 2 3))))

  (let* ((xs `(2 4 8 1 2))
         (res (take-while! even? xs)))
    (check (equal? res '(2 4 8)))
    (check (eq? res xs)))

  (let* ((xs `(2 4 8 1 2))
         (res (take-while! even? xs)))
    (check (equal? res '(2 4 8)))
    (check (eq? res xs)))

  (let* ((xs `(2 4 8 1 . 2))
         (res (take-while! even? xs)))
    (check (equal? res '(2 4 8)))
    (check (eq? res xs)))

  (check (equal? (take-while! (always #t) `(2 4 8 1 . 2))
                 `(2 4 8 1 . 2)))

  (check (equal? (take-while! even? `(1 3 5)) '()))
  (check (equal? (take-while! even? `(1 2 4)) '())))
