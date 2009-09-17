(use-package! "unit-test")

(define-test take-while!
  (test-case (equal? (take-while! even? ()) ()))
  (test-case (runtime-error? (take-while! even? :foo)))
  (test-case (runtime-error? (take-while! :not-a-procecdure '(1 2 3))))

  (let* ((xs `(2 4 8 1 2))
         (res (take-while! even? xs)))
    (test-case (equal? res '(2 4 8)))
    (test-case (eq? res xs)))

  (let* ((xs `(2 4 8 1 2))
         (res (take-while! even? xs)))
    (test-case (equal? res '(2 4 8)))
    (test-case (eq? res xs)))

  (let* ((xs `(2 4 8 1 . 2))
         (res (take-while! even? xs)))
    (test-case (equal? res '(2 4 8)))
    (test-case (eq? res xs)))

  (test-case (equal? (take-while! (always #t) `(2 4 8 1 . 2))
                 `(2 4 8 1 . 2)))

  (test-case (equal? (take-while! even? `(1 3 5)) '()))
  (test-case (equal? (take-while! even? `(1 2 4)) '())))
