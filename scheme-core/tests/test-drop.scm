(use-package! "unit-test")

(define-test drop
  (let ((xs `(1 2 3 4 5)))
    (check (eq? xs (drop xs 0)))
    (check (eq? (cdr xs) (drop xs 1)))
    (check (eq? (cddr xs) (drop xs 2)))
    (check (eq? (cdddr xs) (drop xs 3)))
    (check (eq? (cddddr xs) (drop xs 4)))
    (check (eq? '() (drop xs 5))))
  (check (runtime-error? (drop '(1 . 2) 2)))
  (check (runtime-error? (drop :not-a-list 1)))
  (check (not (runtime-error? (drop :not-a-list 0)))))

