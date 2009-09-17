(use-package! "unit-test")

(define-test drop
  (let ((xs `(1 2 3 4 5)))
    (test-case (eq? xs (drop xs 0)))
    (test-case (eq? (cdr xs) (drop xs 1)))
    (test-case (eq? (cddr xs) (drop xs 2)))
    (test-case (eq? (cdddr xs) (drop xs 3)))
    (test-case (eq? (cddddr xs) (drop xs 4)))
    (test-case (eq? '() (drop xs 5))))
  (test-case (runtime-error? (drop '(1 . 2) 2)))
  (test-case (runtime-error? (drop :not-a-list 1)))
  (test-case (not (runtime-error? (drop :not-a-list 0)))))

