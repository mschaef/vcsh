(use-package! "unit-test")

(define-test test-memq
  (check (runtime-error? (memq 'x :not-a-list)))
  (check (runtime-error? (memq 'x '[not a list either])))
  (check (runtime-error? (memq 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (eq? xs (memq 1 xs)))
    (check (eq? (cdr xs) (memq :symbol xs)))
    (check (not (memq '(composite key) xs)))
    (check (eq? (cddr xs) (memq (caddr xs) xs)))        
    (check (eq? (cdddr xs) (memq () xs)))
    (check (not (memq :not-a-memq xs))))

    (let ((xs '(1000000000)))
      (check (memq 1000000000 xs)))

    (let ((xs '(1 1000000)))
      (check (not (memq 1000000000 xs)))))
