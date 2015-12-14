(use-package! "unit-test")

(define-test test-member
  (check (runtime-error? (member 'x :not-a-list)))
  (check (runtime-error? (member 'x '[not a list either])))
  (check (runtime-error? (member 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (eq? xs (member 1 xs)))
    (check (eq? (cdr xs) (member :symbol xs)))
    (check (eq? (cddr xs) (member '(composite key) xs)))
    (check (eq? (cdddr xs) (member () xs)))
    (check (eq? (cddddr xs) (member "string" xs)))
    (check (not (member :not-a-member xs))))

  (let ((xs '(1000000)))
    (check (member 1000000 xs)))

  (let ((xs '(1 1000000)))
    (check (member 1000000 xs))))
