(use-package! "unit-test")

(define-test test-member
  (test-case (runtime-error? (member 'x :not-a-list)))
  (test-case (runtime-error? (member 'x [not a list either])))
  (test-case (runtime-error? (member 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (test-case (eq? xs (member 1 xs)))
    (test-case (eq? (cdr xs) (member :symbol xs)))
    (test-case (eq? (cddr xs) (member '(composite key) xs)))
    (test-case (eq? (cdddr xs) (member () xs)))
    (test-case (eq? (cddddr xs) (member "string" xs)))
    (test-case (not (member :not-a-member xs))))

  (let ((xs '(1000000)))
    (test-case (member 1000000 xs)))

  (let ((xs '(1 1000000)))
    (test-case (member 1000000 xs))))
