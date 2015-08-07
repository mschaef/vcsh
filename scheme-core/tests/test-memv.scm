(use-package! "unit-test")

(define-test test-memv
  (test-case (runtime-error? (memv 'x :not-a-list)))
  (test-case (runtime-error? (memv 'x [not a list either])))
  (test-case (runtime-error? (memv 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (test-case (eq? xs (memv 1 xs)))
    (test-case (eq? (cdr xs) (memv :symbol xs)))
    (test-case (not (memv '(composite key) xs)))
    (test-case (eq? (cddr xs) (memv (caddr xs) xs)))        
    (test-case (eq? (cdddr xs) (memv () xs)))
    (test-case (not (memv :not-a-memv xs)))
    (test-case (not (memv "string" xs))))

  (let ((xs '(1000000)))
    (test-case (memv 1000000 xs)))

  (let ((xs '(1 1000000)))
    (test-case (memv 1000000 xs))))
