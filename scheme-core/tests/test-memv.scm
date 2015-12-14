(use-package! "unit-test")

(define-test test-memv
  (check (runtime-error? (memv 'x :not-a-list)))
  (check (runtime-error? (memv 'x '[not a list either])))
  (check (runtime-error? (memv 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (eq? xs (memv 1 xs)))
    (check (eq? (cdr xs) (memv :symbol xs)))
    (check (not (memv '(composite key) xs)))
    (check (eq? (cddr xs) (memv (caddr xs) xs)))        
    (check (eq? (cdddr xs) (memv () xs)))
    (check (not (memv :not-a-memv xs)))
    (check (not (memv "string" xs))))

  (let ((xs '(1000000)))
    (check (memv 1000000 xs)))

  (let ((xs '(1 1000000)))
    (check (memv 1000000 xs))))
