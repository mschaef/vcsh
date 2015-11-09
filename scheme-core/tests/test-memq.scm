(use-package! "unit-test")

(define-test test-memq
  (test-case (runtime-error? (memq 'x :not-a-list)))
  (test-case (runtime-error? (memq 'x '[not a list either])))
  (test-case (runtime-error? (memq 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (test-case (eq? xs (memq 1 xs)))
    (test-case (eq? (cdr xs) (memq :symbol xs)))
    (test-case (not (memq '(composite key) xs)))
    (test-case (eq? (cddr xs) (memq (caddr xs) xs)))        
    (test-case (eq? (cdddr xs) (memq () xs)))
    (test-case (not (memq :not-a-memq xs))))

    (let ((xs '(1000000000)))
      (test-case (memq 1000000000 xs)))

    (let ((xs '(1 1000000)))
      (test-case (not (memq 1000000000 xs)))))
