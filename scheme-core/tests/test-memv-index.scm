(use-package! "unit-test")

(define-test test-memv-index
  (test-case (runtime-error? (memv-index 'x :not-a-list)))
  (test-case (runtime-error? (memv-index 'x '[not a list either])))
  (test-case (runtime-error? (memv-index 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (test-case (= 0 (memv-index 1 xs)))
    (test-case (= 1 (memv-index :symbol xs)))
    (test-case (not (memv-index '(composite key) xs)))
    (test-case (= 2 (memv-index (caddr xs) xs)))        
    (test-case (= 3 (memv-index () xs)))
    (test-case (not (memv-index :not-a-memv-index xs)))
    (test-case (not (memv-index "string" xs))))

  (let ((xs '(1000000)))
    (test-case (memv-index 1000000 xs)))

  (let ((xs '(1 1000000)))
    (test-case (memv-index 1000000 xs))))
