(use-package! "unit-test")

(define-test test-memv-index
  (check (runtime-error? (memv-index 'x :not-a-list)))
  (check (runtime-error? (memv-index 'x '[not a list either])))
  (check (runtime-error? (memv-index 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (= 0 (memv-index 1 xs)))
    (check (= 1 (memv-index :symbol xs)))
    (check (not (memv-index '(composite key) xs)))
    (check (= 2 (memv-index (caddr xs) xs)))        
    (check (= 3 (memv-index () xs)))
    (check (not (memv-index :not-a-memv-index xs)))
    (check (not (memv-index "string" xs))))

  (let ((xs '(1000000)))
    (check (memv-index 1000000 xs)))

  (let ((xs '(1 1000000)))
    (check (memv-index 1000000 xs))))
