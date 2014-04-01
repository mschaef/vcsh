(use-package! "unit-test")

(define-test test-memq-index
  (test-case (runtime-error? (memq-index 'x :not-a-list)))
  (test-case (runtime-error? (memq-index 'x #(not a list either))))
  (test-case (runtime-error? (memq-index 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (test-case (= 0 (memq-index 1 xs)))
    (test-case (= 1 (memq-index :symbol xs)))
    (test-case (not (memq-index '(composite key) xs)))
    (test-case (= 2 (memq-index (caddr xs) xs)))        
    (test-case (= 3 (memq-index () xs)))
    (test-case (not (memq-index :not-a-memq-index xs))))

    (let ((xs '(1000000000)))
      (test-case (= 0 (memq-index 1000000000 xs))))

    (let ((xs '(1 1000000000)))
      (test-case (= 1 (memq-index 1000000000 xs)))))
