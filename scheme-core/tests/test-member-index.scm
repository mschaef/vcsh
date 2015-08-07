(use-package! "unit-test")

(define-test test-member-index
  (test-case (runtime-error? (member-index 'x :not-a-list)))
  (test-case (runtime-error? (member-index 'x [not a list either])))
  (test-case (runtime-error? (member-index 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (test-case (= 0 (member-index 1 xs)))
    (test-case (= 1 (member-index :symbol xs)))
    (test-case (= 2 (member-index '(composite key) xs)))
    (test-case (= 3 (member-index () xs)))
    (test-case (= 4 (member-index "string" xs)))
    (test-case (not (member-index :not-a-member xs))))

  (let ((xs '(1000000)))
    (test-case (member-index 1000000 xs)))

  (let ((xs '(1 1000000)))
    (test-case (member-index 1000000 xs))))
