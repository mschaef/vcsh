(use-package! "unit-test")

(define-test test-member-index
  (check (runtime-error? (member-index 'x :not-a-list)))
  (check (runtime-error? (member-index 'x '[not a list either])))
  (check (runtime-error? (member-index 1 '(x y z z . y))))

  (let ((xs '(1 :symbol (composite key) () "string")))
    (check (= 0 (member-index 1 xs)))
    (check (= 1 (member-index :symbol xs)))
    (check (= 2 (member-index '(composite key) xs)))
    (check (= 3 (member-index () xs)))
    (check (= 4 (member-index "string" xs)))
    (check (not (member-index :not-a-member xs))))

  (let ((xs '(1000000)))
    (check (member-index 1000000 xs)))

  (let ((xs '(1 1000000)))
    (check (member-index 1000000 xs))))
