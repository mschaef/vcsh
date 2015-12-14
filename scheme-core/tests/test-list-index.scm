(use-package! "unit-test")

(define-test test-list-index
  (check (runtime-error? (list-index (always #t) :not-a-list)))
  (check (runtime-error? (list-index (always #t) '[not a list either])))
  (check (runtime-error? (list-index (always #f) '(x y z z . y))))
  (check (runtime-error? (list-index 1 '(x y z z y))))

  (check (not (list-index even? '(1 3 5 7 9))))
  (check (= 0 (list-index even? '(2 1 3 5 7 9))))
  (check (= 5 (list-index even? '(1 3 5 7 9 2)))))
