(use-package! "unit-test")

(define-test test-list-index
  (test-case (runtime-error? (list-index (always #t) :not-a-list)))
  (test-case (runtime-error? (list-index (always #t) #(not a list either))))
  (test-case (runtime-error? (list-index (always #f) '(x y z z . y))))
  (test-case (runtime-error? (list-index 1 '(x y z z y))))

  (test-case (not (list-index even? '(1 3 5 7 9))))
  (test-case (= 0 (list-index even? '(2 1 3 5 7 9))))
  (test-case (= 5 (list-index even? '(1 3 5 7 9 2)))))