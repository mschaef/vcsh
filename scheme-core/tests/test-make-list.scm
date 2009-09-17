(use-package! "unit-test")

(define-test make-list
  (test-case (runtime-error? (make-list :non-numeric 0)))
  (test-case (runtime-error? (make-list -1 0)))

  (test-case (equal? () (make-list 0 :foo)))
  (test-case (equal? '(:foo) (make-list 1 :foo)))
  (test-case (equal? '(:foo :foo :foo) (make-list 3 :foo)))

  (test-case (= 1000 (length (make-list 1000 ())))))

  