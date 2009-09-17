(use-package! "unit-test")

(define-test delq
  (test-case (equal? () (delq :anything ())))

  (test-case (equal? '(x y z z y) (delq :foo '(:foo x y z z y))))
  (test-case (equal? '(x y z z y) (delq :foo '(:foo :foo x y z z y))))
  (test-case (equal? '(x y z z y) (delq :foo '(x :foo y z z y))))
  (test-case (equal? '(x y z z y) (delq :foo '(x :foo :foo y z z y))))
  (test-case (equal? '(x y z z y) (delq :foo '(x y z z y :foo))))
  (test-case (equal? '(x y z z y) (delq :foo '(x y z z y :foo :foo))))

  (test-case (equal? '(x y z z y) (delq :foo '(:foo x :foo y :foo z
                                           :foo z :foo y :foo))))

  (test-case (equal? () (delq :foo '(:foo))))
  (test-case (equal? () (delq :foo '(:foo :foo)))))