(use-package! "unit-test")

(define-test delq
  (check (equal? () (delq :anything ())))

  (check (equal? '(x y z z y) (delq :foo '(:foo x y z z y))))
  (check (equal? '(x y z z y) (delq :foo '(:foo :foo x y z z y))))
  (check (equal? '(x y z z y) (delq :foo '(x :foo y z z y))))
  (check (equal? '(x y z z y) (delq :foo '(x :foo :foo y z z y))))
  (check (equal? '(x y z z y) (delq :foo '(x y z z y :foo))))
  (check (equal? '(x y z z y) (delq :foo '(x y z z y :foo :foo))))

  (check (equal? '(x y z z y) (delq :foo '(:foo x :foo y :foo z
                                           :foo z :foo y :foo))))

  (check (equal? () (delq :foo '(:foo))))
  (check (equal? () (delq :foo '(:foo :foo)))))
