(use-package! "unit-test")

(define-test make-list
  (check (runtime-error? (make-list :non-numeric 0)))
  (check (runtime-error? (make-list -1 0)))

  (check (equal? () (make-list 0 :foo)))
  (check (equal? '(:foo) (make-list 1 :foo)))
  (check (equal? '(:foo :foo :foo) (make-list 3 :foo)))

  (check (= 1000 (length (make-list 1000 ())))))

  
