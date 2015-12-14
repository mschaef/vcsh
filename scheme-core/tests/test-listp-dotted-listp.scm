(use-package! "unit-test")

(define-test list?-dotted-list?
  (check (list? '()))
  (check (list? '(1 2 3)))
  (check (not (list? '(1 2 . 4))))

  (check (not (dotted-list? '())))
  (check (not (dotted-list? '(1 2 3))))
  (check (dotted-list? '(1 2 . 4))))

