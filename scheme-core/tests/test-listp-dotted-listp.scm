(use-package! "unit-test")

(define-test list?-dotted-list?
  (test-case (list? '()))
  (test-case (list? '(1 2 3)))
  (test-case (not (list? '(1 2 . 4))))

  (test-case (not (dotted-list? '())))
  (test-case (not (dotted-list? '(1 2 3))))
  (test-case (dotted-list? '(1 2 . 4))))

