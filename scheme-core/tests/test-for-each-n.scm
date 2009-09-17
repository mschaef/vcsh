(use-package! "unit-test")

(define-test for-each-n
  (test-case (runtime-error? (for-each (lambda (x y z) (list x y z))
				   )))

  (test-case (runtime-error? (for-each (lambda (x y z) (list x y z))
				   '(1 2 3) '(1 2 3) '(1 2 3))))

  (test-case (runtime-error? (for-each (lambda (x y z) (list x y z))
				   '(1 2 3) '(1 2 3) '(1 2 3) '(1 2 3) '(1 2 3))))

  )

