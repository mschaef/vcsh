(use-package! "unit-test")

(define-test list-set!
  (let ((xs (list-copy '(:a :b :c :d))))
    (test-case (equal? '(:a :b :c :d) (list-set! xs 0 :a)))
    (test-case (equal? '(:a :b :c :d) (list-set! xs 1 :b)))
    (test-case (equal? '(:a :b :c :d) (list-set! xs 2 :c)))
    (test-case (equal? '(:a :b :c :d) (list-set! xs 3 :d)))

    (test-case (equal? '(1 2 3 4) (list-set! (list-set! (list-set! (list-set! xs 
									  0 1) 
							       1 2)
						    2 3)
					 3 4)))

    (test-case (runtime-error? (list-set! '() 0 0)))
    (test-case (runtime-error? (list-set! '(1 2 3) -1 0)))
    (test-case (runtime-error? (list-set! '(1 2 3) 10 0)))
    (test-case (runtime-error? (list-set! '(1 2 . 3) 2 0)))

    ))
