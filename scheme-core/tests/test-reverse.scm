(use-package! "unit-test")

(define-test reverse
  (test-case (runtime-error? (reverse :symbol)))

  (test-case (equal? () (reverse ())))

  (let ((xs (list 1)))
    (test-case (equal? '(1) (reverse xs)))
    (test-case (not (eq? xs (reverse xs)))))

  (let ((xs (list 1 2 3)))
    (test-case (equal? '(3 2 1) (reverse xs)))
    (test-case (not (eq? xs (reverse xs))))))

(define-test reverse!
  (test-case (runtime-error? (reverse! :symbol)))

  (test-case (equal? () (reverse! ())))

  (let ((xs (list 1)))
    (test-case (equal? '(1) (reverse! xs)))
    (test-case (eq? xs (reverse! xs))))

  (let ((xs (list 1 2 3)))
    (test-case (equal? '(3 2 1) (reverse! xs)))
    (test-case (eq? xs (reverse! xs)))))
