(use-package! "unit-test")

(define-test reverse
  (check (runtime-error? (reverse :symbol)))

  (check (equal? () (reverse ())))

  (let ((xs (list 1)))
    (check (equal? '(1) (reverse xs)))
    (check (not (eq? xs (reverse xs)))))

  (let ((xs (list 1 2 3)))
    (check (equal? '(3 2 1) (reverse xs)))
    (check (not (eq? xs (reverse xs))))))

(define-test reverse!
  (check (runtime-error? (reverse! :symbol)))

  (check (equal? () (reverse! ())))

  (let ((xs (list 1)))
    (check (equal? '(1) (reverse! xs)))
    (check (eq? xs (reverse! xs))))

  (let ((xs (list 1 2 3)))
    (check (equal? '(3 2 1) (reverse! xs)))
    (check (eq? xs (reverse! xs)))))
