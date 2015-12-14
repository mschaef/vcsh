(use-package! "unit-test")

(define-test pop!
  (let* ((x '(:foo :bar :baz))
	 (y x))
    (check (eq? (pop! x) :foo))
    (check (equal? x (cdr y)))
    (check (eq? (pop! x) :bar))
    (check (equal? x (cddr y)))
    (check (eq? (pop! x) :baz))
    (check (equal? x (cdddr y)))
    (check (equal? y '(:foo :bar :baz)))))

