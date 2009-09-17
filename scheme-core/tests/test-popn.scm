(use-package! "unit-test")

(define-test pop!
  (let* ((x '(:foo :bar :baz))
	 (y x))
    (test-case (eq? (pop! x) :foo))
    (test-case (equal? x (cdr y)))
    (test-case (eq? (pop! x) :bar))
    (test-case (equal? x (cddr y)))
    (test-case (eq? (pop! x) :baz))
    (test-case (equal? x (cdddr y)))
    (test-case (equal? y '(:foo :bar :baz)))))

