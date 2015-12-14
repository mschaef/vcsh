(use-package! "unit-test")

(define-test fast-io-circular-structure
  (check (can-fast-io-round-trip? (let ((x (cons 's1 's1))) (set-cdr! x x) x)))
  (check (can-fast-io-round-trip? (let ((x (cons 's1 's1))) (set-car! x x) x)))
  (check (can-fast-io-round-trip? (cons 's2 (let ((x (cons 's1 's1))) (set-cdr! x x) x))))
  (check (can-fast-io-round-trip? (cons 's2 (let ((x (cons 's1 's1))) (set-car! x x) x))))

  (check (can-fast-io-round-trip? (cons :foo (let ((x (cons 's1 's1))) (set-cdr! x x) x))))
  (check (can-fast-io-round-trip? (cons :foo (let ((x (cons 's1 's1))) (set-car! x x) x))))
  (check (can-fast-io-round-trip? (cons :foo (cons 's2 (let ((x (cons 's1 's1))) (set-cdr! x x) x)))))
  (check (can-fast-io-round-trip? (cons :foo (cons 's2 (let ((x (cons 's1 's1))) (set-car! x x) x)))))

  (check (can-fast-io-round-trip? (circular-list 1 2 3 4 5 6 7 8 9 10)))

  (check (can-fast-io-round-trip? (let ((a (circular-list #f #f))
					(b (circular-list 1 2 3)))
				    (set-car! a b)
				    (set-car! (cdr a) (cdr b))
				    a))))
