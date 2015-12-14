(use-package! "unit-test")

(define-test every?
  (check (every? even? '()))
  (check (eq? #t (every? even? '())))
   
  (check (runtime-error? (every? even? 'foo)))
  (check (runtime-error? (every? even? 12)))

  (check (every? even? '(2 4 6 8 10)))
  (check (every? even? '(2)))
  (check (not (every? even? '(2 4 6 8 9))))
  (check (not (every? even? '(1 4 6 8 10))))
  (check (not (every? even? '(1 3 5 7 9))))
  (check (not (every? even? '(1))))

  (check (eq? (every? symbol? '(a b c d)) 'd))
  (check (eq? (every? symbol? '(a)) 'a)))
