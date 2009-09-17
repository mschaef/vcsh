(use-package! "unit-test")

(define-test every?
  (test-case (every? even? '()))
  (test-case (eq? #t (every? even? '())))
   
  (test-case (runtime-error? (every? even? 'foo)))
  (test-case (runtime-error? (every? even? 12)))

  (test-case (every? even? '(2 4 6 8 10)))
  (test-case (every? even? '(2)))
  (test-case (not (every? even? '(2 4 6 8 9))))
  (test-case (not (every? even? '(1 4 6 8 10))))
  (test-case (not (every? even? '(1 3 5 7 9))))
  (test-case (not (every? even? '(1))))

  (test-case (eq? (every? symbol? '(a b c d)) 'd))
  (test-case (eq? (every? symbol? '(a)) 'a)))
