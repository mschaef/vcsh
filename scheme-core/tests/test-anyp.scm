(use-package! "unit-test")

(define-test any?
  (check (not (any? even? '())))
  (check (runtime-error? (any? even? 'foo)))
  (check (runtime-error? (any? even? 12)))
  (check (any? even? '(1 2 3 4 5)))
  (check (not (any? even? '(1 3 5 7 9))))

  (check (not (any? symbol? '(1 2 3 4 5))))
  (check (any? symbol? '(1 2 3 a)))
  (check (any? symbol? '(a 1 2 3)))
  (check (any? symbol? '(a b c d)))

  (check (eq? 'a (any? symbol? '(a b c d))))
  (check (eq? 'a (any? symbol? '(a 1 2 3))))
  (check (eq? 'a (any? symbol? '(1 2 3 a))))

  (check (eq? 'a (any? symbol? '(a)))))
