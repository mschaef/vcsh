(use-package! "unit-test")

(define-test any?
  (test-case (not (any? even? '())))
  (test-case (runtime-error? (any? even? 'foo)))
  (test-case (runtime-error? (any? even? 12)))
  (test-case (any? even? '(1 2 3 4 5)))
  (test-case (not (any? even? '(1 3 5 7 9))))

  (test-case (not (any? symbol? '(1 2 3 4 5))))
  (test-case (any? symbol? '(1 2 3 a)))
  (test-case (any? symbol? '(a 1 2 3)))
  (test-case (any? symbol? '(a b c d)))

  (test-case (eq? 'a (any? symbol? '(a b c d))))
  (test-case (eq? 'a (any? symbol? '(a 1 2 3))))
  (test-case (eq? 'a (any? symbol? '(1 2 3 a))))

  (test-case (eq? 'a (any? symbol? '(a)))))
