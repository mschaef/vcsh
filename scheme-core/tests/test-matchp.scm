(define-package "test-matchp"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test match?
  (check (equal? {} (match? () ())))
  (check (equal? {} (match? 'symbol 'symbol)))
  (check (equal? {} (match? "string" "string")))
  (check (equal? {} (match? 314 314)))
  (check (equal? {} (match? 3.14 3.14)))

  (check (not (match? () 'symbol)))
  (check (not (match? () "string")))
  (check (not (match? () 314)))
  (check (not (match? () 3.14)))
  (check (not (match? 'symbol ())))
  (check (not (match? "string" ())))
  (check (not (match? 314 ())))
  (check (not (match? 3.14 ())))

  (check (not (match? '(1) [1])))
  (check (not (match? [1] '(1))))

  (check (not (match? '(1 2 3) '(1))))
  (check (not (match? '(1) '(1 1 2))))

  (check (not (match? [1 2 3] [1])))
  (check (not (match? [1] [1 1 2])))

  (check (equal? {} (match? '(1) '(1))))
  (check (equal? {} (match? [1] [1])))

  (check (equal? {} (match? '(1 2 3) '(1 2 3))))
  (check (equal? {} (match? [1 2 3] [1 2 3])))

  (check (equal? '{?a a} (match? '?a 'a)))

  (check (equal? '{?a 1} (match? '?a 1 )))
  (check (equal? '{?a 1} (match? '(?a . ?a) '(1 . 1))))
  (check (equal? '{?a 1} (match? '(?a (?a . ?a)) '(1 (1 . 1)))))

  (check (not (match? '(?a . ?a) '(1 . 2))))
  (check (not (match? '(?a (?a . ?a)) '(2 (1 . 1)))))
  (check (not (match? '(?a (?a . ?a)) '(1 (2 . 1)))))
  (check (not (match? '(?a (?a . ?a)) '(1 (1 . 2)))))

  (check (not (match? '(?a ?a) '(1 2))))
  (check (not (match? '(?a ?a ?a) '(2 1 1))))
  (check (not (match? '(?a ?a ?a) '(1 2 1))))
  (check (not (match? '(?a ?a ?a) '(1 1 2))))

  (check (equal? '{?a (b c d)} (match? '?a '(b c d))))

  (check (equal? '{?b (c d) ?a b} (match? '(?a . ?b) '(b c d))))
  (check (equal? '{?c 3 ?b 2 ?a 1} (match? '(?a ?b ?c) '(1 2 3))))

  (check (equal? '{?a 1} (match? '[?a] [1])))
  (check (equal? '{?c 3 ?b 2 ?a 1} (match? '[?a ?b ?c] [1 2 3])))

  (check (not (match? '[?a ?a] [1 2])))
  (check (not (match? '[?a (?a ?a)] '[2 (1 1)])))
  (check (not (match? '[?a (?a ?a)] '[1 (2 1)])))
  (check (not (match? '[?a (?a ?a)] '[1 (1 2)])))

  (check (not (match? '[?a ?a ?a] [2 1 1])))
  (check (not (match? '[?a ?a ?a] [1 2 1])))
  (check (not (match? '[?a ?a ?a] [1 1 2])))

  (check (not (match? '(?a [?a ?a]) '(2 [1 1]))))
  (check (not (match? '(?a [?a ?a]) '(1 [2 1]))))
  (check (not (match? '(?a [?a ?a]) '(1 [1 2]))))

  (check (not (match? '(??) '())))
  (check (not (match? '(??) '(a b))))
  (check (not (match? '() '(??))))
  (check (not (match? '(a) '(??))))
  (check (not (match? '(a b) '(??))))

  (check (equal? {} (match? '(??) '(a))))
  (check (equal? {} (match? '(?? ??) '(a b))))
  (check (equal? '{?a a} (match? '(?a ??) '(a b))))
  (check (equal? '{?a a} (match? '(?a ?? ?a) '(a b a))))
  (check (not (match? '(?a ?a ??) '(a b a))))

  (check (equal? {} (match? {} {})))

  (check (equal? {} (match? '{key val} '{key val})))

  (check (equal? '{?a val} (match? '{key ?a} '{key val})))

  (check (not (match? '{key val} '{key ?a})))

  (check (not (match? '{a ?a b ?a} '{a b b c})))

  (check (match? '{a ?a b ?b} '{a b b c}))

  (check (not (match? '({a ?a b ?a}) '({a b b c}))))

  (check (match? '({a ?a b ?b}) '({a b b c})))

  (check (runtime-error? (match? '{?a a} '{?a a})))

  (check (not (match? (identity-hash) {})))
  (check (not (match? {} (identity-hash)))))
