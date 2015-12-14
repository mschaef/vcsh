(use-package! "unit-test")

(define-structure match-test-structure x y)
(define-structure match-test-structure-3 x y z)

(define-test match?
  (check (equal? () (match? () ())))
  (check (equal? () (match? 'symbol 'symbol)))
  (check (equal? () (match? "string" "string")))
  (check (equal? () (match? 314 314)))
  (check (equal? () (match? 3.14 3.14)))

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

  (check (equal? () (match? '(1) '(1))))
  (check (equal? () (match? [1] [1])))

  (check (equal? () (match? '(1 2 3) '(1 2 3))))
  (check (equal? () (match? [1 2 3] [1 2 3])))

  (check (equal? '((?a . a)) (match? '?a 'a)))

  (check (equal? '((?a . 1)) (match? '?a 1 )))
  (check (equal? '((?a . 1)) (match? '(?a . ?a) '(1 . 1))))
  (check (equal? '((?a . 1)) (match? '(?a (?a . ?a)) '(1 (1 . 1)))))

  (check (not (match? '(?a . ?a) '(1 . 2))))
  (check (not (match? '(?a (?a . ?a)) '(2 (1 . 1)))))
  (check (not (match? '(?a (?a . ?a)) '(1 (2 . 1)))))
  (check (not (match? '(?a (?a . ?a)) '(1 (1 . 2)))))

  (check (not (match? '(?a ?a) '(1 2))))
  (check (not (match? '(?a ?a ?a) '(2 1 1))))
  (check (not (match? '(?a ?a ?a) '(1 2 1))))
  (check (not (match? '(?a ?a ?a) '(1 1 2))))

  (check (equal? '((?a b c d)) (match? '?a '(b c d))))

  (check (equal? '((?b c d) (?a . b)) (match? '(?a . ?b) '(b c d))))
  (check (equal? '((?c . 3) (?b . 2) (?a . 1)) (match? '(?a ?b ?c) '(1 2 3))))

  (check (equal? '((?a . 1)) (match? '[?a] [1])))
  (check (equal? '((?c . 3) (?b . 2) (?a . 1)) (match? '[?a ?b ?c] [1 2 3])))

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

  (check (null? (match? '(??) '(a))))
  (check (null? (match? '(?? ??) '(a b))))
  (check (equal? '((?a . a)) (match? '(?a ??) '(a b))))
  (check (equal? '((?a . a)) (match? '(?a ?? ?a) '(a b a))))
  (check (not (match? '(?a ?a ??) '(a b a))))

  (check (null? (match? (make-match-test-structure)
                            (make-match-test-structure))))

  (check (null? (match? (make-match-test-structure :x 2)
                            (make-match-test-structure :x 2))))

  (check (null? (match? (make-match-test-structure :x 2 :y 3)
                            (make-match-test-structure :x 2 :y 3))))

  (check (equal? '((?a . 2))
                     (match? (make-match-test-structure :x '?a :y 3)
                             (make-match-test-structure :x 2 :y 3))))

  (check (equal? '((?b . 3) (?a . 2))
                     (match? (make-match-test-structure :x '?a :y '?b)
                             (make-match-test-structure :x 2 :y 3))))

  (check (equal? '((?b . 3) (?a . 2))
                     (match? (make-match-test-structure :x '(?a) :y '(?b))
                             (make-match-test-structure :x '(2) :y '(3)))))

  (check (equal? '((?b . 3) (?a . 2))
                     (match? (make-match-test-structure :x '(?a . ?b) :y '(?b . ?a))
                             (make-match-test-structure :x '(2 . 3) :y '(3 . 2)))))

  (check (not (match? (make-match-test-structure :x '?a :y '?a)
                          (make-match-test-structure :x 2 :y 3))))

  (check (not (match? (make-match-test-structure)
                          (make-match-test-structure-3))))

  (check (not (match? (make-match-test-structure) 12)))

  (check (not (match? 12 (make-match-test-structure))))

  (check (null? (match? {} {})))

  (check (null? (match? '{key val} '{key val})))

  (check (equal? '((?a . val)) (match? '{key ?a} '{key val})))

  (check (not (match? '{key val} '{key ?a})))

  (check (not (match? '{a ?a b ?a} '{a b b c})))

  (check (match? '{a ?a b ?b} '{a b b c}))

  (check (not (match? '({a ?a b ?a}) '({a b b c}))))

  (check (match? '({a ?a b ?b}) '({a b b c})))

  (check (runtime-error? (match? '{?a a} '{?a a})))

  (check (not (match? (identity-hash) {})))
  (check (not (match? {} (identity-hash)))))
