(use-package! "unit-test")

(define-structure match-test-structure x y)
(define-structure match-test-structure-3 x y z)

(define-test match?
  (test-case (equal? () (match? () ())))
  (test-case (equal? () (match? 'symbol 'symbol)))
  (test-case (equal? () (match? "string" "string")))
  (test-case (equal? () (match? 314 314)))
  (test-case (equal? () (match? 3.14 3.14)))

  (test-case (not (match? () 'symbol)))
  (test-case (not (match? () "string")))
  (test-case (not (match? () 314)))
  (test-case (not (match? () 3.14)))
  (test-case (not (match? 'symbol ())))
  (test-case (not (match? "string" ())))
  (test-case (not (match? 314 ())))
  (test-case (not (match? 3.14 ())))

  (test-case (not (match? '(1) [1])))
  (test-case (not (match? [1] '(1))))

  (test-case (not (match? '(1 2 3) '(1))))
  (test-case (not (match? '(1) '(1 1 2))))

  (test-case (not (match? [1 2 3] [1])))
  (test-case (not (match? [1] [1 1 2])))

  (test-case (equal? () (match? '(1) '(1))))
  (test-case (equal? () (match? [1] [1])))

  (test-case (equal? () (match? '(1 2 3) '(1 2 3))))
  (test-case (equal? () (match? [1 2 3] [1 2 3])))

  (test-case (equal? '((?a . a)) (match? '?a 'a)))

  (test-case (equal? '((?a . 1)) (match? '?a 1 )))
  (test-case (equal? '((?a . 1)) (match? '(?a . ?a) '(1 . 1))))
  (test-case (equal? '((?a . 1)) (match? '(?a (?a . ?a)) '(1 (1 . 1)))))

  (test-case (not (match? '(?a . ?a) '(1 . 2))))
  (test-case (not (match? '(?a (?a . ?a)) '(2 (1 . 1)))))
  (test-case (not (match? '(?a (?a . ?a)) '(1 (2 . 1)))))
  (test-case (not (match? '(?a (?a . ?a)) '(1 (1 . 2)))))

  (test-case (not (match? '(?a ?a) '(1 2))))
  (test-case (not (match? '(?a ?a ?a) '(2 1 1))))
  (test-case (not (match? '(?a ?a ?a) '(1 2 1))))
  (test-case (not (match? '(?a ?a ?a) '(1 1 2))))

  (test-case (equal? '((?a b c d)) (match? '?a '(b c d))))

  (test-case (equal? '((?b c d) (?a . b)) (match? '(?a . ?b) '(b c d))))
  (test-case (equal? '((?c . 3) (?b . 2) (?a . 1)) (match? '(?a ?b ?c) '(1 2 3))))

  (test-case (equal? '((?a . 1)) (match? [?a] [1])))
  (test-case (equal? '((?c . 3) (?b . 2) (?a . 1)) (match? [?a ?b ?c] [1 2 3])))

  (test-case (not (match? [?a ?a] [1 2])))
  (test-case (not (match? [?a (?a ?a)] [2 (1 1)])))
  (test-case (not (match? [?a (?a ?a)] [1 (2 1)])))
  (test-case (not (match? [?a (?a ?a)] [1 (1 2)])))

  (test-case (not (match? [?a ?a ?a] [2 1 1])))
  (test-case (not (match? [?a ?a ?a] [1 2 1])))
  (test-case (not (match? [?a ?a ?a] [1 1 2])))

  (test-case (not (match? '(?a [?a ?a]) '(2 [1 1]))))
  (test-case (not (match? '(?a [?a ?a]) '(1 [2 1]))))
  (test-case (not (match? '(?a [?a ?a]) '(1 [1 2]))))

  (test-case (not (match? '(??) '())))
  (test-case (not (match? '(??) '(a b))))
  (test-case (not (match? '() '(??))))
  (test-case (not (match? '(a) '(??))))
  (test-case (not (match? '(a b) '(??))))

  (test-case (null? (match? '(??) '(a))))
  (test-case (null? (match? '(?? ??) '(a b))))
  (test-case (equal? '((?a . a)) (match? '(?a ??) '(a b))))
  (test-case (equal? '((?a . a)) (match? '(?a ?? ?a) '(a b a))))
  (test-case (not (match? '(?a ?a ??) '(a b a))))

  (test-case (null? (match? (make-match-test-structure)
                            (make-match-test-structure))))

  (test-case (null? (match? (make-match-test-structure :x 2)
                            (make-match-test-structure :x 2))))

  (test-case (null? (match? (make-match-test-structure :x 2 :y 3)
                            (make-match-test-structure :x 2 :y 3))))

  (test-case (equal? '((?a . 2))
                     (match? (make-match-test-structure :x '?a :y 3)
                             (make-match-test-structure :x 2 :y 3))))

  (test-case (equal? '((?b . 3) (?a . 2))
                     (match? (make-match-test-structure :x '?a :y '?b)
                             (make-match-test-structure :x 2 :y 3))))

  (test-case (equal? '((?b . 3) (?a . 2))
                     (match? (make-match-test-structure :x '(?a) :y '(?b))
                             (make-match-test-structure :x '(2) :y '(3)))))

  (test-case (equal? '((?b . 3) (?a . 2))
                     (match? (make-match-test-structure :x '(?a . ?b) :y '(?b . ?a))
                             (make-match-test-structure :x '(2 . 3) :y '(3 . 2)))))

  (test-case (not (match? (make-match-test-structure :x '?a :y '?a)
                          (make-match-test-structure :x 2 :y 3))))

  (test-case (not (match? (make-match-test-structure)
                          (make-match-test-structure-3))))

  (test-case (not (match? (make-match-test-structure) 12)))

  (test-case (not (match? 12 (make-match-test-structure))))

  (test-case (null? (match? {} {})))

  (test-case (null? (match? {key val} {key val})))

  (test-case (equal? '((?a . val)) (match? {key ?a} {key val})))

  (test-case (not (match? {key val} {key ?a})))

  (test-case (not (match? {a ?a b ?a} {a b b c})))

  (test-case (match? {a ?a b ?b} {a b b c}))

  (test-case (not (match? '({a ?a b ?a}) '({a b b c}))))

  (test-case (match? '({a ?a b ?b}) '({a b b c})))

  (test-case (runtime-error? (match? {?a a} {?a a})))

  (test-case (not (match? (identity-hash) {})))
  (test-case (not (match? {} (identity-hash)))))
