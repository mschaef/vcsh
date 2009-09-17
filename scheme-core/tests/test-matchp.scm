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

  (test-case (not (match? '(1) #(1))))
  (test-case (not (match? #(1) '(1))))

  (test-case (not (match? '(1 2 3) '(1))))
  (test-case (not (match? '(1) '(1 1 2))))

  (test-case (not (match? #(1 2 3) #(1))))
  (test-case (not (match? #(1) #(1 1 2))))

  (test-case (equal? () (match? '(1) '(1))))
  (test-case (equal? () (match? #(1) #(1))))

  (test-case (equal? () (match? '(1 2 3) '(1 2 3))))
  (test-case (equal? () (match? #(1 2 3) #(1 2 3))))

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

  (test-case (equal? '((?a . 1)) (match? #(?a) #(1))))
  (test-case (equal? '((?c . 3) (?b . 2) (?a . 1)) (match? #(?a ?b ?c) #(1 2 3))))

  (test-case (not (match? #(?a ?a) #(1 2))))
  (test-case (not (match? #(?a (?a ?a)) #(2 (1 1)))))
  (test-case (not (match? #(?a (?a ?a)) #(1 (2 1)))))
  (test-case (not (match? #(?a (?a ?a)) #(1 (1 2)))))

  (test-case (not (match? #(?a ?a ?a) #(2 1 1))))
  (test-case (not (match? #(?a ?a ?a) #(1 2 1))))
  (test-case (not (match? #(?a ?a ?a) #(1 1 2))))

  (test-case (not (match? '(?a #(?a ?a)) '(2 #(1 1)))))
  (test-case (not (match? '(?a #(?a ?a)) '(1 #(2 1)))))
  (test-case (not (match? '(?a #(?a ?a)) '(1 #(1 2)))))

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


  (test-case (not (match? #h(:eq) 12)))
  (test-case (not (match? 12 #h(:eq))))

  (test-case (not (match? #h(:eq) #h(:equal))))
  (test-case (not (match? #h(:equal) #h(:eq))))

  (test-case (null? (match? #h(:eq) #h(:eq))))
  (test-case (null? (match? #h(:equal) #h(:equal))))

  (test-case (runtime-error? (match? #h(:eq ?a a) #h(:eq ?a a))))
  (test-case (runtime-error? (match? #h(:equal ?a a) #h(:equal ?a a))))

  (test-case (null? (match? #h(:eq key val) #h(:eq key val))))
  (test-case (null? (match? #h(:equal key val) #h(:equal key val))))

  (test-case (equal? '((?a . val)) (match? #h(:eq key ?a) #h(:eq key val))))
  (test-case (equal? '((?a . val)) (match? #h(:equal key ?a) #h(:equal key val))))

  (test-case (not (match? #h(:eq key val) #h(:eq key ?a))))
  (test-case (not (match? #h(:equal key val) #h(:equal key ?a))))

  (test-case (not (match? #h(:eq a ?a b ?a) #h(:eq a b b c))))
  (test-case (not (match? #h(:equal a ?a b ?a) #h(:equal a b b c))))

  (test-case (match? #h(:eq a ?a b ?b) #h(:eq a b b c)))
  (test-case (match? #h(:equal a ?a b ?b) #h(:equal a b b c)))

  (test-case (not (match? '(#h(:eq a ?a b ?a))
                          '(#h(:eq a b b c)))))
  (test-case (not (match? '(#h(:equal a ?a b ?a))
                          '(#h(:equal a b b c)))))

  (test-case (match? '(#h(:eq a ?a b ?b))
                     '(#h(:eq a b b c))))
  (test-case (match? '(#h(:equal a ?a b ?b))
                     '(#h(:equal a b b c))))

  (test-case (not (match? #h(:eq ax #h(:eq a ?a b ?a))
                          #h(:eq ax #h(:eq a b b c)))))
  (test-case (not (match? #h(:eq ax #h(:equal a ?a b ?a))
                          #h(:eq ax #h(:equal a b b c)))))

  (test-case (match? #h(:eq ax #h(:eq a ?a b ?b))
                     #h(:eq ax #h(:eq a b b c))))
  (test-case (match? #h(:eq ax #h(:equal a ?a b ?b))
                     #h(:eq ax #h(:equal a b b c))))
  )