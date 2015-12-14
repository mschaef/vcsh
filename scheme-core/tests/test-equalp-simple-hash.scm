(use-package! "unit-test")

(define-test equal?-simple-hash
  (check (equal? (identity-hash) (identity-hash)))
  (check (equal? {} {}))
  (check (equal? (identity-hash :a 1) (identity-hash :a 1)))
  (check (equal? '{a 1} '{a 1}))
  (check (equal? (identity-hash :a 1 :b 12) (identity-hash :a 1 :b 12)))
  (check (equal? '{a 1 b 12} '{a 1 b 12}))
  (check (equal? '{2943 a 2321 b} '{2321 b 2943 a}))
  (check (equal? '{(h e l l o - w o r l d) 123
                       (f r o b o z z l e) 23
                       [1 2 3 4 2 2 3] 23}
                     '{(f r o b o z z l e) 23
                       [1 2 3 4 2 2 3] 23
                       (h e l l o - w o r l d) 123})))

