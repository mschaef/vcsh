(use-package! "unit-test")

(define-test typecase
  (check (runtime-error? (macroexpand '(typecase x 1 ((foo) bar)))))
  (check (runtime-error? (macroexpand '(typecase x ((foo 12) bar)))))
  (check (runtime-error? (macroexpand '(typecase x (foo bar)))))
  (check (runtime-error? (macroexpand '(typecase x (12 bar)))))

  (check (equal? :foo (typecase 1
                            ((fixnum) :foo)
                            ((flonum) :xyzzy))))

  (check (not (runtime-error? (typecase 'symbol
                                    ((fixnum) 12)))))
  

  (check (equal? :xyzzy (typecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((flonum symbol) :xyzzy))))

  (check (equal? :xyzzy (typecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((symbol flonum) :xyzzy))))

  (check (equal? 12 (typecase 'symbol
                          ((fixnum) :foo)
                          ((character) :bar)
                          ((flonum) :xyzzy)
                          (#t 12))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (typecase #\a
              ((fixnum) 12)
              ((character)
               (checkpoint 1)
               (checkpoint 2)
               (checkpoint 3)))))))

(define-test etypecase
  (check (runtime-error? (macroexpand '(etypecase x 1 ((foo) bar)))))
  (check (runtime-error? (macroexpand '(etypecase x ((foo 12) bar)))))
  (check (runtime-error? (macroexpand '(etypecase x (foo bar)))))
  (check (runtime-error? (macroexpand '(etypecase x (12 bar)))))

  (check (runtime-error? (macroexpand '(etypecase x ((fixnum) bar) (#t baz)))))

  (check (equal? :foo (etypecase 1
                            ((fixnum) :foo)
                            ((flonum) :xyzzy))))

  (check (runtime-error? (etypecase 'symbol
                               ((fixnum) :foo)
                               ((character) :bar)
                               ((flonum) :xyzzy))))

  (check (equal? :xyzzy (etypecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((flonum symbol) :xyzzy))))

  (check (equal? :xyzzy (etypecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((symbol flonum) :xyzzy))))
  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (etypecase #\a
              ((fixnum) 12)
              ((character)
               (checkpoint 1)
               (checkpoint 2)
               (checkpoint 3)))))))
