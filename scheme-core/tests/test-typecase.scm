(use-package! "unit-test")

(define-test typecase
  (test-case (runtime-error? (macroexpand '(typecase x 1 ((foo) bar)))))
  (test-case (runtime-error? (macroexpand '(typecase x ((foo 12) bar)))))
  (test-case (runtime-error? (macroexpand '(typecase x (foo bar)))))
  (test-case (runtime-error? (macroexpand '(typecase x (12 bar)))))

  (test-case (equal? :foo (typecase 1
                            ((fixnum) :foo)
                            ((fixnum) :bar)
                            ((flonum) :xyzzy))))

  (test-case (not (runtime-error? (typecase 'symbol
                                    ((fixnum) 12)))))
  
  (test-case (equal? () (typecase 'symbol
                          ((fixnum) :foo)
                          ((character) :bar)
                          ((flonum) :xyzzy))))

  (test-case (equal? :xyzzy (typecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((flonum symbol) :xyzzy))))

  (test-case (equal? :xyzzy (typecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((symbol flonum) :xyzzy))))

  (test-case (equal? 12 (typecase 'symbol
                          ((fixnum) :foo)
                          ((character) :bar)
                          ((flonum) :xyzzy)
                          (#t 12))))

  (test-case/execution-order 3
    (typecase #\a
      ((fixnum) 12)
      ((character)
       (checkpoint 1)
       (checkpoint 2)
       (checkpoint 3))))
  )

(define-test etypecase
  (test-case (runtime-error? (macroexpand '(etypecase x 1 ((foo) bar)))))
  (test-case (runtime-error? (macroexpand '(etypecase x ((foo 12) bar)))))
  (test-case (runtime-error? (macroexpand '(etypecase x (foo bar)))))
  (test-case (runtime-error? (macroexpand '(etypecase x (12 bar)))))

  (test-case (runtime-error? (macroexpand '(etypecase x ((fixnum) bar) (#t baz)))))

  (test-case (equal? :foo (etypecase 1
                            ((fixnum) :foo)
                            ((fixnum) :bar)
                            ((flonum) :xyzzy))))

  (test-case (runtime-error? (etypecase 'symbol
                               ((fixnum) :foo)
                               ((character) :bar)
                               ((flonum) :xyzzy))))

  (test-case (equal? :xyzzy (etypecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((flonum symbol) :xyzzy))))

  (test-case (equal? :xyzzy (etypecase 'symbol
                              ((fixnum) :foo)
                              ((character) :bar)
                              ((symbol flonum) :xyzzy))))
  (test-case/execution-order 3
    (etypecase #\a
      ((fixnum) 12)
      ((character)
       (checkpoint 1)
       (checkpoint 2)
       (checkpoint 3))))
  )