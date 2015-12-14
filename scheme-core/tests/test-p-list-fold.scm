(use-package! "unit-test")

(define-test p-list-fold
  (define (fold/->a-list name value list)
    "This is a procedure that can be passed into p-list-fold to convert p-list
     name/value pairs into pairs for a-list. Note that the resulting a-list is
     in reverse order."
    (cons (cons name value) list))

  (check (eq? :test (p-list-fold + :test ())))
  (check (runtime-error? (p-list-fold + 0 '(:non-numeric :non-numeric))))
  (check (runtime-error? (p-list-fold (lambda (n v xs) (error "foo")) () '(1 2))))
  (check (not (runtime-error? (p-list-fold (lambda (n v xs) (error "foo")) () ()))))
  (check (runtime-error? (p-list-fold fold/->a-list () :not-a-list)))

  (check (equal? '((:baz . 3) (:bar . 2) (:foo . 1))
                     (p-list-fold fold/->a-list () '(:foo 1 :bar 2 :baz 3))))

  (check (equal? '((:baz) (:bar . 2) (:foo . 1))
                       (p-list-fold fold/->a-list () '(:foo 1 :bar 2 :baz))))

  (check (equal? () (p-list-fold fold/->a-list () ()))))

