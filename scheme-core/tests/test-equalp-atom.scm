(use-package! "unit-test")

(define-test equal?-atom
  (check (equal? 1 1))
  (check (equal? 1.0 1.0))
  (check (equal? #\a #\a))
  (check (equal? "" ""))
  (check (equal? "hello" "hello"))
  (check (equal? 'a 'a))
  (check (equal? :key1 :key1))
  (check (equal? #t #t))
  (check (equal? #f #f))
  (check (equal? () ()))
  (check (not (equal? 1 2)))
  (check (not (equal? 1.0 1.2)))
  (check (not (equal? #\a #\b)))
  (check (not (equal? "" "foo")))
  (check (not (equal? "foo" "")))
  (check (not (equal? "hello" "world")))
  (check (not (equal? 'a 'b)))
  (check (not (equal? :key1 :key3)))
  (check (not (equal? #f #t)))
  (check (not (equal? () 12))))
