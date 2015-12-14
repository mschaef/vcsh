(use-package! "unit-test")

(define-test parse-glob-pattern
  (check (equal? (scheme::parse-glob-pattern "foo") '("foo")))
  (check (equal? (scheme::parse-glob-pattern "*") '(:any-n)))
  (check (equal? (scheme::parse-glob-pattern "**") '(:any-n)))

  (check (equal? (scheme::parse-glob-pattern "***") '(:any-n)))
  (check (equal? (scheme::parse-glob-pattern "**?") '(:any-n)))
  (check (equal? (scheme::parse-glob-pattern "*?*") '(:any-n)))
  (check (equal? (scheme::parse-glob-pattern "*??") '(:any-n)))
  (check (equal? (scheme::parse-glob-pattern "?**") '(:any-n)))
  (check (equal? (scheme::parse-glob-pattern "?*?") '(:any-n)))
  (check (equal? (scheme::parse-glob-pattern "??*") '(:any-n)))

  (check (equal? (scheme::parse-glob-pattern "????*") '(:any-n)))
  (check (equal? (scheme::parse-glob-pattern "foo????*") '("foo" :any-n)))
  (check (equal? (scheme::parse-glob-pattern "foo????*bar") '("foo" :any-n "bar")))
  (check (equal? (scheme::parse-glob-pattern "foo*?*?*?bar") '("foo" :any-n "bar")))

  (check (equal? (scheme::parse-glob-pattern "*.*") '(:any-n "." :any-n)))
  (check (equal? (scheme::parse-glob-pattern "*.txt") '(:any-n ".txt")))
  (check (equal? (scheme::parse-glob-pattern "tests.*") '("tests." :any-n)))
  (check (equal? (scheme::parse-glob-pattern "?") '(:any-1)))
  (check (equal? (scheme::parse-glob-pattern "??") '(:any-1 :any-1)))
  (check (equal? (scheme::parse-glob-pattern "???") '(:any-1 :any-1 :any-1)))
  (check (equal? (scheme::parse-glob-pattern "foo*") '("foo" :any-n)))
  (check (equal? (scheme::parse-glob-pattern "*foo") '(:any-n "foo")))
  (check (equal? (scheme::parse-glob-pattern "foo?") '("foo" :any-1)))
  (check (equal? (scheme::parse-glob-pattern "?foo") '(:any-1 "foo")))
  (check (equal? (scheme::parse-glob-pattern "foo*foo") '("foo" :any-n "foo")))
  (check (equal? (scheme::parse-glob-pattern "foo?foo") '("foo" :any-1 "foo")))
  (check (equal? (scheme::parse-glob-pattern "*foo?foo*") '(:any-n "foo" :any-1 "foo" :any-n)))
  (check (equal? (scheme::parse-glob-pattern "?bar*baz") '(:any-1 "bar" :any-n "baz"))))
