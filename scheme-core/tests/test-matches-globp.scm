(use-package! "unit-test")

(define-test matches-glob?
  (check (not (matches-glob? "foo" "")))
  (check (matches-glob? "foo" "*"))
  
  (check (matches-glob? "foo" "foo"))
  (check (matches-glob? "foo" "fo?"))
  (check (matches-glob? "foo" "f?o"))
  (check (matches-glob? "foo" "f??"))  
  (check (matches-glob? "foo" "?oo"))
  (check (matches-glob? "foo" "?o?"))
  (check (matches-glob? "foo" "??o"))
  (check (matches-glob? "foo" "???"))

  (check (matches-glob? "foo" "foo"))
  (check (matches-glob? "foo" "fo*"))
  (check (matches-glob? "foo" "f*o"))
  (check (matches-glob? "foo" "f**"))  
  (check (matches-glob? "foo" "*oo"))
  (check (matches-glob? "foo" "*o*"))
  (check (matches-glob? "foo" "**o"))
  (check (matches-glob? "foo" "***"))

  (check (matches-glob? "xxxxxx" "x*xx"))
  (check (matches-glob? "xxxxxxx" "x*xx"))

  (check (not (matches-glob? "foo" "FOO")))

  (check (not (matches-glob? "langtest.scm" "test*.scm")))

  (check (matches-glob? "foofoobar" "foo*bar"))
  (check (matches-glob? "foofoobar" "*foobar"))
  (check (matches-glob? "foofoobar" "*foobar*"))
  (check (matches-glob? "foofoobar" "*f*f*b*"))

  (check (matches-glob? "langtest.scm" "*.*"))
  (check (matches-glob? "langtest.scm" "*.scm"))
  (check (matches-glob? "langtest.scm" "l*.scm"))
  (check (matches-glob? "langtest.scm" "*test.scm"))
  (check (matches-glob? "langtest.scm" "?*.scm")))
