(use-package! "unit-test")

(define-test matches-glob?
  (test-case (not (matches-glob? "foo" "")))
  (test-case (matches-glob? "foo" "*"))
  
  (test-case (matches-glob? "foo" "foo"))
  (test-case (matches-glob? "foo" "fo?"))
  (test-case (matches-glob? "foo" "f?o"))
  (test-case (matches-glob? "foo" "f??"))  
  (test-case (matches-glob? "foo" "?oo"))
  (test-case (matches-glob? "foo" "?o?"))
  (test-case (matches-glob? "foo" "??o"))
  (test-case (matches-glob? "foo" "???"))

  (test-case (matches-glob? "foo" "foo"))
  (test-case (matches-glob? "foo" "fo*"))
  (test-case (matches-glob? "foo" "f*o"))
  (test-case (matches-glob? "foo" "f**"))  
  (test-case (matches-glob? "foo" "*oo"))
  (test-case (matches-glob? "foo" "*o*"))
  (test-case (matches-glob? "foo" "**o"))
  (test-case (matches-glob? "foo" "***"))

  (test-case (matches-glob? "xxxxxx" "x*xx"))
  (test-case (matches-glob? "xxxxxxx" "x*xx"))

  (test-case (not (matches-glob? "foo" "FOO")))

  (test-case (not (matches-glob? "langtest.scm" "test*.scm")))

  (test-case (matches-glob? "foofoobar" "foo*bar"))
  (test-case (matches-glob? "foofoobar" "*foobar"))
  (test-case (matches-glob? "foofoobar" "*foobar*"))
  (test-case (matches-glob? "foofoobar" "*f*f*b*"))

  (test-case (matches-glob? "langtest.scm" "*.*"))
  (test-case (matches-glob? "langtest.scm" "*.scm"))
  (test-case (matches-glob? "langtest.scm" "l*.scm"))
  (test-case (matches-glob? "langtest.scm" "*test.scm"))
  (test-case (matches-glob? "langtest.scm" "?*.scm"))
  )
