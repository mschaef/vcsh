(use-package! "unit-test")

(define-test is-directory-basename?
  (check (is-directory-basename? "foo/"))
  (check (is-directory-basename? :back))
  (check (is-directory-basename? :same-dir))
  (check (is-directory-basename? :any-dirs))
  (check (is-directory-basename? :absolute))
  (check (is-directory-basename? :delim))
  (check (is-directory-basename? "/"))
  (check (is-directory-basename? "."))
  (check (is-directory-basename? ".."))
  (check (is-directory-basename? "./"))
  (check (is-directory-basename? "../"))
  (check (is-directory-basename? "/../"))
  (check (is-directory-basename? "/../")))
