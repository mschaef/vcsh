(use-package! "unit-test")

(define-test list->filename
  (check (runtime-error? (list->filename 'foo)))
  (check (runtime-error? (list->filename 12)))
  (check (runtime-error? (list->filename '(12))))
  (check (runtime-error? (list->filename '(foo))))
  (check (runtime-error? (list->filename '(:crapola))))
  (check (runtime-error? (list->filename '("foo" :crapola "bar"))))
  (check (equal? (list->filename '()) ""))
  (check (equal? (list->filename '(" ")) " "))
  (check (equal? (list->filename '("foo")) "foo"))
  (check (equal? (list->filename '(" foo")) " foo"))
  (check (equal? (list->filename '("foo/")) "foo/"))
  (check (equal? (list->filename '("foo /")) "foo /"))
  (check (equal? (list->filename '(:absolute "foo")) "/foo"))
  (check (equal? (list->filename '(:absolute "foo/")) "/foo/"))
  (check (equal? (list->filename '("foo/" "bar")) "foo/bar"))
  (check (equal? (list->filename '("foo/" "bar/")) "foo/bar/"))
  (check (equal? (list->filename '(:absolute "foo/" "bar")) "/foo/bar"))
  (check (equal? (list->filename '(:absolute "foo/" "bar/")) "/foo/bar/"))
  (check (equal? (list->filename '(:absolute "foo/" " bar/")) "/foo/ bar/"))
  (check (equal? (list->filename '("foo/" "bar/" "baz/" "xyzzy/" "fubar/" "snafu/")) "foo/bar/baz/xyzzy/fubar/snafu/"))

  (check (equal? (list->filename '(:back "bar/")) "../bar/"))
  (check (equal? (list->filename '(:same-dir "bar/")) "bar/"))
  (check (equal? (list->filename '("foo/" :back "bar/")) "foo/../bar/"))
  (check (equal? (list->filename '("foo/" :same-dir "bar/")) "foo/bar/"))
  (check (equal? (list->filename '(:back)) "../"))
  (check (equal? (list->filename '(:back :delim)) "../"))
  (check (equal? (list->filename '(:back :same-dir)) "../"))
  (check (equal? (list->filename '(:absolute)) "/"))
  (check (equal? (list->filename '(:absolute :delim)) "/"))
  (check (equal? (list->filename '(:absolute :same-dir)) "/"))
  (check (equal? (list->filename '(:any-dirs :delim)) "**/"))
  (check (equal? (list->filename '(:any-dirs :same-dir)) "**/"))
  (check (equal? (list->filename '(:any-dirs :back)) "**/../"))
  (check (equal? (list->filename '(:absolute :any-dirs)) "/**/"))
  (check (equal? (list->filename '(:absolute :any-dirs :delim)) "/**/"))
  (check (equal? (list->filename '(:absolute :delim :delim :delim)) "/")))
