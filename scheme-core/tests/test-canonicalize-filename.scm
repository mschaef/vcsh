(use-package! "unit-test")

(define-test canonicalize-filename
  (test-case (runtime-error? (canonicalize-filename :foo)))
  (test-case (runtime-error? (canonicalize-filename 23)))
  
  (test-case (equal? (canonicalize-filename "") ""))
  (test-case (equal? (canonicalize-filename " ") ""))
  (test-case (equal? (canonicalize-filename "foo") "foo"))
  (test-case (equal? (canonicalize-filename " foo") "foo"))
  (test-case (equal? (canonicalize-filename "foo/") "foo/"))
  (test-case (equal? (canonicalize-filename "foo /") "foo /"))
  (test-case (equal? (canonicalize-filename "/foo") "/foo"))
  (test-case (equal? (canonicalize-filename "/foo/") "/foo/"))
  (test-case (equal? (canonicalize-filename "foo/bar") "foo/bar"))
  (test-case (equal? (canonicalize-filename "foo/bar/") "foo/bar/"))
  (test-case (equal? (canonicalize-filename "/foo/bar") "/foo/bar"))
  (test-case (equal? (canonicalize-filename "/foo/bar/") "/foo/bar/"))
  (test-case (equal? (canonicalize-filename "/foo/ bar/") "/foo/ bar/"))
  (test-case (equal? (canonicalize-filename "foo/bar/baz/xyzzy/fubar/snafu/") "foo/bar/baz/xyzzy/fubar/snafu/"))
  (test-case (equal? (canonicalize-filename "/foo/bar/baz/xyzzy/fubar/snafu/") "/foo/bar/baz/xyzzy/fubar/snafu/"))
  (test-case (equal? (canonicalize-filename "../bar/") "../bar/"))
  (test-case (equal? (canonicalize-filename "./bar/") "bar/"))
  (test-case (equal? (canonicalize-filename "foo/../bar/") "bar/"))
  (test-case (equal? (canonicalize-filename "foo/./bar/") "foo/bar/"))
  (test-case (equal? (canonicalize-filename "../") "../"))
  (test-case (equal? (canonicalize-filename "..//") "../"))
  (test-case (equal? (canonicalize-filename ".././") "../"))
  (test-case (equal? (canonicalize-filename "/") "/"))
  (test-case (equal? (canonicalize-filename "//") "/"))
  (test-case (equal? (canonicalize-filename "/./") "/"))
  (test-case (equal? (canonicalize-filename "**//") "**/"))
  (test-case (equal? (canonicalize-filename "**/./") "**/"))
  (test-case (equal? (canonicalize-filename "**/../") "**/../"))
  (test-case (equal? (canonicalize-filename "/**/") "/**/"))
  (test-case (equal? (canonicalize-filename "/**//") "/**/"))
  (test-case (equal? (canonicalize-filename "////") "/"))
  (test-case (equal? (canonicalize-filename "/**") "/**/*"))
  (test-case (equal? (canonicalize-filename "//**") "/**/*"))
  (test-case (runtime-error? (canonicalize-filename "/../"))))
