(use-package! "unit-test")

(define-test filename-path
  (test-case (equal? (filename-path "") "./"))
  
  (test-case (equal? (filename-path "*") "./"))
  (test-case (equal? (filename-path "foo/*") "./foo/"))
  (test-case (equal? (filename-path "/foo/*") "/foo/"))

  (test-case (equal? (filename-path "bar") "./"))
  (test-case (equal? (filename-path "foo/bar") "./foo/"))
  (test-case (equal? (filename-path "/foo/bar") "/foo/"))

  (test-case (equal? (filename-path "./bar") "./"))
  (test-case (equal? (filename-path "./foo/bar") "./foo/"))

  (test-case (equal? (filename-path "../bar") "../"))
  (test-case (equal? (filename-path "../foo/bar") "../foo/"))
  (test-case (equal? (filename-path "../foo/bar/") "../foo/bar/"))
  ) 
  
