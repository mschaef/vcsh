(use-package! "unit-test")

(define-test filename-path
  (check (equal? (filename-path "") "./"))
  
  (check (equal? (filename-path "*") "./"))
  (check (equal? (filename-path "foo/*") "./foo/"))
  (check (equal? (filename-path "/foo/*") "/foo/"))

  (check (equal? (filename-path "bar") "./"))
  (check (equal? (filename-path "foo/bar") "./foo/"))
  (check (equal? (filename-path "/foo/bar") "/foo/"))

  (check (equal? (filename-path "./bar") "./"))
  (check (equal? (filename-path "./foo/bar") "./foo/"))

  (check (equal? (filename-path "../bar") "../"))
  (check (equal? (filename-path "../foo/bar") "../foo/"))
  (check (equal? (filename-path "../foo/bar/") "../foo/bar/"))) 
  
