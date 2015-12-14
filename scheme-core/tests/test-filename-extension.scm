(use-package! "unit-test")

(define-test filename-extension
  (check (equal? (filename-extension "") #f))
  
  (check (equal? (filename-extension "*") ""))
  (check (equal? (filename-extension "foo/*") ""))
  (check (equal? (filename-extension "/foo/*") ""))

  (check (equal? (filename-extension "bar.baz") "baz"))
  (check (equal? (filename-extension "foo/bar.baz") "baz"))
  (check (equal? (filename-extension "/foo/bar.baz") "baz"))

  (check (equal? (filename-extension "./bar.baz") "baz"))
  (check (equal? (filename-extension "./foo/bar.baz") "baz"))
  (check (equal? (filename-extension "./foo/bar.baz") "baz"))
  
  (check (equal? (filename-extension "../bar.baz") "baz"))
  (check (equal? (filename-extension "../foo/bar.baz") "baz"))
  (check (equal? (filename-extension "../foo/bar.baz") "baz"))
  
  (check (equal? (filename-extension "foo.baz/") #f))
  (check (equal? (filename-extension "/foo.baz/") #f))
  (check (equal? (filename-extension "./foo.baz/") #f))
  (check (equal? (filename-extension "./foo/bar.baz/") #f))
  (check (equal? (filename-extension "../bar.baz/") #f))
  (check (equal? (filename-extension "../foo/bar.baz/") #f)))
