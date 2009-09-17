(use-package! "unit-test")

(define-test filename-extension
  (test-case (equal? (filename-extension "") #f))
  
  (test-case (equal? (filename-extension "*") ""))
  (test-case (equal? (filename-extension "foo/*") ""))
  (test-case (equal? (filename-extension "/foo/*") ""))

  (test-case (equal? (filename-extension "bar.baz") "baz"))
  (test-case (equal? (filename-extension "foo/bar.baz") "baz"))
  (test-case (equal? (filename-extension "/foo/bar.baz") "baz"))

  (test-case (equal? (filename-extension "./bar.baz") "baz"))
  (test-case (equal? (filename-extension "./foo/bar.baz") "baz"))
  (test-case (equal? (filename-extension "./foo/bar.baz") "baz"))
  
  (test-case (equal? (filename-extension "../bar.baz") "baz"))
  (test-case (equal? (filename-extension "../foo/bar.baz") "baz"))
  (test-case (equal? (filename-extension "../foo/bar.baz") "baz"))
  
  (test-case (equal? (filename-extension "foo.baz/") #f))
  (test-case (equal? (filename-extension "/foo.baz/") #f))
  (test-case (equal? (filename-extension "./foo.baz/") #f))
  (test-case (equal? (filename-extension "./foo/bar.baz/") #f))
  (test-case (equal? (filename-extension "../bar.baz/") #f))
  (test-case (equal? (filename-extension "../foo/bar.baz/") #f))
  )
