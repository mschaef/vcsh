(use-package! "unit-test")

(define-test filename-basename
  (test-case (equal? (filename-basename "") #f))
  
  (test-case (equal? (filename-basename "*") "*"))
  (test-case (equal? (filename-basename "foo/*") "*"))
  (test-case (equal? (filename-basename "/foo/*") "*"))

  (test-case (equal? (filename-basename "bar") "bar"))
  (test-case (equal? (filename-basename "foo/bar") "bar"))
  (test-case (equal? (filename-basename "/foo/bar") "bar"))

  (test-case (equal? (filename-basename "./bar") "bar"))
  (test-case (equal? (filename-basename "./foo/bar") "bar"))
  (test-case (equal? (filename-basename "./foo/bar") "bar"))
  
  (test-case (equal? (filename-basename "../bar") "bar"))
  (test-case (equal? (filename-basename "../foo/bar") "bar"))
  (test-case (equal? (filename-basename "../foo/bar") "bar"))
  
  (test-case (equal? (filename-basename "foo/") #f))
  (test-case (equal? (filename-basename "/foo/") #f))
  (test-case (equal? (filename-basename "./foo/") #f))
  (test-case (equal? (filename-basename "./foo/bar/") #f))
  (test-case (equal? (filename-basename "../bar/") #f))
  (test-case (equal? (filename-basename "../foo/bar/") #f))
  )
