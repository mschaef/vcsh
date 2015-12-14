(use-package! "unit-test")

(define-test filename-basename
  (check (equal? (filename-basename "") #f))
  
  (check (equal? (filename-basename "*") "*"))
  (check (equal? (filename-basename "foo/*") "*"))
  (check (equal? (filename-basename "/foo/*") "*"))

  (check (equal? (filename-basename "bar") "bar"))
  (check (equal? (filename-basename "foo/bar") "bar"))
  (check (equal? (filename-basename "/foo/bar") "bar"))

  (check (equal? (filename-basename "./bar") "bar"))
  (check (equal? (filename-basename "./foo/bar") "bar"))
  (check (equal? (filename-basename "./foo/bar") "bar"))
  
  (check (equal? (filename-basename "../bar") "bar"))
  (check (equal? (filename-basename "../foo/bar") "bar"))
  (check (equal? (filename-basename "../foo/bar") "bar"))
  
  (check (equal? (filename-basename "foo/") #f))
  (check (equal? (filename-basename "/foo/") #f))
  (check (equal? (filename-basename "./foo/") #f))
  (check (equal? (filename-basename "./foo/bar/") #f))
  (check (equal? (filename-basename "../bar/") #f))
  (check (equal? (filename-basename "../foo/bar/") #f)))
