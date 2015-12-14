(use-package! "unit-test")

(define-test filename->list
  (check (runtime-error? (filename->list 123)))
  (check (runtime-error? (filename->list :sym)))
  
  (check (equal? (filename->list "") '(:relative)))
  (check (equal? (filename->list " ") '(:relative)))
  (check (equal? (filename->list "foo") '(:relative "foo")))
  (check (equal? (filename->list " foo") '(:relative "foo")))
  (check (equal? (filename->list "foo/") '(:relative "foo/")))
  (check (equal? (filename->list "foo /") '(:relative "foo /")))
  (check (equal? (filename->list "/foo") '(:absolute "foo")))
  (check (equal? (filename->list "/foo/") '(:absolute "foo/")))
  (check (equal? (filename->list "foo/bar") '(:relative "foo/" "bar")))
  (check (equal? (filename->list "foo/bar/") '(:relative "foo/" "bar/")))
  (check (equal? (filename->list "/foo/bar") '(:absolute "foo/" "bar")))
  (check (equal? (filename->list "/foo/bar/") '(:absolute "foo/" "bar/")))
  (check (equal? (filename->list "/foo/ bar/") '(:absolute "foo/" " bar/")))

  (check (equal? (filename->list "/foo/../bar/") '(:absolute "bar/")))
  (check (equal? (filename->list "foo/../bar/") '(:relative "bar/")))

  (check (equal? (filename->list "/foo/baz/../../bar/") '(:absolute "bar/")))
  (check (equal? (filename->list "foo/baz/../../bar/") '(:relative "bar/")))
  
  (check (equal? (filename->list "/foo/*/../../bar/") '(:absolute "foo/" :any-dir  :back :back "bar/")))
  (check (equal? (filename->list "foo/*/../../bar/") '(:relative "foo/" :any-dir  :back :back "bar/")))

  (check (equal? (filename->list "/foo/?/../../bar/") '(:absolute "foo/" "?/"  :back :back "bar/")))
  (check (equal? (filename->list "foo/?/../../bar/") '(:relative "foo/" "?/"  :back :back "bar/")))

  (check (equal? (filename->list "/foo/*/baz/../../bar/") '(:absolute "foo/" :any-dir :back "bar/")))
  (check (equal? (filename->list "foo/*/baz/../../bar/") '(:relative "foo/" :any-dir  :back "bar/")))

  (check (equal? (filename->list "foo/bar/baz/xyzzy/fubar/snafu/")
                 '(:relative "foo/" "bar/" "baz/" "xyzzy/" "fubar/" "snafu/")))
    (check (equal? (filename->list "/foo/bar/baz/xyzzy/fubar/snafu/")
                 '(:absolute "foo/" "bar/" "baz/" "xyzzy/" "fubar/" "snafu/")))
  (check (equal? (filename->list "/fill/*/bar/*") ' (:absolute "fill/" :any-dir "bar/" "*")))
  (check (equal? (filename->list "fill/*/bar/*") ' (:relative "fill/" :any-dir "bar/" "*")))

  (check (equal? (filename->list "**/") ' (:relative :any-dirs)))
  (check (equal? (filename->list "**") ' (:relative :any-dirs "*")))

  (check (equal? (filename->list "../**/") ' (:relative :back :any-dirs)))
  (check (equal? (filename->list "../**") ' (:relative :back :any-dirs "*")))

  (check (equal? (filename->list "/**/") ' (:absolute :any-dirs)))
  (check (equal? (filename->list "/**") ' (:absolute :any-dirs "*")))

  (check (equal? (filename->list "/**/../") ' (:absolute :any-dirs :back)))
  (check (equal? (filename->list "../**/../") ' (:relative :back :any-dirs :back)))
  
  (check (equal? (filename->list "../*") ' (:relative :back "*")))
  (check (equal? (filename->list "../../*") ' (:relative :back :back "*")))
  (check (equal? (filename->list ".././../*") ' (:relative :back :back "*")))
  (check (equal? (filename->list "./.././../*") ' (:relative :back :back "*")))
    
    
  (check (runtime-error? (filename->list "/../")))
  (check (runtime-error? (filename->list "/foo/../../")))
  (check (runtime-error? (filename->list "/../../")))
  (check (runtime-error? (filename->list "/./../"))))

