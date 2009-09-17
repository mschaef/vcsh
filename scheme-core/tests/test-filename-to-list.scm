(use-package! "unit-test")

(define-test filename->list
  (test-case (runtime-error? (filename->list 123)))
  (test-case (runtime-error? (filename->list :sym)))
  
  (test-case (equal? (filename->list "") '(:relative)))
  (test-case (equal? (filename->list " ") '(:relative)))
  (test-case (equal? (filename->list "foo") '(:relative "foo")))
  (test-case (equal? (filename->list " foo") '(:relative "foo")))
  (test-case (equal? (filename->list "foo/") '(:relative "foo/")))
  (test-case (equal? (filename->list "foo /") '(:relative "foo /")))
  (test-case (equal? (filename->list "/foo") '(:absolute "foo")))
  (test-case (equal? (filename->list "/foo/") '(:absolute "foo/")))
  (test-case (equal? (filename->list "foo/bar") '(:relative "foo/" "bar")))
  (test-case (equal? (filename->list "foo/bar/") '(:relative "foo/" "bar/")))
  (test-case (equal? (filename->list "/foo/bar") '(:absolute "foo/" "bar")))
  (test-case (equal? (filename->list "/foo/bar/") '(:absolute "foo/" "bar/")))
  (test-case (equal? (filename->list "/foo/ bar/") '(:absolute "foo/" " bar/")))

  (test-case (equal? (filename->list "/foo/../bar/") '(:absolute "bar/")))
  (test-case (equal? (filename->list "foo/../bar/") '(:relative "bar/")))

  (test-case (equal? (filename->list "/foo/baz/../../bar/") '(:absolute "bar/")))
  (test-case (equal? (filename->list "foo/baz/../../bar/") '(:relative "bar/")))
  
  (test-case (equal? (filename->list "/foo/*/../../bar/") '(:absolute "foo/" :any-dir  :back :back "bar/")))
  (test-case (equal? (filename->list "foo/*/../../bar/") '(:relative "foo/" :any-dir  :back :back "bar/")))

  (test-case (equal? (filename->list "/foo/?/../../bar/") '(:absolute "foo/" "?/"  :back :back "bar/")))
  (test-case (equal? (filename->list "foo/?/../../bar/") '(:relative "foo/" "?/"  :back :back "bar/")))

  (test-case (equal? (filename->list "/foo/*/baz/../../bar/") '(:absolute "foo/" :any-dir :back "bar/")))
  (test-case (equal? (filename->list "foo/*/baz/../../bar/") '(:relative "foo/" :any-dir  :back "bar/")))

  (test-case (equal? (filename->list "foo/bar/baz/xyzzy/fubar/snafu/")
                 '(:relative "foo/" "bar/" "baz/" "xyzzy/" "fubar/" "snafu/")))
    (test-case (equal? (filename->list "/foo/bar/baz/xyzzy/fubar/snafu/")
                 '(:absolute "foo/" "bar/" "baz/" "xyzzy/" "fubar/" "snafu/")))
  (test-case (equal? (filename->list "/fill/*/bar/*") ' (:absolute "fill/" :any-dir "bar/" "*")))
  (test-case (equal? (filename->list "fill/*/bar/*") ' (:relative "fill/" :any-dir "bar/" "*")))

  (test-case (equal? (filename->list "**/") ' (:relative :any-dirs)))
  (test-case (equal? (filename->list "**") ' (:relative :any-dirs "*")))

  (test-case (equal? (filename->list "../**/") ' (:relative :back :any-dirs)))
  (test-case (equal? (filename->list "../**") ' (:relative :back :any-dirs "*")))

  (test-case (equal? (filename->list "/**/") ' (:absolute :any-dirs)))
  (test-case (equal? (filename->list "/**") ' (:absolute :any-dirs "*")))

  (test-case (equal? (filename->list "/**/../") ' (:absolute :any-dirs :back)))
  (test-case (equal? (filename->list "../**/../") ' (:relative :back :any-dirs :back)))
  
  (test-case (equal? (filename->list "../*") ' (:relative :back "*")))
  (test-case (equal? (filename->list "../../*") ' (:relative :back :back "*")))
  (test-case (equal? (filename->list ".././../*") ' (:relative :back :back "*")))
  (test-case (equal? (filename->list "./.././../*") ' (:relative :back :back "*")))
    
    
  (test-case (runtime-error? (filename->list "/../")))
  (test-case (runtime-error? (filename->list "/foo/../../")))
  (test-case (runtime-error? (filename->list "/../../")))
  (test-case (runtime-error? (filename->list "/./../"))))

