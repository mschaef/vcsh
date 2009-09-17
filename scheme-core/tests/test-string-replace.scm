(use-package! "unit-test")

(define-test string-replace
  (test-case (runtime-error? (string-replace 23 "old" "new")))
  (test-case (runtime-error? (string-replace "string" 23 "new")))
  (test-case (runtime-error? (string-replace "string" "old" 23)))

  (test-case (runtime-error? (string-replace "string" "old")))
  (test-case (runtime-error? (string-replace "string")))

  (test-case (equal? "test" (string-replace "test" "old" "new")))

  (test-case (equal? "test" (string-replace "replace-everything" "replace-everything" "test")))

  (test-case (equal? "foo" (string-replace "foo" "@" "!")))
  (test-case (equal? "foo" (string-replace "foo" "@" "!")))
  (test-case (equal? "f!oo" (string-replace "f@oo" "@" "!")))
  (test-case (equal? "foo!" (string-replace "foo@" "@" "!")))
  (test-case (equal? "!f!oo" (string-replace "@f@oo" "@" "!")))
  (test-case (equal? "!f!oo!" (string-replace "@f@oo@" "@" "!")))

  (test-case (equal? "foo" (string-replace "foo" "@@" "!!")))
  (test-case (equal? "foo" (string-replace "foo" "@@" "!!")))
  (test-case (equal? "f!!oo" (string-replace "f@@oo" "@@" "!!")))
  (test-case (equal? "foo!!" (string-replace "foo@@" "@@" "!!")))
  (test-case (equal? "!!f!!oo" (string-replace "@@f@@oo" "@@" "!!")))
  (test-case (equal? "!!f!!oo!!" (string-replace "@@f@@oo@@" "@@" "!!")))

  (test-case (equal? "!@" (string-replace "@@@" "@@" "!")))

  (let ((original "this should not be changed"))
    (test-case (equal? "this is changed" (string-replace original "should not be" "is")))
    (test-case (equal? original "this should not be changed"))))