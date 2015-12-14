(use-package! "unit-test")

(define-test string-replace
  (check (runtime-error? (string-replace 23 "old" "new")))
  (check (runtime-error? (string-replace "string" 23 "new")))
  (check (runtime-error? (string-replace "string" "old" 23)))

  (check (runtime-error? (string-replace "string" "old")))
  (check (runtime-error? (string-replace "string")))

  (check (equal? "test" (string-replace "test" "old" "new")))

  (check (equal? "test" (string-replace "replace-everything" "replace-everything" "test")))

  (check (equal? "foo" (string-replace "foo" "@" "!")))
  (check (equal? "foo" (string-replace "foo" "@" "!")))
  (check (equal? "f!oo" (string-replace "f@oo" "@" "!")))
  (check (equal? "foo!" (string-replace "foo@" "@" "!")))
  (check (equal? "!f!oo" (string-replace "@f@oo" "@" "!")))
  (check (equal? "!f!oo!" (string-replace "@f@oo@" "@" "!")))

  (check (equal? "foo" (string-replace "foo" "@@" "!!")))
  (check (equal? "foo" (string-replace "foo" "@@" "!!")))
  (check (equal? "f!!oo" (string-replace "f@@oo" "@@" "!!")))
  (check (equal? "foo!!" (string-replace "foo@@" "@@" "!!")))
  (check (equal? "!!f!!oo" (string-replace "@@f@@oo" "@@" "!!")))
  (check (equal? "!!f!!oo!!" (string-replace "@@f@@oo@@" "@@" "!!")))

  (check (equal? "!@" (string-replace "@@@" "@@" "!")))

  (let ((original "this should not be changed"))
    (check (equal? "this is changed" (string-replace original "should not be" "is")))
    (check (equal? original "this should not be changed"))))
