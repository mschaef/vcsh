(use-package! "unit-test")

(define-test is-file-basename?
  (test-case (not (is-file-basename? ".")))
  (test-case (not (is-file-basename? "..")))
  (test-case (not (is-file-basename? "./")))
  (test-case (not (is-file-basename? "../")))
  (test-case (is-file-basename? "*"))
  (test-case (is-file-basename? "*.*"))
  (test-case (is-file-basename? "foo"))
  )
  