(use-package! "unit-test")

(define-test is-file-basename?
  (check (not (is-file-basename? ".")))
  (check (not (is-file-basename? "..")))
  (check (not (is-file-basename? "./")))
  (check (not (is-file-basename? "../")))
  (check (is-file-basename? "*"))
  (check (is-file-basename? "*.*"))
  (check (is-file-basename? "foo")))
