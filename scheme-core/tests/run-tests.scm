
(require-package! "unit-test")
(use-package! "unit-test")

(define *test-case-path* (filename-path *current-load-file*))

(define (run)
  (load-tests *test-case-path*)
  (if (time (test)) 0 1))



