
(require-package! "unit-test")
(use-package! "unit-test")

(define *test-case-path* (filename-path *current-load-file*))

(define (run)
  (dynamic-let ((scheme::*location-mapping* (make-hash :eq)))
    (load-tests *test-case-path*)
    (if (time (test)) 0 1)))



