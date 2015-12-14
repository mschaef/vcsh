(require-package! "unit-test")
(use-package! "unit-test")

(require-package! "unit-test-utils")
(use-package! "unit-test-utils")

(define *test-case-path* (filename-path *current-load-file*))

(define (run)
  (dynamic-let ((scheme::*location-mapping* (make-identity-hash)))
    (load-tests *test-case-path*)
    (if (time (run-tests)) 0 1)))



