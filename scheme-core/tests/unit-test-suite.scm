
(define-package "unit-test-suite"
  (:requires "unit-test")
  (:uses "scheme" "unit-test"))

(eval-when (:compile-toplevel)
   (include "test*.scm"))

(define (scheme:run)
  (in-package! "unit-test-suite")
  (if (time (test))
      0
      1))