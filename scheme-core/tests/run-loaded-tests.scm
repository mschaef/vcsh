; run-loaded-tests.scm
;
; Runs any currently loaded tests.

(require-package! "unit-test")

(define (run) (if (run-tests) 0 1))
