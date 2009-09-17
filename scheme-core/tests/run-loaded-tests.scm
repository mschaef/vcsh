; run-loaded-tests.scm
;
; Runs any currently loaded tests.

(require-package! "unit-test")

(define (run) (if (test) 0 1))
