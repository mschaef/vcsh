(require-package! "unit-test")

(define (run) (if (run-tests) 0 1))
