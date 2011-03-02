

(define (retrieve-package) *package*)
(define (display-package package) (format (current-error-port) "; LOAD-TIME_PACKAGE = ~s\n" package))

(define (test)
  (with-fasl-file output-fasl-stream "stack-test.scf"
    (fasl-write-op system::FASL_OP_LOADER_APPLY0 (list retrieve-package) output-fasl-stream)
    (fasl-write-op system::FASL_OP_LOADER_PUSH () output-fasl-stream)
    (fasl-write-op system::FASL_OP_LOADER_APPLYN (list display-package 1) output-fasl-stream)
    ))


