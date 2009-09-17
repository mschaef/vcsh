(use-package! "unit-test")

(define-test linear-execution
  (test-case/execution-order 5
    (checkpoint 1)
    (checkpoint 2)
    (checkpoint 3)
    (checkpoint 4)
    (checkpoint 5)))

;
; Boolean test cases
;
