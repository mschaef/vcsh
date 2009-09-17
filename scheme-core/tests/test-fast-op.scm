(use-package! "unit-test")

(scheme::define-fast-op :test-fast-op/arity-0 188 0)
(scheme::define-fast-op :test-fast-op/arity-1 189 1)
(scheme::define-fast-op :test-fast-op/arity-2 190 2)

(define-test fast-op
  (test-case (runtime-error? (scheme::assemble-fast-op 16)))
  (test-case (runtime-error? (scheme::assemble-fast-op :invalid-fast-op)))

  ;; Arity 0 cases
  (let ((op-0 (scheme::assemble-fast-op :test-fast-op/arity-0)))
    (test-case (can-fast-io-round-trip? op-0))
    (test-case (scheme::fast-op? op-0))

    (test-case (= 188 (scheme::%fast-op-opcode op-0)))
    (test-case (equal? '(() ())  (scheme::%fast-op-args op-0)))

    (test-case (equal? '() (scheme::fast-op-args op-0)))

    (values-bind (scheme::parse-fast-op op-0) (op args)
       (test-case (eq? :test-fast-op/arity-0 op))
       (test-case (equal? '() args))))

  (test-case (runtime-error? (scheme::assemble-fast-op :test-fast-op/arity-0 'x)))

   ;; Arity 1 cases
  (let ((op-1 (scheme::assemble-fast-op :test-fast-op/arity-1 :operand-1)))
    (test-case (can-fast-io-round-trip? op-1))
    (test-case (scheme::fast-op? op-1))

    (test-case (= 189 (scheme::%fast-op-opcode op-1)))
    (test-case (equal? '(:operand-1 ())  (scheme::%fast-op-args op-1)))

    (test-case (equal? '(:operand-1) (scheme::fast-op-args op-1)))

    (values-bind (scheme::parse-fast-op op-1) (op args)
       (test-case (eq? :test-fast-op/arity-1 op))
       (test-case (equal? '(:operand-1) args))))

  (test-case (runtime-error? (scheme::assemble-fast-op :test-fast-op/arity-1)))
  (test-case (runtime-error? (scheme::assemble-fast-op :test-fast-op/arity-1 'x 'x)))

   ;; Arity 2 cases
  (let ((op-2 (scheme::assemble-fast-op :test-fast-op/arity-2 :operand-1 :operand-2)))
    (test-case (can-fast-io-round-trip? op-2))
    (test-case (scheme::fast-op? op-2))

    (test-case (= 190 (scheme::%fast-op-opcode op-2)))
    (test-case (equal? '(:operand-1 :operand-2)  (scheme::%fast-op-args op-2)))

    (test-case (equal? '(:operand-1 :operand-2) (scheme::fast-op-args op-2)))

    (values-bind (scheme::parse-fast-op op-2) (op args)
       (test-case (eq? :test-fast-op/arity-2 op))
       (test-case (equal? '(:operand-1 :operand-2) args))))

  (test-case (runtime-error? (scheme::assemble-fast-op :test-fast-op/arity-2)))
  (test-case (runtime-error? (scheme::assemble-fast-op :test-fast-op/arity-2 'x)))
  (test-case (runtime-error? (scheme::assemble-fast-op :test-fast-op/arity-2 'x 'x 'x)))

  )
    
