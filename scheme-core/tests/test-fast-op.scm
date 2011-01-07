(use-package! "unit-test")

(compiler::define-fast-op :test-fast-op/arity-0 188 0)
(compiler::define-fast-op :test-fast-op/arity-1 189 1)
(compiler::define-fast-op :test-fast-op/arity-2 190 2)
(compiler::define-fast-op :test-fast-op/arity-3 191 3)

(define-test fast-op
  (test-case (runtime-error? (compiler::assemble-fast-op 16)))
  (test-case (runtime-error? (compiler::assemble-fast-op 188)))
  (test-case (runtime-error? (compiler::assemble-fast-op :invalid-fast-op)))

  ;; Arity 0 cases
  (let ((op-0 (compiler::assemble-fast-op :test-fast-op/arity-0)))
    (test-case (can-fast-io-round-trip? op-0))
    (test-case (compiler::fast-op? op-0))

    (test-case (= 188 (scheme::%fast-op-opcode op-0)))
    (test-case (equal? '(() () ())  (scheme::%fast-op-args op-0)))

    (test-case (equal? '() (compiler::fast-op-args op-0)))

    (mvbind (op args) (compiler::parse-fast-op op-0)
       (test-case (eq? :test-fast-op/arity-0 op))
       (test-case (equal? '() args))))

  (test-case (runtime-error? (compiler::assemble-fast-op :test-fast-op/arity-0 'x)))

   ;; Arity 1 cases
  (let ((op-1 (compiler::assemble-fast-op :test-fast-op/arity-1 :operand-1)))
    (test-case (can-fast-io-round-trip? op-1))
    (test-case (compiler::fast-op? op-1))

    (test-case (= 189 (scheme::%fast-op-opcode op-1)))
    (test-case (equal? '(:operand-1 () ())  (scheme::%fast-op-args op-1)))

    (test-case (equal? '(:operand-1) (compiler::fast-op-args op-1)))

    (mvbind (op args) (compiler::parse-fast-op op-1)
       (test-case (eq? :test-fast-op/arity-1 op))
       (test-case (equal? '(:operand-1) args))))

  (test-case (runtime-error? (compiler::assemble-fast-op :test-fast-op/arity-1)))
  (test-case (runtime-error? (compiler::assemble-fast-op :test-fast-op/arity-1 'x 'x)))

   ;; Arity 2 cases
  (let ((op-2 (compiler::assemble-fast-op :test-fast-op/arity-2 :operand-1 :operand-2)))
    (test-case (can-fast-io-round-trip? op-2))
    (test-case (compiler::fast-op? op-2))

    (test-case (= 190 (scheme::%fast-op-opcode op-2)))
    (test-case (equal? '(:operand-1 :operand-2 ())  (scheme::%fast-op-args op-2)))

    (test-case (equal? '(:operand-1 :operand-2) (compiler::fast-op-args op-2)))

    (mvbind (op args) (compiler::parse-fast-op op-2)
       (test-case (eq? :test-fast-op/arity-2 op))
       (test-case (equal? '(:operand-1 :operand-2) args))))

  (test-case (runtime-error? (compiler::assemble-fast-op :test-fast-op/arity-2)))
  (test-case (runtime-error? (compiler::assemble-fast-op :test-fast-op/arity-2 'x)))
  (test-case (runtime-error? (compiler::assemble-fast-op :test-fast-op/arity-2 'x 'x 'x)))

   ;; Arity 3 cases
  (let ((op-3 (compiler::assemble-fast-op :test-fast-op/arity-3 :operand-1 :operand-2 :operand-3)))
    (test-case (can-fast-io-round-trip? op-3))
    (test-case (compiler::fast-op? op-3))

    (test-case (= 191 (scheme::%fast-op-opcode op-3)))
    (test-case (equal? '(:operand-1 :operand-2 :operand-3)  (scheme::%fast-op-args op-3)))

    (test-case (equal? '(:operand-1 :operand-2 :operand-3) (scheme::%fast-op-args op-3)))

    (mvbind (op args) (compiler::parse-fast-op op-3)
       (test-case (eq? :test-fast-op/arity-3 op))
       (test-case (equal? '(:operand-1 :operand-2 :operand-3) args))))

  (test-case (runtime-error? (compiler::assemble-fast-op :test-fast-op/arity-3)))
  (test-case (runtime-error? (compiler::assemble-fast-op :test-fast-op/arity-3 'x)))
  (test-case (runtime-error? (compiler::assemble-fast-op :test-fast-op/arity-3 'x 'x)))
  (test-case (runtime-error? (compiler::assemble-fast-op :test-fast-op/arity-3 'x 'x 'x 'x)))
  )
    
