(use-package! "unit-test")

(define-test gensym
  (let ((symbol-1 (gensym))
        (symbol-2 (gensym)))
    (test-case (symbol? symbol-1))
    (test-case (not (symbol-package symbol-1)))
    
    (test-case (not (eq? symbol-1 symbol-2)))
    (test-case (not (equal? (symbol-name symbol-1) (symbol-name symbol-2))))))

(define-test global-symbol/unbound
  (let ((global-name (gensym "gensym-global-name-unbound")))
    (test-case (not (symbol-bound? global-name)))
    (test-case (runtime-error? (symbol-value global-name)))
    (test-case (runtime-error? (eval global-name)))))

(define-test global-symbol/bound
  (let ((global-name (gensym "gensym-global-name-bound"))
        (global-value (gensym "gensym-global-value")))
    (scheme::%define-global global-name global-value)

    (test-case (eq? global-name (symbol-bound? global-name)))    
    (test-case (not (runtime-error? (symbol-value global-name))))
    (test-case (not (runtime-error? (eval global-name))))
    
    (test-case (eq? global-value (symbol-value global-name)))
    (test-case (eq? global-value (eval global-name)))))
