(use-package! "unit-test")

(define-test gensym
  (let ((symbol-1 (gensym))
        (symbol-2 (gensym)))
    (check (symbol? symbol-1))
    (check (not (symbol-package symbol-1)))
    
    (check (not (eq? symbol-1 symbol-2)))
    (check (not (equal? (symbol-name symbol-1) (symbol-name symbol-2))))))

(define-test global-symbol/unbound
  (let ((global-name (gensym "gensym-global-name-unbound")))
    (check (not (symbol-bound? global-name)))
    (check (runtime-error? (symbol-value global-name)))
    (check (runtime-error? (eval global-name)))))

(define-test global-symbol/bound
  (let ((global-name (gensym "gensym-global-name-bound"))
        (global-value (gensym "gensym-global-value")))
    (scheme::%define-global global-name global-value)

    (check (eq? global-name (symbol-bound? global-name)))    
    (check (not (runtime-error? (symbol-value global-name))))
    (check (not (runtime-error? (eval global-name))))

    (check (eq? global-value (symbol-value global-name)))
    (check (eq? global-value (eval global-name)))))
