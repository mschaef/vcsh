(use-package! "unit-test")

(define-test symbols

  (test-case (symbol? (gensym)))

  (let ((gs (gensym)))
    (test-case (runtime-error? (symbol-value gs)))
    (test-case (runtime-error? (eval gs)))

    (scheme::%define-global gs 'foo)

    (test-case (not (runtime-error? (symbol-value gs))))
    (test-case (not (runtime-error? (eval gs))))

    (test-case (eq? 'foo (eval gs)))
    (test-case (eq? 'foo (symbol-value gs)))))

