(use-package! "unit-test")

(define-test properties/symbols
  (let* ((test-symbol (gensym "property-test-symbol"))
         (test-symbol-name (symbol-name test-symbol)))

    ;; no properties, ever
    (check (symbol? test-symbol))
    (check (string? test-symbol-name))

    (check (equal? () (scheme::%property-list test-symbol)))
    (check (equal? () (properties test-symbol)))
    (check (equal? :marker (get-property test-symbol :no-properties :marker)))
    (check (equal? #f (get-property test-symbol :no-properties)))

    ;; one property
    (set-property! test-symbol 'test-property-0 :test-value)

    (check (symbol? test-symbol))
    (check (string? test-symbol-name))
    (check (equal? '((test-property-0 . :test-value)) (scheme::%property-list test-symbol)))
    (check (equal? '(test-property-0) (properties test-symbol)))

    (check (equal? :test-value (get-property test-symbol 'test-property-0 :marker)))
    (check (equal? :test-value (get-property test-symbol 'test-property-0)))

    ;; three properties
    (set-property! test-symbol 'test-property-1 :test-value-1)
    (set-property! test-symbol 'test-property-2 :test-value-2)

    (check (symbol? test-symbol))
    (check (string? test-symbol-name))
    (check (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1)
                         (test-property-0 . :test-value))
                       (scheme::%property-list test-symbol)))
    (check (equal? '(test-property-2 test-property-1 test-property-0) (properties test-symbol)))

    (check (equal? :test-value (get-property test-symbol 'test-property-0)))
    (check (equal? :test-value-1 (get-property test-symbol 'test-property-1)))
    (check (equal? :test-value-2 (get-property test-symbol 'test-property-2)))


    ;; remove the last property on the list (the first added)
    (remove-property! test-symbol 'test-property-0)


    (check (symbol? test-symbol))
    (check (string? test-symbol-name))
    (check (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1))
                       (scheme::%property-list test-symbol)))
    (check (equal? '(test-property-2 test-property-1) (properties test-symbol)))

    (check (equal? #f (get-property test-symbol 'test-property-0)))
    (check (equal? :test-value-1 (get-property test-symbol 'test-property-1)))
    (check (equal? :test-value-2 (get-property test-symbol 'test-property-2)))

    ;; remove the first property on the list (the last added)
    (remove-property! test-symbol 'test-property-2)

    (check (symbol? test-symbol))
    (check (string? test-symbol-name))
    (check (equal? '((test-property-1 . :test-value-1))
                       (scheme::%property-list test-symbol)))
    (check (equal? '(test-property-1) (properties test-symbol)))

    (check (equal? #f (get-property test-symbol 'test-property-0)))
    (check (equal? :test-value-1 (get-property test-symbol 'test-property-1)))
    (check (equal? #f (get-property test-symbol 'test-property-2)))

    ;; Remove the last remaining property, taking us back down to zero
    (remove-property! test-symbol 'test-property-1)
    (check (symbol? test-symbol))
    (check (string? test-symbol-name))

    (check (equal? () (scheme::%property-list test-symbol)))
    (check (equal? () (properties test-symbol)))
    (check (equal? :marker (get-property test-symbol :no-properties :marker)))
    (check (equal? #f (get-property test-symbol :no-properties)))))

(define-test properties/closure
  (let* ((test-closure (lambda (x) (+ x 1))))

    ;; clear out the default properties created by the runtime
    (scheme::%set-property-list! test-closure ())

    ;; no properties
    (check (closure? test-closure))

    (check (equal? () (scheme::%property-list test-closure)))
    (check (equal? () (properties test-closure)))
    (check (equal? :marker (get-property test-closure :no-properties :marker)))
    (check (equal? #f (get-property test-closure :no-properties)))

    ;; one property
    (set-property! test-closure 'test-property-0 :test-value)

    (check (closure? test-closure))
    (check (equal? '((test-property-0 . :test-value)) (scheme::%property-list test-closure)))
    (check (equal? '(test-property-0) (properties test-closure)))

    (check (equal? :test-value (get-property test-closure 'test-property-0 :marker)))
    (check (equal? :test-value (get-property test-closure 'test-property-0)))

    ;; three properties
    (set-property! test-closure 'test-property-1 :test-value-1)
    (set-property! test-closure 'test-property-2 :test-value-2)

    (check (closure? test-closure))
    (check (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1)
                         (test-property-0 . :test-value))
                       (scheme::%property-list test-closure)))
    (check (equal? '(test-property-2 test-property-1 test-property-0) (properties test-closure)))

    (check (equal? :test-value (get-property test-closure 'test-property-0)))
    (check (equal? :test-value-1 (get-property test-closure 'test-property-1)))
    (check (equal? :test-value-2 (get-property test-closure 'test-property-2)))


    ;; remove the last property on the list (the first added)
    (remove-property! test-closure 'test-property-0)


    (check (closure? test-closure))
    (check (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1))
                       (scheme::%property-list test-closure)))
    (check (equal? '(test-property-2 test-property-1) (properties test-closure)))

    (check (equal? #f (get-property test-closure 'test-property-0)))
    (check (equal? :test-value-1 (get-property test-closure 'test-property-1)))
    (check (equal? :test-value-2 (get-property test-closure 'test-property-2)))

    ;; remove the first property on the list (the last added)
    (remove-property! test-closure 'test-property-2)

    (check (closure? test-closure))
    (check (equal? '((test-property-1 . :test-value-1))
                       (scheme::%property-list test-closure)))
    (check (equal? '(test-property-1) (properties test-closure)))

    (check (equal? #f (get-property test-closure 'test-property-0)))
    (check (equal? :test-value-1 (get-property test-closure 'test-property-1)))
    (check (equal? #f (get-property test-closure 'test-property-2)))

    ;; Remove the last remaining property, taking us back down to zero
    (remove-property! test-closure 'test-property-1)
    (check (closure? test-closure))

    (check (equal? () (scheme::%property-list test-closure)))
    (check (equal? () (properties test-closure)))
    (check (equal? :marker (get-property test-closure :no-properties :marker)))
    (check (equal? #f (get-property test-closure :no-properties))))) 

(define-test properties-macro
  (let* ((test-transformer (lambda (x) (+ x 1)))
         (test-macro (scheme::%macrocons test-transformer)))

    ;; clear out the default properties created by the runtime
    (scheme::%set-property-list! test-transformer ())

    ;; no properties
    (check (macro? test-macro))

    (check (equal? () (scheme::%property-list test-transformer)))
    (check (equal? () (properties test-macro)))
    (check (equal? :marker (get-property test-macro :no-properties :marker)))
    (check (equal? #f (get-property test-macro :no-properties)))

    ;; one property
    (set-property! test-macro 'test-property-0 :test-value)

    (check (macro? test-macro))
    (check (equal? '((test-property-0 . :test-value)) (scheme::%property-list test-transformer)))
    (check (equal? '(test-property-0) (properties test-macro)))

    (check (equal? :test-value (get-property test-macro 'test-property-0 :marker)))
    (check (equal? :test-value (get-property test-macro 'test-property-0)))

    ;; three properties
    (set-property! test-macro 'test-property-1 :test-value-1)
    (set-property! test-macro 'test-property-2 :test-value-2)

    (check (macro? test-macro))
    (check (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1)
                         (test-property-0 . :test-value))
                       (scheme::%property-list test-transformer)))
    (check (equal? '(test-property-2 test-property-1 test-property-0) (properties test-macro)))

    (check (equal? :test-value (get-property test-macro 'test-property-0)))
    (check (equal? :test-value-1 (get-property test-macro 'test-property-1)))
    (check (equal? :test-value-2 (get-property test-macro 'test-property-2)))


    ;; remove the last property on the list (the first added)
    (remove-property! test-macro 'test-property-0)

    (check (macro? test-macro))
    (check (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1))
                       (scheme::%property-list test-transformer)))
    (check (equal? '(test-property-2 test-property-1) (properties test-macro)))

    (check (equal? #f (get-property test-macro 'test-property-0)))
    (check (equal? :test-value-1 (get-property test-macro 'test-property-1)))
    (check (equal? :test-value-2 (get-property test-macro 'test-property-2)))

    ;; remove the first property on the list (the last added)
    (remove-property! test-macro 'test-property-2)

    (check (macro? test-macro))
    (check (equal? '((test-property-1 . :test-value-1))
                       (scheme::%property-list test-transformer)))
    (check (equal? '(test-property-1) (properties test-macro)))

    (check (equal? #f (get-property test-macro 'test-property-0)))
    (check (equal? :test-value-1 (get-property test-macro 'test-property-1)))
    (check (equal? #f (get-property test-macro 'test-property-2)))

    ;; Remove the last remaining property, taking us back down to zero
    (remove-property! test-macro 'test-property-1)
    (check (macro? test-macro))

    (check (equal? () (scheme::%property-list test-transformer)))
    (check (equal? () (properties test-macro)))
    (check (equal? :marker (get-property test-macro :no-properties :marker)))
    (check (equal? #f (get-property test-macro :no-properties)))))
