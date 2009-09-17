(use-package! "unit-test")

(define-test properties/symbols
  (let* ((test-symbol (gensym "property-test-symbol"))
         (test-symbol-name (symbol-name test-symbol)))

    ;; no properties, ever
    (test-case (symbol? test-symbol))
    (test-case (string? test-symbol-name))

    (test-case (equal? () (scheme::%property-list test-symbol)))
    (test-case (equal? () (properties test-symbol)))
    (test-case (equal? :marker (get-property test-symbol :no-properties :marker)))
    (test-case (equal? #f (get-property test-symbol :no-properties)))

    ;; one property
    (set-property! test-symbol 'test-property-0 :test-value)

    (test-case (symbol? test-symbol))
    (test-case (string? test-symbol-name))
    (test-case (equal? '((test-property-0 . :test-value)) (scheme::%property-list test-symbol)))
    (test-case (equal? '(test-property-0) (properties test-symbol)))

    (test-case (equal? :test-value (get-property test-symbol 'test-property-0 :marker)))
    (test-case (equal? :test-value (get-property test-symbol 'test-property-0)))

    ;; three properties
    (set-property! test-symbol 'test-property-1 :test-value-1)
    (set-property! test-symbol 'test-property-2 :test-value-2)

    (test-case (symbol? test-symbol))
    (test-case (string? test-symbol-name))
    (test-case (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1)
                         (test-property-0 . :test-value))
                       (scheme::%property-list test-symbol)))
    (test-case (equal? '(test-property-2 test-property-1 test-property-0) (properties test-symbol)))

    (test-case (equal? :test-value (get-property test-symbol 'test-property-0)))
    (test-case (equal? :test-value-1 (get-property test-symbol 'test-property-1)))
    (test-case (equal? :test-value-2 (get-property test-symbol 'test-property-2)))


    ;; remove the last property on the list (the first added)
    (remove-property! test-symbol 'test-property-0)


    (test-case (symbol? test-symbol))
    (test-case (string? test-symbol-name))
    (test-case (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1))
                       (scheme::%property-list test-symbol)))
    (test-case (equal? '(test-property-2 test-property-1) (properties test-symbol)))

    (test-case (equal? #f (get-property test-symbol 'test-property-0)))
    (test-case (equal? :test-value-1 (get-property test-symbol 'test-property-1)))
    (test-case (equal? :test-value-2 (get-property test-symbol 'test-property-2)))

    ;; remove the first property on the list (the last added)
    (remove-property! test-symbol 'test-property-2)

    (test-case (symbol? test-symbol))
    (test-case (string? test-symbol-name))
    (test-case (equal? '((test-property-1 . :test-value-1))
                       (scheme::%property-list test-symbol)))
    (test-case (equal? '(test-property-1) (properties test-symbol)))

    (test-case (equal? #f (get-property test-symbol 'test-property-0)))
    (test-case (equal? :test-value-1 (get-property test-symbol 'test-property-1)))
    (test-case (equal? #f (get-property test-symbol 'test-property-2)))

    ;; Remove the last remaining property, taking us back down to zero
    (remove-property! test-symbol 'test-property-1)
    (test-case (symbol? test-symbol))
    (test-case (string? test-symbol-name))

    (test-case (equal? () (scheme::%property-list test-symbol)))
    (test-case (equal? () (properties test-symbol)))
    (test-case (equal? :marker (get-property test-symbol :no-properties :marker)))
    (test-case (equal? #f (get-property test-symbol :no-properties)))
    ))

(define-test properties/closure
  (let* ((test-closure (lambda (x) (+ x 1))))

    ;; clear out the default properties created by the runtime
    (scheme::%set-property-list! test-closure ())

    ;; no properties
    (test-case (closure? test-closure))

    (test-case (equal? () (scheme::%property-list test-closure)))
    (test-case (equal? () (properties test-closure)))
    (test-case (equal? :marker (get-property test-closure :no-properties :marker)))
    (test-case (equal? #f (get-property test-closure :no-properties)))

    ;; one property
    (set-property! test-closure 'test-property-0 :test-value)

    (test-case (closure? test-closure))
    (test-case (equal? '((test-property-0 . :test-value)) (scheme::%property-list test-closure)))
    (test-case (equal? '(test-property-0) (properties test-closure)))

    (test-case (equal? :test-value (get-property test-closure 'test-property-0 :marker)))
    (test-case (equal? :test-value (get-property test-closure 'test-property-0)))

    ;; three properties
    (set-property! test-closure 'test-property-1 :test-value-1)
    (set-property! test-closure 'test-property-2 :test-value-2)

    (test-case (closure? test-closure))
    (test-case (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1)
                         (test-property-0 . :test-value))
                       (scheme::%property-list test-closure)))
    (test-case (equal? '(test-property-2 test-property-1 test-property-0) (properties test-closure)))

    (test-case (equal? :test-value (get-property test-closure 'test-property-0)))
    (test-case (equal? :test-value-1 (get-property test-closure 'test-property-1)))
    (test-case (equal? :test-value-2 (get-property test-closure 'test-property-2)))


    ;; remove the last property on the list (the first added)
    (remove-property! test-closure 'test-property-0)


    (test-case (closure? test-closure))
    (test-case (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1))
                       (scheme::%property-list test-closure)))
    (test-case (equal? '(test-property-2 test-property-1) (properties test-closure)))

    (test-case (equal? #f (get-property test-closure 'test-property-0)))
    (test-case (equal? :test-value-1 (get-property test-closure 'test-property-1)))
    (test-case (equal? :test-value-2 (get-property test-closure 'test-property-2)))

    ;; remove the first property on the list (the last added)
    (remove-property! test-closure 'test-property-2)

    (test-case (closure? test-closure))
    (test-case (equal? '((test-property-1 . :test-value-1))
                       (scheme::%property-list test-closure)))
    (test-case (equal? '(test-property-1) (properties test-closure)))

    (test-case (equal? #f (get-property test-closure 'test-property-0)))
    (test-case (equal? :test-value-1 (get-property test-closure 'test-property-1)))
    (test-case (equal? #f (get-property test-closure 'test-property-2)))

    ;; Remove the last remaining property, taking us back down to zero
    (remove-property! test-closure 'test-property-1)
    (test-case (closure? test-closure))

    (test-case (equal? () (scheme::%property-list test-closure)))
    (test-case (equal? () (properties test-closure)))
    (test-case (equal? :marker (get-property test-closure :no-properties :marker)))
    (test-case (equal? #f (get-property test-closure :no-properties)))
    )) 


(define-test properties-macro
  (let* ((test-transformer (lambda (x) (+ x 1)))
         (test-macro (scheme::%macro test-transformer)))

    ;; clear out the default properties created by the runtime
    (scheme::%set-property-list! test-transformer ())

    ;; no properties
    (test-case (macro? test-macro))

    (test-case (equal? () (scheme::%property-list test-transformer)))
    (test-case (equal? () (properties test-macro)))
    (test-case (equal? :marker (get-property test-macro :no-properties :marker)))
    (test-case (equal? #f (get-property test-macro :no-properties)))

    ;; one property
    (set-property! test-macro 'test-property-0 :test-value)

    (test-case (macro? test-macro))
    (test-case (equal? '((test-property-0 . :test-value)) (scheme::%property-list test-transformer)))
    (test-case (equal? '(test-property-0) (properties test-macro)))

    (test-case (equal? :test-value (get-property test-macro 'test-property-0 :marker)))
    (test-case (equal? :test-value (get-property test-macro 'test-property-0)))

    ;; three properties
    (set-property! test-macro 'test-property-1 :test-value-1)
    (set-property! test-macro 'test-property-2 :test-value-2)

    (test-case (macro? test-macro))
    (test-case (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1)
                         (test-property-0 . :test-value))
                       (scheme::%property-list test-transformer)))
    (test-case (equal? '(test-property-2 test-property-1 test-property-0) (properties test-macro)))

    (test-case (equal? :test-value (get-property test-macro 'test-property-0)))
    (test-case (equal? :test-value-1 (get-property test-macro 'test-property-1)))
    (test-case (equal? :test-value-2 (get-property test-macro 'test-property-2)))


    ;; remove the last property on the list (the first added)
    (remove-property! test-macro 'test-property-0)

    (test-case (macro? test-macro))
    (test-case (equal? '((test-property-2 . :test-value-2)
                         (test-property-1 . :test-value-1))
                       (scheme::%property-list test-transformer)))
    (test-case (equal? '(test-property-2 test-property-1) (properties test-macro)))

    (test-case (equal? #f (get-property test-macro 'test-property-0)))
    (test-case (equal? :test-value-1 (get-property test-macro 'test-property-1)))
    (test-case (equal? :test-value-2 (get-property test-macro 'test-property-2)))

    ;; remove the first property on the list (the last added)
    (remove-property! test-macro 'test-property-2)

    (test-case (macro? test-macro))
    (test-case (equal? '((test-property-1 . :test-value-1))
                       (scheme::%property-list test-transformer)))
    (test-case (equal? '(test-property-1) (properties test-macro)))

    (test-case (equal? #f (get-property test-macro 'test-property-0)))
    (test-case (equal? :test-value-1 (get-property test-macro 'test-property-1)))
    (test-case (equal? #f (get-property test-macro 'test-property-2)))

    ;; Remove the last remaining property, taking us back down to zero
    (remove-property! test-macro 'test-property-1)
    (test-case (macro? test-macro))

    (test-case (equal? () (scheme::%property-list test-transformer)))
    (test-case (equal? () (properties test-macro)))
    (test-case (equal? :marker (get-property test-macro :no-properties :marker)))
    (test-case (equal? #f (get-property test-macro :no-properties)))
    ))