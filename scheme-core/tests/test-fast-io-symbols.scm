(use-package! "unit-test")

(define-test fast-io-symbols
    (test-case (can-fast-io-round-trip? 'x))
    (let* ((a (gensym "test-symbol"))
	   (b (fast-io-round-trip a)))
      (test-case (symbol? b))
      (test-case (equal? (symbol-package a) (symbol-package b)))
      (test-case (equal? (symbol-name a) (symbol-name b)))

      (let* ((as (list a a))
             (bs (fast-io-round-trip as)))
        (test-case (eq? (first as) (second as))) ; This might as well be an 'assert' for the next case
        (test-case (eq? (first bs) (second bs)))))
    
    (test-case (can-fast-io-round-trip? :keyword-symbol)))
