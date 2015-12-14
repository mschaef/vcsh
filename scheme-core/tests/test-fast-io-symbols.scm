(use-package! "unit-test")

(define-test fast-io-symbols
    (check (can-fast-io-round-trip? 'x))
    (let* ((a (gensym "test-symbol"))
	   (b (fast-io-round-trip a)))
      (check (symbol? b))
      (check (equal? (symbol-package a) (symbol-package b)))
      (check (equal? (symbol-name a) (symbol-name b)))

      (let* ((as (list a a))
             (bs (fast-io-round-trip as)))
        (check (eq? (first as) (second as))) ; This might as well be an 'assert' for the next case
        (check (eq? (first bs) (second bs)))))
    
    (check (can-fast-io-round-trip? :keyword-symbol)))
