(use-package! "unit-test")

(defmacro (test-macro-1 a b c)
  (list a b c))

(defmacro (test-macro-2 . x)
  x)

(define-test macro-expansion
  (check
   (equal? '(foo bar baz) (macroexpand '(test-macro-1 foo bar baz))))
  (check 
   (equal? '(foo bar baz) (macroexpand '(test-macro-2 foo bar baz)))))
