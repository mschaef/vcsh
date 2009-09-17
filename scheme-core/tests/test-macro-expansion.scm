(use-package! "unit-test")

(defmacro (test-macro-1 a b c)
  (list a b c))

(defmacro (test-macro-2 . x)
  x)

(define-test macro-expansion
  (test-case
   (equal? '(foo bar baz) (macroexpand '(test-macro-1 foo bar baz))))
  (test-case 
   (equal? '(foo bar baz) (macroexpand '(test-macro-2 foo bar baz)))))
