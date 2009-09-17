(use-package! "unit-test")

(define-test character->string
  (test-case (runtime-error? (character->string 11)))
  (test-case (equal? "a" (character->string #\a))))
