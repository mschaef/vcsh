(use-package! "unit-test")

(define-test character->string
  (check (runtime-error? (character->string 11)))
  (check (equal? "a" (character->string #\a))))
