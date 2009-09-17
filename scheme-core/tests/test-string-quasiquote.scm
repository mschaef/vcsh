(use-package! "unit-test")

(define-test string-quasiquote
  (let ((str-var "xyzzy")
        (num-var 12)
        (sym-var :test)
        (empty ""))
    (test-case (equal? #"$$" "$"))
    (test-case (equal? #"$$ " "$ "))
    (test-case (equal? #" $$" " $"))
    (test-case (equal? #" $$ " " $ "))

    (test-case (equal? #"${str-var}" "xyzzy"))
    (test-case (equal? #" ${str-var}" " xyzzy"))
    (test-case (equal? #"${str-var} " "xyzzy "))
    (test-case (equal? #" ${str-var} " " xyzzy "))

    (test-case (equal? #"${num-var}" "12"))
    (test-case (equal? #" ${num-var}" " 12"))
    (test-case (equal? #"${num-var} " "12 "))
    (test-case (equal? #" ${num-var} " " 12 "))

    (test-case (equal? #"${sym-var}" "test"))
    (test-case (equal? #" ${sym-var}" " test"))
    (test-case (equal? #"${sym-var} " "test "))
    (test-case (equal? #" ${sym-var} " " test "))

    (test-case (equal? #"$$${str-var}$$${num-var}$$" "$xyzzy$12$"))
    ))

