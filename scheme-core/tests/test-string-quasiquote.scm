(use-package! "unit-test")

(define-test string-quasiquote
  (let ((str-var "xyzzy")
        (num-var 12)
        (sym-var :test)
        (empty ""))
    (check (equal? #"$$" "$"))
    (check (equal? #"$$ " "$ "))
    (check (equal? #" $$" " $"))
    (check (equal? #" $$ " " $ "))

    (check (equal? #"${str-var}" "xyzzy"))
    (check (equal? #" ${str-var}" " xyzzy"))
    (check (equal? #"${str-var} " "xyzzy "))
    (check (equal? #" ${str-var} " " xyzzy "))

    (check (equal? #"${num-var}" "12"))
    (check (equal? #" ${num-var}" " 12"))
    (check (equal? #"${num-var} " "12 "))
    (check (equal? #" ${num-var} " " 12 "))

    (check (equal? #"${sym-var}" "test"))
    (check (equal? #" ${sym-var}" " test"))
    (check (equal? #"${sym-var} " "test "))
    (check (equal? #" ${sym-var} " " test "))

    (check (equal? #"$$${str-var}$$${num-var}$$" "$xyzzy$12$"))))

