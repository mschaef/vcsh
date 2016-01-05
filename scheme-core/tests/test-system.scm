(define-package "test-system"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test environment-variable/missing
  (dynamic-let ((scheme::*environment-vars* '(("var" . "value"))))
    (check (not (environment-variable "missing")))

    (check (not (environment-variable/boolean "missing")))
    (check (not (environment-variable/boolean "missing" #f)))
    (check (environment-variable/boolean "missing" #t))    

    (check (not (environment-variable/number "missing")))    
    (check (= 42 (environment-variable/number "missing" 42)))))

(define-test environment-variable/name-case
  (dynamic-let ((scheme::*environment-vars* '(("var" . "value"))))
    (check (equal? "value" (environment-variable "var")))
    (check (equal? "value" (environment-variable "VAR")))))

(define-test environment-variable/boolean-values
  (check-for (value '("no" "n" "false" "f" "0" ""))
    (dynamic-let ((scheme::*environment-vars* (alist "var" value)))
      (check (not (environment-variable/boolean "VAR")))))

  (check-for (value '("yes" "y" "true" "t" "1"))
    (dynamic-let ((scheme::*environment-vars* (alist "var" value)))
      (check (environment-variable/boolean "VAR")))))

(define-test environment-variable/number-values
  (dynamic-let ((scheme::*environment-vars* '(("var" . "42"))))
    (check (= 42 (environment-variable/number "VAR"))))

  (dynamic-let ((scheme::*environment-vars* '(("var" . "non-numeric"))))
    (check (not (environment-variable/number "VAR")))))

