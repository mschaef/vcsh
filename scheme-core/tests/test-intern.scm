(use-package! "unit-test")

(define (random-letter)
  (case (random 2)
    ((0) (integer->char (+ 65 (random 26))))
    ((1) (integer->char (+ 97 (random 26))))))

(define (random-string)
  (let ((o (open-output-string)))
    (repeat 10 (display (random-letter) o))
    (get-output-string o)))


(define-test intern!
  (check (runtime-error? (intern! 12)))
  (check (runtime-error? (intern! "test-symbol" 12)))

  (let ((unknown-package (random-string)))
    (check (runtime-error? (intern! "xyzzy" unknown-package))))

  (let* ((sym-name (random-string))
         (sym (intern! sym-name "scheme")))
    (check (equal? (symbol-name sym) sym-name))
    (check (eq? (symbol-package sym) (find-package "scheme"))))

  (let* ((sym-name (random-string))
         (sym (intern! sym-name "unit-test")))
    (check (equal? (symbol-name sym) sym-name))
    (check (eq? (symbol-package sym) (find-package "unit-test"))))


  (let* ((sym-name (random-string))
         (pkg (find-package "scheme"))
         (sym (intern! sym-name pkg)))
    (check (equal? (symbol-name sym) sym-name))
    (check (eq? (symbol-package sym) pkg)))

  (let* ((sym-name (random-string))
         (pkg (find-package "unit-test"))
         (sym (intern! sym-name pkg)))
    (check (equal? (symbol-name sym) sym-name))
    (check (eq? (symbol-package sym) pkg)))

  (let ((sym-name (random-string)))
    (check (symbol? (intern! sym-name)))
    (check (equal? (symbol-name (intern! sym-name)) sym-name))
    (check (eq? (intern! sym-name) (intern! sym-name))))

  (let ((sym-name (random-string)))
    (check (eq? (intern! sym-name "scheme") (intern! sym-name "scheme")))))
