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
  (test-case (runtime-error? (intern! 12)))
  (test-case (runtime-error? (intern! "test-symbol" 12)))

  (let ((unknown-package (random-string)))
    (test-case (runtime-error? (intern! "xyzzy" unknown-package))))

  (let* ((sym-name (random-string))
         (sym (intern! sym-name "scheme")))
    (test-case (equal? (symbol-name sym) sym-name))
    (test-case (eq? (symbol-package sym) (find-package "scheme"))))

  (let* ((sym-name (random-string))
         (sym (intern! sym-name "unit-test")))
    (test-case (equal? (symbol-name sym) sym-name))
    (test-case (eq? (symbol-package sym) (find-package "unit-test"))))


  (let* ((sym-name (random-string))
         (pkg (find-package "scheme"))
         (sym (intern! sym-name pkg)))
    (test-case (equal? (symbol-name sym) sym-name))
    (test-case (eq? (symbol-package sym) pkg)))

  (let* ((sym-name (random-string))
         (pkg (find-package "unit-test"))
         (sym (intern! sym-name pkg)))
    (test-case (equal? (symbol-name sym) sym-name))
    (test-case (eq? (symbol-package sym) pkg)))

  (let ((sym-name (random-string)))
    (test-case (symbol? (intern! sym-name)))
    (test-case (equal? (symbol-name (intern! sym-name)) sym-name))
    (test-case (eq? (intern! sym-name) (intern! sym-name))))

  (let ((sym-name (random-string)))
    (test-case (eq? (intern! sym-name "scheme") (intern! sym-name "scheme"))))

  )
