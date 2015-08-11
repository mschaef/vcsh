(use-package! "unit-test")

(define (random-letter)
  (case (random 2)
    ((0) (integer->char (+ 65 (random 26))))
    ((1) (integer->char (+ 97 (random 26))))))

(define (random-string)
  (let ((o (open-output-string)))
    (repeat 10 (display (random-letter) o))
    (get-output-string o)))

(define-test symbol-printing-package-names
  (let ((original-package *package*)
        (p1-name (random-string))
        (p2-name (random-string))
        (p3-name (random-string))
        (sym1-name (random-string))
        (sym2-name (random-string))
        (symx-name (random-string))
        (symx2-name (random-string))
        (p1 #f)
        (p2 #f)
        (p3 #f))

    (set! p1 (make-package! p1-name))
    (test-case (runtime-error? (make-package! p1-name)))

    (set! p2 (make-package! p2-name))
    (test-case (runtime-error? (make-package! p1-name)))
    (test-case (runtime-error? (make-package! p2-name)))

    (set! p3 (make-package! p3-name))
    (test-case (runtime-error? (make-package! p1-name)))
    (test-case (runtime-error? (make-package! p2-name)))
    (test-case (runtime-error? (make-package! p3-name)))

    (use-package! p2 p3)

    (test-case (package? p1))
    (test-case (package? p2))
    (test-case (equal? (package-name p1) p1-name))
    (test-case (equal? (package-name p2) p2-name))

    (let ((sym1 (intern! sym1-name))
          (sym2 (intern! sym2-name p1-name))
          (sym3 (intern! sym2-name p2-name))
          (symx (intern! symx-name p2-name))
          (symx2 (intern! symx2-name p2-name))
          )

      ;; no explicit package in printed representation
      (test-case (equal? (write-to-string sym1) sym1-name))

      ;; explicit package with private qualifier
      (test-case (equal? (write-to-string sym2) (string-append p1-name "::" sym2-name)))
      (test-case (equal? (write-to-string sym3) (string-append p2-name "::" sym2-name)))

      (test-case (eq? (read-from-string (string-append p1-name "::" sym2-name)) sym2))
      (test-case (read-error? (read-from-string (string-append p1-name ":" sym2-name))))
      (test-case (eq? (read-from-string (string-append p2-name "::" sym2-name)) sym3))
      (test-case (read-error? (read-from-string (string-append p2-name ":" sym2-name))))

      ;; By default, export! exports from *package*, so these should fail since
      ;; neither sym2 nor sym3 are in *package*.
      (test-case (runtime-error? (export! sym2)))
      (test-case (runtime-error? (export! sym3)))

      (export! sym2 p1-name)
      (export! sym3 p2-name)

      (test-case (equal? (write-to-string sym1) sym1-name))
      (test-case (equal? (write-to-string sym2) (string-append p1-name ":" sym2-name)))
      (test-case (equal? (write-to-string sym3) (string-append p2-name ":" sym2-name)))

      (test-case (eq? (read-from-string (string-append p1-name "::" sym2-name)) sym2))
      (test-case (eq? (read-from-string (string-append p1-name ":" sym2-name)) sym2))
      (test-case (eq? (read-from-string (string-append p2-name "::" sym2-name)) sym3))
      (test-case (eq? (read-from-string (string-append p2-name ":" sym2-name)) sym3))

      (dynamic-let ((*package* p3))
        ;; Package prefixes should be displayed for un-exported symbols visible only
        ;; through the use list
        (test-case (equal? (write-to-string symx) (string-append p2-name "::" symx-name)))
        (export! symx (find-package p2-name))
        (test-case (equal? (write-to-string symx) symx-name))

        ;; Un-exported symbols visible only through the use list should not
        ;; be referenced if they are read unqualified. What should happen
        ;; instead is that a new symbol should be interned in *package*,
        (let ((s (read-from-string (symbol-name symx2))))
          (test-case (eq? (symbol-package s) p3))
          (test-case (equal? (symbol-name symx2) (write-to-string s))))))))
