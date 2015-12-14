(define-package "test-symbols"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test gensym
  (let ((symbol-1 (gensym))
        (symbol-2 (gensym)))
    (check (symbol? symbol-1))
    (check (not (symbol-package symbol-1)))
    
    (check (not (eq? symbol-1 symbol-2)))
    (check (not (equal? (symbol-name symbol-1) (symbol-name symbol-2))))))

(define-test global-symbol/unbound
  (let ((global-name (gensym "gensym-global-name-unbound")))
    (check (not (symbol-bound? global-name)))
    (check (runtime-error? (symbol-value global-name)))
    (check (runtime-error? (eval global-name)))))

(define-test global-symbol/bound
  (let ((global-name (gensym "gensym-global-name-bound"))
        (global-value (gensym "gensym-global-value")))
    (scheme::%define-global global-name global-value)

    (check (eq? global-name (symbol-bound? global-name)))    
    (check (not (runtime-error? (symbol-value global-name))))
    (check (not (runtime-error? (eval global-name))))

    (check (eq? global-value (symbol-value global-name)))
    (check (eq? global-value (eval global-name)))))

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

(define (random-letter)
  (case (random 2)
    ((0) (integer->char (+ 65 (random 26))))
    ((1) (integer->char (+ 97 (random 26))))))

(define (random-string)
  (let ((o (open-output-string)))
    (repeat 10 (display (random-letter) o))
    (get-output-string o)))

(define-test import-symbol
  (let* ((p1-name (random-string))
         (p1 (make-package! p1-name))
         (p2-name (random-string))
         (p2 (make-package! p2-name)))

    (let* ((s1-name (random-string))
           (s1 (intern! s1-name p1)))

      (check (values-eq? (find-symbol s1-name p1) s1 :internal))
      (check (values-eq? (find-symbol s1-name p2) #f #f))
      
      (import! s1 p2)

      (check (values-eq? (find-symbol s1-name p1) s1 :internal))
      (check (values-eq? (find-symbol s1-name p2) s1 :internal))

      (check (eq? (intern! s1-name p2) s1))

      (check (values-eq? (find-symbol s1-name p1) s1 :internal))
      (check (values-eq? (find-symbol s1-name p2) s1 :internal)))

    (let* ((s1-name (random-string))
           (s1 (intern! s1-name p1))
           (s2 (intern! s1-name p2)))
 
      (check (values-eq? (find-symbol s1-name p1) s1 :internal))
      (check (values-eq? (find-symbol s1-name p2) s2 :internal))
      
      (check (runtime-error? (import! s1 p2)))

      (check (values-eq? (find-symbol s1-name p1) s1 :internal))
      (check (values-eq? (find-symbol s1-name p2) s2 :internal))

      (check (eq? (intern! s1-name p2) s2))

      (check (values-eq? (find-symbol s1-name p1) s1 :internal))
      (check (values-eq? (find-symbol s1-name p2) s2 :internal)))))

    


  
(define-test find-symbol
  (let* ((p1-name (random-string))
         (p1 (make-package! p1-name))

         (p2-name (random-string))
         (p2 (make-package! p2-name))

         (s1-name (random-string))
         (s1 (intern! s1-name p1))
         (s2-name (random-string))
         (s2 (intern! s2-name p1))

         (s-bad-name (random-string)))

    ;; Put p1 on p2's use list
    (use-package! p1 p2)

    ;; s1 and s2 are in p1 but not in p2
    (check (values-eq? (find-symbol s1-name p1) s1 :internal))
    (check (values-eq? (find-symbol s1-name p1-name) s1 :internal))
    (check (values-eq? (find-symbol s2-name p1) s2 :internal))
    (check (values-eq? (find-symbol s2-name p1-name) s2 :internal))

    (check (values-eq? (find-symbol s1-name p2) #f #f))
    (check (values-eq? (find-symbol s1-name p2-name) #f #f))
    (check (values-eq? (find-symbol s2-name p2) #f #f))
    (check (values-eq? (find-symbol s2-name p2-name) #f #f))
 
    ;; s-bad is not in p1 or p2
    (check (values-eq? (find-symbol s-bad-name p1) #f #f))
    (check (values-eq? (find-symbol s-bad-name p2) #f #f))

    ;; Export s2 so that it's visible from p2
    (export! s2 p1)

    ;; s1 and s2 are in p1 but only s2 is in p2
    (check (values-eq? (find-symbol s1-name p1) s1 :internal))
    (check (values-eq? (find-symbol s1-name p1-name) s1 :internal))
    (check (values-eq? (find-symbol s2-name p1) s2 :external))
    (check (values-eq? (find-symbol s2-name p1-name) s2 :external))
   
    (check (values-eq? (find-symbol s1-name p2) #f #f))
    (check (values-eq? (find-symbol s1-name p2-name) #f #f))
    (check (values-eq? (find-symbol s2-name p2) s2 :inherited))
    (check (values-eq? (find-symbol s2-name p2-name) s2 :inherited))

    ;; post export, s1 and s2 are in still p1
    (check (values-eq? (find-symbol s1-name p1) s1 :internal))
    (check (values-eq? (find-symbol s1-name p1-name) s1 :internal))
    (check (values-eq? (find-symbol s2-name p1) s2 :external))
    (check (values-eq? (find-symbol s2-name p1-name) s2 :external))

    ;; s-bad is still not in p1 or p2
    (check (values-eq? (find-symbol s-bad-name p1) #f #f))
    (check (values-eq? (find-symbol s-bad-name p2) #f #f))))

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
    (check (runtime-error? (make-package! p1-name)))

    (set! p2 (make-package! p2-name))
    (check (runtime-error? (make-package! p1-name)))
    (check (runtime-error? (make-package! p2-name)))

    (set! p3 (make-package! p3-name))
    (check (runtime-error? (make-package! p1-name)))
    (check (runtime-error? (make-package! p2-name)))
    (check (runtime-error? (make-package! p3-name)))

    (use-package! p2 p3)

    (check (package? p1))
    (check (package? p2))
    (check (equal? (package-name p1) p1-name))
    (check (equal? (package-name p2) p2-name))

    (let ((sym1 (intern! sym1-name))
          (sym2 (intern! sym2-name p1-name))
          (sym3 (intern! sym2-name p2-name))
          (symx (intern! symx-name p2-name))
          (symx2 (intern! symx2-name p2-name)))

      ;; no explicit package in printed representation
      (check (equal? (write-to-string sym1) sym1-name))

      ;; explicit package with private qualifier
      (check (equal? (write-to-string sym2) (string-append p1-name "::" sym2-name)))
      (check (equal? (write-to-string sym3) (string-append p2-name "::" sym2-name)))

      (check (eq? (read-from-string (string-append p1-name "::" sym2-name)) sym2))
      (check (read-error? (read-from-string (string-append p1-name ":" sym2-name))))
      (check (eq? (read-from-string (string-append p2-name "::" sym2-name)) sym3))
      (check (read-error? (read-from-string (string-append p2-name ":" sym2-name))))

      ;; By default, export! exports from *package*, so these should fail since
      ;; neither sym2 nor sym3 are in *package*.
      (check (runtime-error? (export! sym2)))
      (check (runtime-error? (export! sym3)))

      (export! sym2 p1-name)
      (export! sym3 p2-name)

      (check (equal? (write-to-string sym1) sym1-name))
      (check (equal? (write-to-string sym2) (string-append p1-name ":" sym2-name)))
      (check (equal? (write-to-string sym3) (string-append p2-name ":" sym2-name)))

      (check (eq? (read-from-string (string-append p1-name "::" sym2-name)) sym2))
      (check (eq? (read-from-string (string-append p1-name ":" sym2-name)) sym2))
      (check (eq? (read-from-string (string-append p2-name "::" sym2-name)) sym3))
      (check (eq? (read-from-string (string-append p2-name ":" sym2-name)) sym3))

      (dynamic-let ((*package* p3))
        ;; Package prefixes should be displayed for un-exported symbols visible only
        ;; through the use list
        (check (equal? (write-to-string symx) (string-append p2-name "::" symx-name)))
        (export! symx (find-package p2-name))
        (check (equal? (write-to-string symx) symx-name))

        ;; Un-exported symbols visible only through the use list should not
        ;; be referenced if they are read unqualified. What should happen
        ;; instead is that a new symbol should be interned in *package*,
        (let ((s (read-from-string (symbol-name symx2))))
          (check (eq? (symbol-package s) p3))
          (check (equal? (symbol-name symx2) (write-to-string s))))))))
