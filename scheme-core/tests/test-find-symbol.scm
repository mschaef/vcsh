(use-package! "unit-test")

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
