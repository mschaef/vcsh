(use-package! "unit-test")

(define-test span!
  (mvbind (t d) (span! even? ())
    (test-case (equal? t ()))
    (test-case (equal? d ())))
    
  (test-case (runtime-error? (span! even? :foo)))
  (test-case (runtime-error? (span! :not-a-procecdure `(1 2 3))))

  (let* ((xs `(2 4 8 1 2))
         (correct-d (cdddr xs)))
    (mvbind (t d) (span! even? xs)
      (test-case (equal? t `(2 4 8)))
      (test-case (equal? d `(1 2)))
      (test-case (eq? t xs))
      (test-case (eq? d correct-d))))

  (let ((xs `(2 4 8 10 2)))
    (mvbind (t d) (span! even? xs)
      (test-case (equal? t `(2 4 8 10 2)))
      (test-case (equal? d `()))
      (test-case (eq? t xs))))

  (let ((xs `(21 41 81 11 21)))
    (mvbind (t d) (span! even? xs)
      (test-case (equal? t `()))
      (test-case (equal? d `(21 41 81 11 21)))
      (test-case (eq? d xs))))
  
  (let* ((xs `(2 4 8 1 . 2))
         (correct-d (cdddr xs)))
    (mvbind (t d) (span! even? xs)
      (test-case (equal? t `(2 4 8)))
      (test-case (equal? d `(1 . 2)))
      (test-case (eq? t xs))
      (test-case (eq? d correct-d))))
  
  (test-case (runtime-error? (span! (always #t) `(2 4 8 1 . 2)))))
