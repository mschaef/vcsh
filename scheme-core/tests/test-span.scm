(use-package! "unit-test")

(define-test span
  (mvbind (t d) (span even? ())
    (check (equal? t ()))
    (check (equal? d ())))
    
  (check (runtime-error? (span even? :foo)))
  (check (runtime-error? (span :not-a-procecdure '(1 2 3))))

  (let ((xs '(2 4 8 1 2)))
    (mvbind (t d) (span even? xs)
      (check (equal? t '(2 4 8)))
      (check (equal? d '(1 2)))
      (check (not (eq? t xs)))
      (check (eq? d (cdddr xs)))))

  (let ((xs '(2 4 8 10 2)))
    (mvbind (t d) (span even? xs)
      (check (equal? t '(2 4 8 10 2)))
      (check (equal? d '()))
      (check (not (eq? t xs)))))

  (let ((xs '(21 41 81 11 21)))
    (mvbind (t d) (span even? xs)
      (check (equal? t '()))
      (check (equal? d '(21 41 81 11 21)))
      (check (eq? d xs))))
  
  (let ((xs '(2 4 8 1 . 2)))
    (mvbind (t d) (span even? xs)
      (check (equal? t '(2 4 8)))
      (check (equal? d '(1 . 2)))
      (check (not (eq? t xs)))
      (check (eq? d (cdddr xs)))))
  
  (check (runtime-error? (span (always #t) '(2 4 8 1 . 2)))))


