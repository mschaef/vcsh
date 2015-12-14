(use-package! "unit-test")

(define-test break!
  (mvbind (t d) (break! odd? ())
    (check (equal? t ()))
    (check (equal? d ())))
    
  (check (runtime-error? (break! odd? :foo)))
  (check (runtime-error? (break! :not-a-procecdure '(1 2 3))))

  (let* ((xs '(2 4 8 1 2))
         (correct-d (cdddr xs)))
    (mvbind (t d) (break! odd? xs)
      (check (equal? t '(2 4 8)))
      (check (equal? d '(1 2)))
      (check (eq? t xs))
      (check (eq? d correct-d))))

  (let ((xs '(2 4 8 10 2)))
    (mvbind (t d) (break! odd? xs)
      (check (equal? t '(2 4 8 10 2)))
      (check (equal? d '()))
      (check (eq? t xs))))

  (let ((xs '(21 41 81 11 21)))
    (mvbind (t d) (break! odd? xs)
      (check (equal? t '()))
      (check (equal? d '(21 41 81 11 21)))
      (check (eq? d xs))))
  
  (let* ((xs '(2 4 8 1 . 2))
         (correct-d (cdddr xs)))
    (mvbind (t d) (break! odd? xs)
      (check (equal? t '(2 4 8)))
      (check (equal? d '(1 . 2)))
      (check (eq? t xs))
      (check (eq? d correct-d))))
  
  (check (runtime-error? (break! (always #f) '(2 4 8 1 . 2)))))
