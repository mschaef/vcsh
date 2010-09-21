(use-package! "unit-test")

(define-test break
  (mvbind (t d) (break odd? ())
    (test-case (equal? t ()))
    (test-case (equal? d ())))
    
  (test-case (runtime-error? (break odd? :foo)))
  (test-case (runtime-error? (break :not-a-procecdure '(1 2 3))))

  (let ((xs '(2 4 8 1 2)))
    (mvbind (t d) (break odd? xs)
      (test-case (equal? t '(2 4 8)))
      (test-case (equal? d '(1 2)))
      (test-case (not (eq? t xs)))
      (test-case (eq? d (cdddr xs)))))

  (let ((xs '(2 4 8 10 2)))
    (mvbind (t d) (break odd? xs)
      (test-case (equal? t '(2 4 8 10 2)))
      (test-case (equal? d '()))
      (test-case (not (eq? t xs)))))

  (let ((xs '(21 41 81 11 21)))
    (mvbind (t d) (break odd? xs)
      (test-case (equal? t '()))
      (test-case (equal? d '(21 41 81 11 21)))
      (test-case (eq? d xs))))
  
  (let ((xs '(2 4 8 1 . 2)))
    (mvbind (t d) (break odd? xs)
      (test-case (equal? t '(2 4 8)))
      (test-case (equal? d '(1 . 2)))
      (test-case (not (eq? t xs)))
      (test-case (eq? d (cdddr xs)))))
  
  (test-case (runtime-error? (break (always #f) '(2 4 8 1 . 2)))))

