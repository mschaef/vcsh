(use-package! "unit-test")

(define-test last-pair

  (check (eq? 12 (last-pair 12)))
  (check (eq? 'a (last-pair 'a)))
  (check (eq? #\a (last-pair #\a)))

  (check (eq? () (last-pair ())))

  (let ((xs '(1)))
    (check (eq? xs (last-pair xs))))

  (let ((xs '(1 2 3)))
    (check (eq? (last-pair xs) (cddr xs))))

  (let ((xs '(1 2 3 . 4)))
    (check (eq? (last-pair xs) (cddr xs)))))

  
  
