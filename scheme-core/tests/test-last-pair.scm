(use-package! "unit-test")

(define-test last-pair

  (test-case (eq? 12 (last-pair 12)))
  (test-case (eq? 'a (last-pair 'a)))
  (test-case (eq? #\a (last-pair #\a)))

  (test-case (eq? () (last-pair ())))

  (let ((xs '(1)))
    (test-case (eq? xs (last-pair xs))))

  (let ((xs '(1 2 3)))
    (test-case (eq? (last-pair xs) (cddr xs))))

  (let ((xs '(1 2 3 . 4)))
    (test-case (eq? (last-pair xs) (cddr xs)))))

  
  