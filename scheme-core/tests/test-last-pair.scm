(use-package! "unit-test")

(define-test last-pair

  (test-case (runtime-error? (last-pair 12)))
  (test-case (runtime-error? (last-pair 'a)))
  (test-case (runtime-error? (last-pair #\a)))
  (test-case (runtime-error? (last-pair #(1 2 3))))

  (test-case (eq? () (last-pair ())))

  (let ((xs '(1)))
    (test-case (eq? xs (last-pair xs))))

  (let ((xs '(1 2 3)))
    (test-case (eq? (last-pair xs) (cddr xs))))

  (let ((xs '(1 2 3 . 4)))
    (test-case (eq? (last-pair xs) (cddr xs)))))

  
  