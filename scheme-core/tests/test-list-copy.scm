(use-package! "unit-test")

(define-test list-copy

  (let ((xs (list)))
    (test-case (eq? () (list-copy xs))))
  
  (test-case (eq? :atom (list-copy :atom)))
  (test-case (eq? 12 (list-copy 12)))
  
  (let ((xs (list 1)))
    (test-case (not (eq? xs (list-copy xs))))
    (test-case (equal? xs (list-copy xs))))
  
  (let ((xs (list 1 2 3 4 5)))
    (test-case (not (eq? xs (list-copy xs))))
    (test-case (equal? xs (list-copy xs))))
  
  (let ((xs `(1 2 3 4 . 5)))
    (test-case (not (eq? xs (list-copy xs))))
    (test-case (equal? xs (list-copy xs)))))

    
