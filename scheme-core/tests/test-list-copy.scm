(use-package! "unit-test")

(define-test list-copy

  (let ((xs (list)))
    (check (eq? () (list-copy xs))))
  
  (check (eq? :atom (list-copy :atom)))
  (check (eq? 12 (list-copy 12)))
  
  (let ((xs (list 1)))
    (check (not (eq? xs (list-copy xs))))
    (check (equal? xs (list-copy xs))))
  
  (let ((xs (list 1 2 3 4 5)))
    (check (not (eq? xs (list-copy xs))))
    (check (equal? xs (list-copy xs))))
  
  (let ((xs `(1 2 3 4 . 5)))
    (check (not (eq? xs (list-copy xs))))
    (check (equal? xs (list-copy xs)))))

    
