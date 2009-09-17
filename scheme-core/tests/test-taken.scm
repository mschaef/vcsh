(use-package! "unit-test")

(define-test take!
  (test-case (not (runtime-error? (take! :foo 0))))
  (test-case (runtime-error? (take! `() #\a)))
  (test-case (runtime-error? (take! `(1 2 3) #\a)))
  
  (let* ((xs `(1 2 3 4 5))
         (res (take! xs 0)))
    (test-case (equal? res '()))
    (test-case (eq? res ())))
  (let* ((xs `(1 2 3 4 5))
         (res (take! xs 1)))
    (test-case (equal? res '(1)))
    (test-case (eq? res xs)))
  (let* ((xs `(1 2 3 4 5))
         (res (take! xs 2)))
    (test-case (equal? res '(1 2)))
    (test-case (eq? res xs)))
  (let* ((xs `(1 2 3 4 5))
         (res (take! xs 5)))
    (test-case (equal? res '(1 2 3 4 5)))
    (test-case (eq? res xs)))
  
  (let ((xs `(1 2 3 4 5)))
    (test-case (runtime-error? (take! xs 6))))
  
  (let* ((xs `(1 2 3 4 . 5))
         (res (take! xs 0)))
    (test-case (equal? res '())))

  (let* ((xs `(1 2 3 4 . 5))
         (res (take! xs 1)))
    (test-case (equal? res '(1)))
    (test-case (eq? res xs)))
  (let* ((xs `(1 2 3 4 . 5))
         (res (take! xs 2)))
    (test-case (equal? res '(1 2)))
    (test-case (eq? res xs)))
  (let* ((xs `(1 2 3 4 . 5))
         (res (take! xs 4)))
    (test-case (equal? res '(1 2 3 4)))
    (test-case (eq? res xs)))
  (let ((xs `(1 2 3 4 . 5)))
    (test-case (runtime-error? (take! xs 5)))))
