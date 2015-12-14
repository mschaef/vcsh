(use-package! "unit-test")

(define-test list-duplicates
  (check (runtime-error? (duplicates :not-a-list)))
  (check (runtime-error? (duplicates '(1 . :minimal-improper-list))))
  (check (runtime-error? (duplicates '(1 2 3 4 5 . :longer-improper-list))))

  (check (equal? () (duplicates '())))
  (check (equal? () (duplicates '(0))))
  (check (equal? () (duplicates '(0 1))))

  (check (equal? '(1) (duplicates '(0 1 1))))

  (check (equal? '(1) (duplicates '(0 1 1 1))))

  (check (equal? '(1) (duplicates '(1 0 1 1 1 2 3 4 1))))

  (check (equal? '(y z) (duplicates '(x y z z y))))

  (check (equal? () (duplicates (iseq 1 10))))

  (check (equal? '(1) (duplicates (cons 1 (iseq 1 10)))))

  (check (equal? '(1) (duplicates (append (iseq 1 10) (cons 1))))))


;; (define-test delete-duplicates
;;   (check (runtime-error? (delete-duplicates :not-a-list)))
;;   (check (runtime-error? (delete-duplicates '(1 . :minimal-improper-list))))
;;   (check (runtime-error? (delete-duplicates '(1 2 3 4 5 . :longer-improper-list))))

;;   (check (equal? '() (delete-duplicates '())))
;;   (check (equal? '(0) (delete-duplicates '(0))))
;;   (check (equal? '(0 1) (delete-duplicates '(0 1))))

;;   (check (equal? '(0 1) (delete-duplicates '(0 1 1))))

;;   (check (equal? '(0 1) (delete-duplicates '(0 1 1 1))))

;;   (check (equal? '(1 0 2 3 4) (delete-duplicates '(1 0 1 1 1 2 3 4 1))))

;;   (let ((big-list (iseq 1 200)))
;;     (check (equal? big-list (delete-duplicates big-list)))

;;     (check (equal? big-list (cons 1 (delete-duplicates big-list))))

;;     (check (equal? big-list (delete-duplicates (append big-list (cons 1)))))))
