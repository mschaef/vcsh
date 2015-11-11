(use-package! "unit-test")

(define-test list-duplicates
  (test-case (runtime-error? (duplicates :not-a-list)))
  (test-case (runtime-error? (duplicates '(1 . :minimal-improper-list))))
  (test-case (runtime-error? (duplicates '(1 2 3 4 5 . :longer-improper-list))))

  (test-case (equal? () (duplicates '())))
  (test-case (equal? () (duplicates '(0))))
  (test-case (equal? () (duplicates '(0 1))))

  (test-case (equal? '(1) (duplicates '(0 1 1))))

  (test-case (equal? '(1) (duplicates '(0 1 1 1))))

  (test-case (equal? '(1) (duplicates '(1 0 1 1 1 2 3 4 1))))

  (test-case (equal? '(y z) (duplicates '(x y z z y))))

  (test-case (equal? () (duplicates (iseq 1 10))))

  (test-case (equal? '(1) (duplicates (cons 1 (iseq 1 10)))))

  (test-case (equal? '(1) (duplicates (append (iseq 1 10) (cons 1))))))


;; (define-test delete-duplicates
;;   (test-case (runtime-error? (delete-duplicates :not-a-list)))
;;   (test-case (runtime-error? (delete-duplicates '(1 . :minimal-improper-list))))
;;   (test-case (runtime-error? (delete-duplicates '(1 2 3 4 5 . :longer-improper-list))))

;;   (test-case (equal? '() (delete-duplicates '())))
;;   (test-case (equal? '(0) (delete-duplicates '(0))))
;;   (test-case (equal? '(0 1) (delete-duplicates '(0 1))))

;;   (test-case (equal? '(0 1) (delete-duplicates '(0 1 1))))

;;   (test-case (equal? '(0 1) (delete-duplicates '(0 1 1 1))))

;;   (test-case (equal? '(1 0 2 3 4) (delete-duplicates '(1 0 1 1 1 2 3 4 1))))

;;   (let ((big-list (iseq 1 200)))
;;     (test-case (equal? big-list (delete-duplicates big-list)))

;;     (test-case (equal? big-list (cons 1 (delete-duplicates big-list))))

;;     (test-case (equal? big-list (delete-duplicates (append big-list (cons 1)))))))
