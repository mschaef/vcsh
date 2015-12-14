(use-package! "unit-test")


(define-test string-fold
  (define (ccons a d) (cons a d))
  
  (check (not (runtime-error? (string-fold cons '() "0123456789"))))
  (check (not (runtime-error? (string-fold cons '() ""))))
  (check (not (runtime-error? (string-fold ccons '() "0123456789"))))
  (check (not (runtime-error? (string-fold ccons '() ""))))

  (check (runtime-error? (string-fold cons '() ())))
  (check (runtime-error? (string-fold 12 '() "0123456789")))

  (check (equal? '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		 (reverse (string-fold cons '() "0123456789"))))
  (check (equal? '() (reverse (string-fold cons '() ""))))

  (check (equal? '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		 (reverse (string-fold ccons '() "0123456789"))))
  (check (equal? '() (reverse (string-fold ccons '() ""))))

  (check (runtime-error? (string-fold (lambda (ch chs) (error "foo")) () "12345")))
  (check (not (runtime-error? (string-fold (lambda (ch chs) (error "foo")) () ""))))

  (let ((catch-tag (gensym "string-fold-catch-tag"))
	(return-value (gensym "string-fold-return-value")))

    (check (eq? (catch catch-tag
		  (string-fold (lambda (ch chs) (throw catch-tag return-value)) () "12345"))
		return-value))

    (check
     (equal? '(1 2 3 4)
             (checkpoint-order-of
              (checkpoint 1)
              (catch catch-tag
                (checkpoint 2)
                (string-fold 
                 (lambda (ch chs) 
                   (checkpoint 3)
                   (throw catch-tag return-value)
                   (checkpoint #f))
                 () 
                 "12345")
                (checkpoint #f))
              (checkpoint 4))))))
