(use-package! "unit-test")


(define-test string-fold
  (define (ccons a d) (cons a d))
  
  (test-case (not (runtime-error? (string-fold cons '() "0123456789"))))
  (test-case (not (runtime-error? (string-fold cons '() ""))))
  (test-case (not (runtime-error? (string-fold ccons '() "0123456789"))))
  (test-case (not (runtime-error? (string-fold ccons '() ""))))

  (test-case (runtime-error? (string-fold cons '() ())))
  (test-case (runtime-error? (string-fold 12 '() "0123456789")))

  (test-case (equal? '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		 (reverse (string-fold cons '() "0123456789"))))
  (test-case (equal? '() (reverse (string-fold cons '() ""))))

  (test-case (equal? '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		 (reverse (string-fold ccons '() "0123456789"))))
  (test-case (equal? '() (reverse (string-fold ccons '() ""))))

  (test-case (runtime-error? (string-fold (lambda (ch chs) (error "foo")) () "12345")))
  (test-case (not (runtime-error? (string-fold (lambda (ch chs) (error "foo")) () ""))))

  (let ((catch-tag (gensym "string-fold-catch-tag"))
	(return-value (gensym "string-fold-return-value")))

    (test-case (eq? (catch catch-tag
		  (string-fold (lambda (ch chs) (throw catch-tag return-value)) () "12345"))
		return-value))

    (test-case/execution-order 4
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
	(checkpoint 4))))
