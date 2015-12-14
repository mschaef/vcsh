(use-package! "unit-test")

(define-test letrec-lambda
  (letrec ((llt-simple 
	    (lambda ()
	      (checkpoint :llt-simple)))
	   (llt-docs
	    (lambda ()
	      "llt-docs documentation"
	      (checkpoint :llt-docs)))
	       
	   (llt-simple-1-arg 
	    (lambda (x)
	      (checkpoint :llt-simple-1-arg)))
	   (llt-docs-1-arg  
	    (lambda (x)
	      "llt-docs-1-arg documentation"
	      (checkpoint :llt-docs-1-arg)))
	   
	   (llt-simple-3-args  
	    (lambda (x y z)
	      (checkpoint :llt-simple-3-args)))
	   (llt-docs-3-args  
	    (lambda (x y z)
	      "llt-docs-3-args documentation"
	      (checkpoint :llt-docs-3-args))))

    (check (equal? (documentation llt-simple) #f))
    (check (equal? (documentation llt-docs) "llt-docs documentation"))
    
    (check (equal? (documentation llt-simple-1-arg) #f))
    (check (equal? (documentation llt-docs-1-arg) "llt-docs-1-arg documentation"))
    
    (check (equal? (documentation llt-simple-3-args) #f))
    (check (equal? (documentation llt-docs-3-args) "llt-docs-3-args documentation"))
    
    (check (equal? (procedure-name llt-simple) #f))
    (check (equal? (procedure-name llt-docs) #f))
    
    (check (equal? (procedure-name llt-simple-1-arg) #f))
    (check (equal? (procedure-name llt-docs-1-arg) #f))
    
    (check (equal? (procedure-name llt-simple-3-args) #f))
    (check (equal? (procedure-name llt-docs-3-args) #f))
    
    (check (equal? (dtu-lambda-list llt-simple) ()))
    (check (equal? (dtu-lambda-list llt-docs) ()))
    
    (check (equal? (dtu-lambda-list llt-simple-1-arg) '(x)))
    (check (equal? (dtu-lambda-list llt-docs-1-arg) '(x)))
    
    (check (equal? (dtu-lambda-list llt-simple-3-args) '(x y z)))
    (check (equal? (dtu-lambda-list llt-docs-3-args) '(x y z)))
    
    (check
     (equal? '(:begin :llt-simple :end)
             (checkpoint-order-of
              (checkpoint :begin)
              (llt-simple)
              (checkpoint :end))))
    
    (check
     (equal? '(:begin :llt-docs :end)
             (checkpoint-order-of
              (checkpoint :begin)
              (llt-docs)
              (checkpoint :end))))
    
    (check
     (equal? '(:begin :llt-simple-1-arg :end)
             (checkpoint-order-of
              (checkpoint :begin)
              (llt-simple-1-arg 1)
              (checkpoint :end))))
    
    (check
     (equal? '(:begin :llt-docs-1-arg :end)
             (checkpoint-order-of
              (checkpoint :begin)
              (llt-docs-1-arg 1)
              (checkpoint :end))))
    
    (check
     (equal? '(:begin :llt-simple-3-args :end)
             (checkpoint-order-of
              (checkpoint :begin)
              (llt-simple-3-args 1 2 3)
              (checkpoint :end))))
    
    (check
     (equal? '(:begin :llt-docs-3-args :end)
             (checkpoint-order-of
              (checkpoint :begin)
              (llt-docs-3-args 1 2 3)
              (checkpoint :end))))))

