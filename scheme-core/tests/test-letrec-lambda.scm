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

    (test-case (equal? (documentation llt-simple) #f))
    (test-case (equal? (documentation llt-docs) "llt-docs documentation"))
    
    (test-case (equal? (documentation llt-simple-1-arg) #f))
    (test-case (equal? (documentation llt-docs-1-arg) "llt-docs-1-arg documentation"))
    
    (test-case (equal? (documentation llt-simple-3-args) #f))
    (test-case (equal? (documentation llt-docs-3-args) "llt-docs-3-args documentation"))
    
    (test-case (equal? (procedure-name llt-simple) #f))
    (test-case (equal? (procedure-name llt-docs) #f))
    
    (test-case (equal? (procedure-name llt-simple-1-arg) #f))
    (test-case (equal? (procedure-name llt-docs-1-arg) #f))
    
    (test-case (equal? (procedure-name llt-simple-3-args) #f))
    (test-case (equal? (procedure-name llt-docs-3-args) #f))
    
    (test-case (equal? (dtu-lambda-list llt-simple) ()))
    (test-case (equal? (dtu-lambda-list llt-docs) ()))
    
    (test-case (equal? (dtu-lambda-list llt-simple-1-arg) '(x)))
    (test-case (equal? (dtu-lambda-list llt-docs-1-arg) '(x)))
    
    (test-case (equal? (dtu-lambda-list llt-simple-3-args) '(x y z)))
    (test-case (equal? (dtu-lambda-list llt-docs-3-args) '(x y z)))
    
    (test-case/execution-order (:begin :llt-simple :end)
      (checkpoint :begin)
      (llt-simple)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-docs :end)
      (checkpoint :begin)
      (llt-docs)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-simple-1-arg :end)
      (checkpoint :begin)
      (llt-simple-1-arg 1)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-docs-1-arg :end)
      (checkpoint :begin)
      (llt-docs-1-arg 1)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-simple-3-args :end)
      (checkpoint :begin)
      (llt-simple-3-args 1 2 3)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-docs-3-args :end)
      (checkpoint :begin)
      (llt-docs-3-args 1 2 3)
      (checkpoint :end))))

