(use-package! "unit-test")

(define-test letrec-lambda
  (letrec ((llt-simple 
	    (lambda ()
	      (checkpoint :llt-simple)))
	   (llt-docs
	    (lambda ()
	      "llt-docs documentation"
	      (checkpoint :llt-docs)))
	   (llt-decls 
	    (lambda ()
	      (declare (llt-decls 0))
	      (checkpoint :llt-decls)))
	   (llt-docs/decls 
	    (lambda ()
	      "llt-docs/decls documentation"
	      (declare (llt-decls 0))
	      (checkpoint :llt-docs/decls)))
	       
	       
	   (llt-simple-1-arg 
	    (lambda (x)
	      (checkpoint :llt-simple-1-arg)))
	   (llt-docs-1-arg  
	    (lambda (x)
	      "llt-docs-1-arg documentation"
	      (checkpoint :llt-docs-1-arg)))
	   (llt-decls-1-arg  
	    (lambda (x)
	      (declare (llt-decls 0))
	      (checkpoint :llt-decls-1-arg)))
	   (llt-docs/decls-1-arg  
	    (lambda (x)
	      "llt-docs/decls-1-arg documentation"
	      (declare (llt-decls 0))
	      (checkpoint :llt-docs/decls-1-arg)))
	   
	   (llt-simple-3-args  
	    (lambda (x y z)
	      (checkpoint :llt-simple-3-args)))
	   (llt-docs-3-args  
	    (lambda (x y z)
	      "llt-docs-3-args documentation"
	      (checkpoint :llt-docs-3-args)))
	   (llt-decls-3-args  
	    (lambda (x y z)
	      (declare (llt-decls 0))
	      (checkpoint :llt-decls-3-args)))
	   (llt-docs/decls-3-args  
	    (lambda (x y z)
	      "llt-docs/decls-3-args documentation"
	      (declare (llt-decls 0))
	      (checkpoint :llt-docs/decls-3-args))))

    (test-case (equal? (documentation llt-simple) #f))
    (test-case (equal? (documentation llt-docs) "llt-docs documentation"))
    (test-case (equal? (documentation llt-decls) #f))
    (test-case (equal? (documentation llt-docs/decls) "llt-docs/decls documentation"))
    
    (test-case (equal? (documentation llt-simple-1-arg) #f))
    (test-case (equal? (documentation llt-docs-1-arg) "llt-docs-1-arg documentation"))
    (test-case (equal? (documentation llt-decls-1-arg) #f))
    (test-case (equal? (documentation llt-docs/decls-1-arg) "llt-docs/decls-1-arg documentation"))
    
    (test-case (equal? (documentation llt-simple-3-args) #f))
    (test-case (equal? (documentation llt-docs-3-args) "llt-docs-3-args documentation"))
    (test-case (equal? (documentation llt-decls-3-args) #f))
    (test-case (equal? (documentation llt-docs/decls-3-args) "llt-docs/decls-3-args documentation"))
    
    (test-case (equal? (procedure-name llt-simple) #f))
    (test-case (equal? (procedure-name llt-docs) #f))
    (test-case (equal? (procedure-name llt-decls) #f))
    (test-case (equal? (procedure-name llt-docs/decls) #f))
    
    (test-case (equal? (procedure-name llt-simple-1-arg) #f))
    (test-case (equal? (procedure-name llt-docs-1-arg) #f))
    (test-case (equal? (procedure-name llt-decls-1-arg) #f))
    (test-case (equal? (procedure-name llt-docs/decls-1-arg) #f))
    
    (test-case (equal? (procedure-name llt-simple-3-args) #f))
    (test-case (equal? (procedure-name llt-docs-3-args) #f))
    (test-case (equal? (procedure-name llt-decls-3-args) #f))
    (test-case (equal? (procedure-name llt-docs/decls-3-args) #f))
    
    (test-case (equal? (dtu-lambda-list llt-simple) ()))
    (test-case (equal? (dtu-lambda-list llt-docs) ()))
    (test-case (equal? (dtu-lambda-list llt-decls) ()))
    (test-case (equal? (dtu-lambda-list llt-docs/decls) ()))
    
    (test-case (equal? (dtu-lambda-list llt-simple-1-arg) '(x)))
    (test-case (equal? (dtu-lambda-list llt-docs-1-arg) '(x)))
    (test-case (equal? (dtu-lambda-list llt-decls-1-arg) '(x)))
    (test-case (equal? (dtu-lambda-list llt-docs/decls-1-arg) '(x)))
    
    (test-case (equal? (dtu-lambda-list llt-simple-3-args) '(x y z)))
    (test-case (equal? (dtu-lambda-list llt-docs-3-args) '(x y z)))
    (test-case (equal? (dtu-lambda-list llt-decls-3-args) '(x y z)))
    (test-case (equal? (dtu-lambda-list llt-docs/decls-3-args) '(x y z)))
    
    (test-case (equal? (scheme::procedure-decl? llt-simple 'llt-decls) #f))
    (test-case (equal? (scheme::procedure-decl? llt-docs 'llt-decls) #f))
    (test-case (equal? (scheme::procedure-decl? llt-decls 'llt-decls) '(0)))
    (test-case (equal? (scheme::procedure-decl? llt-docs/decls 'llt-decls) '(0)))
    
    (test-case (equal? (scheme::procedure-decl? llt-simple-1-arg 'llt-decls) #f))
    (test-case (equal? (scheme::procedure-decl? llt-docs-1-arg 'llt-decls) #f))
    (test-case (equal? (scheme::procedure-decl? llt-decls-1-arg 'llt-decls) '(0)))
    (test-case (equal? (scheme::procedure-decl? llt-docs/decls-1-arg 'llt-decls) '(0)))
    
    (test-case (equal? (scheme::procedure-decl? llt-simple-3-args 'llt-decls) #f))
    (test-case (equal? (scheme::procedure-decl? llt-docs-3-args 'llt-decls)  #f) )
    (test-case (equal? (scheme::procedure-decl? llt-decls-3-args 'llt-decls) '(0)))
    (test-case (equal? (scheme::procedure-decl? llt-docs/decls-3-args 'llt-decls) '(0)))
    
    (test-case/execution-order (:begin :llt-simple :end)
      (checkpoint :begin)
      (llt-simple)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-docs :end)
      (checkpoint :begin)
      (llt-docs)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-decls :end)
      (checkpoint :begin)
      (llt-decls)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-docs/decls :end)
      (checkpoint :begin)
      (llt-docs/decls)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-simple-1-arg :end)
      (checkpoint :begin)
      (llt-simple-1-arg 1)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-docs-1-arg :end)
      (checkpoint :begin)
      (llt-docs-1-arg 1)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-decls-1-arg :end)
      (checkpoint :begin)
      (llt-decls-1-arg 1)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-docs/decls-1-arg :end)
      (checkpoint :begin)
      (llt-docs/decls-1-arg 1)
      (checkpoint :end))
    
    
    (test-case/execution-order (:begin :llt-simple-3-args :end)
      (checkpoint :begin)
      (llt-simple-3-args 1 2 3)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-docs-3-args :end)
      (checkpoint :begin)
      (llt-docs-3-args 1 2 3)
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-decls-3-args :end)
      (checkpoint :begin)
      (llt-decls-3-args 1 2 3 )
      (checkpoint :end))
    
    (test-case/execution-order (:begin :llt-docs/decls-3-args :end)
      (checkpoint :begin)
      (llt-docs/decls-3-args 1 2 3)
      (checkpoint :end))
    
    ))

