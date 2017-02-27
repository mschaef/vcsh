(define-package "test-local-binding-forms"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))


(define-test let
  (let (unbound-var)
    (check (equal? unbound-var '())))
  (let ((closure (let lp ((x 10)) lp)))
    (check (closure? closure)))
  (let ((x (let lp ((x2 10)) x2)))
    (check (equal? x 10)))

  (let lp ((x 0) (y 10) z)
    (check (equal? z '())))

  (let ((side-effect-0 #f)
	(side-effect-1 #f)
	(side-effect-2 #f))
    (let ((x 0)
	  (y 1)
	  (z 2))
      (set! side-effect-0 #t)
      (check (= x 0))
      (check (= y 1))
      (check (= z 2))
      (set! side-effect-1 #t)
      (set! x 10)
      (incr! y)
      (check (= x 10))
      (check (= y 2))
      (check (= z 2))
      (set! side-effect-2 #t))
    (check side-effect-0)
    (check side-effect-1)
    (check side-effect-2))

  (let ((x 10) 
	(y 20))
    (let ((x 100)
	  (y x))
      (check (= x 100))
      (check (= y 10)))))

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

(define-test let*
  (let ((side-effect-0 #f)
	(side-effect-1 #f)
	(side-effect-2 #f))
    (let* ((x 0)
	  (y 1)
	  (z 2))
      (set! side-effect-0 #t)
      (check (= x 0))
      (check (= y 1))
      (check (= z 2))
      (set! side-effect-1 #t)
      (set! x 10)
      (incr! y)
      (check (= x 10))
      (check (= y 2))
      (check (= z 2))
      (set! side-effect-2 #t))
    (check side-effect-0)
    (check side-effect-1)
    (check side-effect-2))

  (let ((x 10) 
	(y 20))
    (let* ((x 100)
	   (y x))
      (check (= x 100))
      (check (= y 100)))))

(define *dynamic-let-var-1* :dlet1)
(define *dynamic-let-var-2* :dlet2)

(define-test dynamic-let
  (check (equal? *dynamic-let-var-1* :dlet1))
  (check (equal? *dynamic-let-var-2* :dlet2))

  (let ((return-value (dynamic-let ((*dynamic-let-var-1* 12)
				    (*dynamic-let-var-2* 24))
			(check (equal? *dynamic-let-var-1* 12))
			(check (equal? *dynamic-let-var-2* 24))
			:foobar)))
    (check (equal? return-value :foobar))
    (check (equal? *dynamic-let-var-1* :dlet1))
    (check (equal? *dynamic-let-var-2* :dlet2)))

  (let ((unique-value (gensym "unique-value")))
    (dynamic-let ((*dynamic-let-var-1* unique-value))
      (check (eq? *dynamic-let-var-1* unique-value))
      (check (eq? (symbol-value '*dynamic-let-var-1*) unique-value))))

  (check (equal? *dynamic-let-var-1* :dlet1))
  
  (catch 'escape-dynamic-let
    (dynamic-let ((*dynamic-let-var-1* 12)
		  (*dynamic-let-var-2* 24))
      (check (equal? *dynamic-let-var-1* 12))
      (check (equal? *dynamic-let-var-2* 24))
      (throw 'escape-dynamic-let)))

  (check (equal? *dynamic-let-var-1* :dlet1))
  (check (equal? *dynamic-let-var-2* :dlet2))

  (dynamic-let ((*dynamic-let-var-1* *dynamic-let-var-1*)
		(*dynamic-let-var-2* *dynamic-let-var-2*))
    (check (equal? *dynamic-let-var-1* :dlet1))
    (check (equal? *dynamic-let-var-2* :dlet2))
    (set! *dynamic-let-var-1* 12)
    (set! *dynamic-let-var-2* 24))
  (check (equal? *dynamic-let-var-1* :dlet1))
  (check (equal? *dynamic-let-var-2* :dlet2)))
