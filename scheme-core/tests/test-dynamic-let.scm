(use-package! "unit-test")

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
