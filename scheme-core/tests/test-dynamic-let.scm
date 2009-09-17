(use-package! "unit-test")

(define *dynamic-let-var-1* :dlet1)
(define *dynamic-let-var-2* :dlet2)

(define-test dynamic-let
  (test-case (equal? *dynamic-let-var-1* :dlet1))
  (test-case (equal? *dynamic-let-var-2* :dlet2))

  (let ((return-value (dynamic-let ((*dynamic-let-var-1* 12)
				    (*dynamic-let-var-2* 24))
			(test-case (equal? *dynamic-let-var-1* 12))
			(test-case (equal? *dynamic-let-var-2* 24))
			:foobar)))
    (test-case (equal? return-value :foobar))
    (test-case (equal? *dynamic-let-var-1* :dlet1))
    (test-case (equal? *dynamic-let-var-2* :dlet2)))

  (let ((unique-value (gensym "unique-value")))
    (dynamic-let ((*dynamic-let-var-1* unique-value))
      (test-case (eq? *dynamic-let-var-1* unique-value))
      (test-case (eq? (symbol-value '*dynamic-let-var-1*) unique-value))))

  (test-case (equal? *dynamic-let-var-1* :dlet1))
  
  (catch 'escape-dynamic-let
    (dynamic-let ((*dynamic-let-var-1* 12)
		  (*dynamic-let-var-2* 24))
      (test-case (equal? *dynamic-let-var-1* 12))
      (test-case (equal? *dynamic-let-var-2* 24))
      (throw 'escape-dynamic-let)))

  (test-case (equal? *dynamic-let-var-1* :dlet1))
  (test-case (equal? *dynamic-let-var-2* :dlet2))

  (dynamic-let ((*dynamic-let-var-1* *dynamic-let-var-1*)
		(*dynamic-let-var-2* *dynamic-let-var-2*))
    (test-case (equal? *dynamic-let-var-1* :dlet1))
    (test-case (equal? *dynamic-let-var-2* :dlet2))
    (set! *dynamic-let-var-1* 12)
    (set! *dynamic-let-var-2* 24))
  (test-case (equal? *dynamic-let-var-1* :dlet1))
  (test-case (equal? *dynamic-let-var-2* :dlet2)))
