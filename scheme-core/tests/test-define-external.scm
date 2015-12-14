(use-package! "unit-test")

(define (edt-simple)
  (checkpoint :edt-simple))
(define (edt-docs)
  "edt-docs documentation"
  (checkpoint :edt-docs))

(define (edt-simple-1-arg x)
  (checkpoint :edt-simple-1-arg))
(define (edt-docs-1-arg x)
  "edt-docs-1-arg documentation"
  (checkpoint :edt-docs-1-arg))

(define (edt-simple-3-args x y z)
  (checkpoint :edt-simple-3-args))
(define (edt-docs-3-args x y z)
  "edt-docs-3-args documentation"
  (checkpoint :edt-docs-3-args))

(define-test define-external
  (check (equal? (documentation edt-simple) #f))
  (check (equal? (documentation edt-docs) "edt-docs documentation"))
  
  (check (equal? (documentation edt-simple-1-arg) #f))
  (check (equal? (documentation edt-docs-1-arg) "edt-docs-1-arg documentation"))
  
  (check (equal? (documentation edt-simple-3-args) #f))
  (check (equal? (documentation edt-docs-3-args) "edt-docs-3-args documentation"))

  (check (equal? (procedure-name edt-simple) 'edt-simple))
  (check (equal? (procedure-name edt-docs) 'edt-docs))

  (check (equal? (procedure-name edt-simple-1-arg) 'edt-simple-1-arg))
  (check (equal? (procedure-name edt-docs-1-arg) 'edt-docs-1-arg))

  (check (equal? (procedure-name edt-simple-3-args) 'edt-simple-3-args))
  (check (equal? (procedure-name edt-docs-3-args) 'edt-docs-3-args))
  
  (check (equal? (dtu-lambda-list edt-simple) ()))
  (check (equal? (dtu-lambda-list edt-docs) ()))
  
  (check (equal? (dtu-lambda-list edt-simple-1-arg) '(x)))
  (check (equal? (dtu-lambda-list edt-docs-1-arg) '(x)))
  
  (check (equal? (dtu-lambda-list edt-simple-3-args) '(x y z)))
  (check (equal? (dtu-lambda-list edt-docs-3-args) '(x y z)))

  (check
   (equal? '(:begin :edt-simple :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (edt-simple)
            (checkpoint :end))))

  (check
   (equal? '(:begin :edt-docs :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (edt-docs)
            (checkpoint :end))))

  (check
   (equal? '(:begin :edt-simple-1-arg :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (edt-simple-1-arg 1)
            (checkpoint :end))))
  
  (check
   (equal? '(:begin :edt-docs-1-arg :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (edt-docs-1-arg 1)
            (checkpoint :end))))

  (check
   (equal? '(:begin :edt-simple-3-args :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (edt-simple-3-args 1 2 3)
            (checkpoint :end))))

  (check
   (equal? '(:begin :edt-docs-3-args :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (edt-docs-3-args 1 2 3)
            (checkpoint :end)))))
