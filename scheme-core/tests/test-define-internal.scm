(use-package! "unit-test")

(define-test define-internal
  (define (idt-simple)
    (checkpoint :idt-simple))
  (define (idt-docs)
    "idt-docs documentation"
    (checkpoint :idt-docs))

  (define (idt-simple-1-arg x)
    (checkpoint :idt-simple-1-arg))
  (define (idt-docs-1-arg x)
    "idt-docs-1-arg documentation"
    (checkpoint :idt-docs-1-arg))

  (define (idt-simple-3-args x y z)
    (checkpoint :idt-simple-3-args))
  (define (idt-docs-3-args x y z)
    "idt-docs-3-args documentation"
    (checkpoint :idt-docs-3-args))

  (check (equal? (documentation idt-simple) #f))
  (check (equal? (documentation idt-docs) "idt-docs documentation"))
  
  (check (equal? (documentation idt-simple-1-arg) #f))
  (check (equal? (documentation idt-docs-1-arg) "idt-docs-1-arg documentation"))
  
  (check (equal? (documentation idt-simple-3-args) #f))
  (check (equal? (documentation idt-docs-3-args) "idt-docs-3-args documentation"))

  (check (equal? (procedure-name idt-simple) 'idt-simple))
  (check (equal? (procedure-name idt-docs) 'idt-docs))

  (check (equal? (procedure-name idt-simple-1-arg) 'idt-simple-1-arg))
  (check (equal? (procedure-name idt-docs-1-arg) 'idt-docs-1-arg))

  (check (equal? (procedure-name idt-simple-3-args) 'idt-simple-3-args))
  (check (equal? (procedure-name idt-docs-3-args) 'idt-docs-3-args))
  
  (check (equal? (dtu-lambda-list idt-simple) ()))
  (check (equal? (dtu-lambda-list idt-docs) ()))
  
  (check (equal? (dtu-lambda-list idt-simple-1-arg) '(x)))
  (check (equal? (dtu-lambda-list idt-docs-1-arg) '(x)))
  
  (check (equal? (dtu-lambda-list idt-simple-3-args) '(x y z)))
  (check (equal? (dtu-lambda-list idt-docs-3-args) '(x y z)))

  (check
   (equal? '(:begin :idt-simple :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-simple)
            (checkpoint :end))))

  (check
   (equal? '(:begin :idt-docs :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-docs)
            (checkpoint :end))))

  (check
   (equal? '(:begin :idt-simple-1-arg :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-simple-1-arg 1)
            (checkpoint :end))))

  (check
   (equal? '(:begin :idt-docs-1-arg :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-docs-1-arg 1)
            (checkpoint :end))))

  (check
   (equal? '(:begin :idt-simple-3-args :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-simple-3-args 1 2 3)
            (checkpoint :end))))

  (check
   (equal? '(:begin :idt-docs-3-args :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-docs-3-args 1 2 3)
            (checkpoint :end)))))
