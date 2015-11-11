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

  (test-case (equal? (documentation idt-simple) #f))
  (test-case (equal? (documentation idt-docs) "idt-docs documentation"))
  
  (test-case (equal? (documentation idt-simple-1-arg) #f))
  (test-case (equal? (documentation idt-docs-1-arg) "idt-docs-1-arg documentation"))
  
  (test-case (equal? (documentation idt-simple-3-args) #f))
  (test-case (equal? (documentation idt-docs-3-args) "idt-docs-3-args documentation"))

  (test-case (equal? (procedure-name idt-simple) 'idt-simple))
  (test-case (equal? (procedure-name idt-docs) 'idt-docs))

  (test-case (equal? (procedure-name idt-simple-1-arg) 'idt-simple-1-arg))
  (test-case (equal? (procedure-name idt-docs-1-arg) 'idt-docs-1-arg))

  (test-case (equal? (procedure-name idt-simple-3-args) 'idt-simple-3-args))
  (test-case (equal? (procedure-name idt-docs-3-args) 'idt-docs-3-args))
  
  (test-case (equal? (dtu-lambda-list idt-simple) ()))
  (test-case (equal? (dtu-lambda-list idt-docs) ()))
  
  (test-case (equal? (dtu-lambda-list idt-simple-1-arg) '(x)))
  (test-case (equal? (dtu-lambda-list idt-docs-1-arg) '(x)))
  
  (test-case (equal? (dtu-lambda-list idt-simple-3-args) '(x y z)))
  (test-case (equal? (dtu-lambda-list idt-docs-3-args) '(x y z)))

  (test-case
   (equal? '(:begin :idt-simple :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-simple)
            (checkpoint :end))))

  (test-case
   (equal? '(:begin :idt-docs :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-docs)
            (checkpoint :end))))

  (test-case
   (equal? '(:begin :idt-simple-1-arg :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-simple-1-arg 1)
            (checkpoint :end))))

  (test-case
   (equal? '(:begin :idt-docs-1-arg :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-docs-1-arg 1)
            (checkpoint :end))))

  (test-case
   (equal? '(:begin :idt-simple-3-args :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-simple-3-args 1 2 3)
            (checkpoint :end))))

  (test-case
   (equal? '(:begin :idt-docs-3-args :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (idt-docs-3-args 1 2 3)
            (checkpoint :end)))))
