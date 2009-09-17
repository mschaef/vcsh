(use-package! "unit-test")

(define-test define-internal
  (define (idt-simple)
    (checkpoint :idt-simple))
  (define (idt-docs)
    "idt-docs documentation"
    (checkpoint :idt-docs))
  (define (idt-decls)
    (declare (idt-decls 0))
    (checkpoint :idt-decls))
  (define (idt-docs/decls)
    "idt-docs/decls documentation"
    (declare (idt-decls 0))
    (checkpoint :idt-docs/decls))


  (define (idt-simple-1-arg x)
    (checkpoint :idt-simple-1-arg))
  (define (idt-docs-1-arg x)
    "idt-docs-1-arg documentation"
    (checkpoint :idt-docs-1-arg))
  (define (idt-decls-1-arg x)
    (declare (idt-decls 0))
    (checkpoint :idt-decls-1-arg))
  (define (idt-docs/decls-1-arg x)
    "idt-docs/decls-1-arg documentation"
    (declare (idt-decls 0))
    (checkpoint :idt-docs/decls-1-arg))

  (define (idt-simple-3-args x y z)
    (checkpoint :idt-simple-3-args))
  (define (idt-docs-3-args x y z)
    "idt-docs-3-args documentation"
    (checkpoint :idt-docs-3-args))
  (define (idt-decls-3-args x y z)
    (declare (idt-decls 0))
    (checkpoint :idt-decls-3-args))
  (define (idt-docs/decls-3-args x y z)
    "idt-docs/decls-3-args documentation"
    (declare (idt-decls 0))
    (checkpoint :idt-docs/decls-3-args))

  (test-case (equal? (documentation idt-simple) #f))
  (test-case (equal? (documentation idt-docs) "idt-docs documentation"))
  (test-case (equal? (documentation idt-decls) #f))
  (test-case (equal? (documentation idt-docs/decls) "idt-docs/decls documentation"))
  
  (test-case (equal? (documentation idt-simple-1-arg) #f))
  (test-case (equal? (documentation idt-docs-1-arg) "idt-docs-1-arg documentation"))
  (test-case (equal? (documentation idt-decls-1-arg) #f))
  (test-case (equal? (documentation idt-docs/decls-1-arg) "idt-docs/decls-1-arg documentation"))
  
  (test-case (equal? (documentation idt-simple-3-args) #f))
  (test-case (equal? (documentation idt-docs-3-args) "idt-docs-3-args documentation"))
  (test-case (equal? (documentation idt-decls-3-args) #f))
  (test-case (equal? (documentation idt-docs/decls-3-args) "idt-docs/decls-3-args documentation"))

  (test-case (equal? (procedure-name idt-simple) 'idt-simple))
  (test-case (equal? (procedure-name idt-docs) 'idt-docs))
  (test-case (equal? (procedure-name idt-decls) 'idt-decls))
  (test-case (equal? (procedure-name idt-docs/decls) 'idt-docs/decls))

  (test-case (equal? (procedure-name idt-simple-1-arg) 'idt-simple-1-arg))
  (test-case (equal? (procedure-name idt-docs-1-arg) 'idt-docs-1-arg))
  (test-case (equal? (procedure-name idt-decls-1-arg) 'idt-decls-1-arg))
  (test-case (equal? (procedure-name idt-docs/decls-1-arg) 'idt-docs/decls-1-arg))

  (test-case (equal? (procedure-name idt-simple-3-args) 'idt-simple-3-args))
  (test-case (equal? (procedure-name idt-docs-3-args) 'idt-docs-3-args))
  (test-case (equal? (procedure-name idt-decls-3-args) 'idt-decls-3-args))
  (test-case (equal? (procedure-name idt-docs/decls-3-args) 'idt-docs/decls-3-args))
  
  (test-case (equal? (dtu-lambda-list idt-simple) ()))
  (test-case (equal? (dtu-lambda-list idt-docs) ()))
  (test-case (equal? (dtu-lambda-list idt-decls) ()))
  (test-case (equal? (dtu-lambda-list idt-docs/decls) ()))
  
  (test-case (equal? (dtu-lambda-list idt-simple-1-arg) '(x)))
  (test-case (equal? (dtu-lambda-list idt-docs-1-arg) '(x)))
  (test-case (equal? (dtu-lambda-list idt-decls-1-arg) '(x)))
  (test-case (equal? (dtu-lambda-list idt-docs/decls-1-arg) '(x)))
  
  (test-case (equal? (dtu-lambda-list idt-simple-3-args) '(x y z)))
  (test-case (equal? (dtu-lambda-list idt-docs-3-args) '(x y z)))
  (test-case (equal? (dtu-lambda-list idt-decls-3-args) '(x y z)))
  (test-case (equal? (dtu-lambda-list idt-docs/decls-3-args) '(x y z)))

  (test-case (equal? (scheme::procedure-decl? idt-simple 'idt-decls) #f))
  (test-case (equal? (scheme::procedure-decl? idt-docs 'idt-decls) #f))
  (test-case (equal? (scheme::procedure-decl? idt-decls 'idt-decls) '(0)))
  (test-case (equal? (scheme::procedure-decl? idt-docs/decls 'idt-decls) '(0)))
  
  (test-case (equal? (scheme::procedure-decl? idt-simple-1-arg 'idt-decls) #f))
  (test-case (equal? (scheme::procedure-decl? idt-docs-1-arg 'idt-decls) #f))
  (test-case (equal? (scheme::procedure-decl? idt-decls-1-arg 'idt-decls) '(0)))
  (test-case (equal? (scheme::procedure-decl? idt-docs/decls-1-arg 'idt-decls) '(0)))
  
  (test-case (equal? (scheme::procedure-decl? idt-simple-3-args 'idt-decls) #f))
  (test-case (equal? (scheme::procedure-decl? idt-docs-3-args 'idt-decls)  #f) )
  (test-case (equal? (scheme::procedure-decl? idt-decls-3-args 'idt-decls) '(0)))
  (test-case (equal? (scheme::procedure-decl? idt-docs/decls-3-args 'idt-decls) '(0)))

  (test-case/execution-order (:begin :idt-simple :end)
    (checkpoint :begin)
    (idt-simple)
    (checkpoint :end))

  (test-case/execution-order (:begin :idt-docs :end)
    (checkpoint :begin)
    (idt-docs)
    (checkpoint :end))

  (test-case/execution-order (:begin :idt-decls :end)
    (checkpoint :begin)
    (idt-decls)
    (checkpoint :end))

  (test-case/execution-order (:begin :idt-docs/decls :end)
    (checkpoint :begin)
    (idt-docs/decls)
    (checkpoint :end))

  (test-case/execution-order (:begin :idt-simple-1-arg :end)
    (checkpoint :begin)
    (idt-simple-1-arg 1)
    (checkpoint :end))

  (test-case/execution-order (:begin :idt-docs-1-arg :end)
    (checkpoint :begin)
    (idt-docs-1-arg 1)
    (checkpoint :end))

  (test-case/execution-order (:begin :idt-decls-1-arg :end)
    (checkpoint :begin)
    (idt-decls-1-arg 1)
    (checkpoint :end))

  (test-case/execution-order (:begin :idt-docs/decls-1-arg :end)
    (checkpoint :begin)
    (idt-docs/decls-1-arg 1)
    (checkpoint :end))


  (test-case/execution-order (:begin :idt-simple-3-args :end)
    (checkpoint :begin)
    (idt-simple-3-args 1 2 3)
    (checkpoint :end))

  (test-case/execution-order (:begin :idt-docs-3-args :end)
    (checkpoint :begin)
    (idt-docs-3-args 1 2 3)
    (checkpoint :end))

  (test-case/execution-order (:begin :idt-decls-3-args :end)
    (checkpoint :begin)
    (idt-decls-3-args 1 2 3 )
    (checkpoint :end))

  (test-case/execution-order (:begin :idt-docs/decls-3-args :end)
    (checkpoint :begin)
    (idt-docs/decls-3-args 1 2 3)
    (checkpoint :end))

  )
