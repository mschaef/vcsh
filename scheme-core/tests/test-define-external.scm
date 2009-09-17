(use-package! "unit-test")

(define (edt-simple)
  (checkpoint :edt-simple))
(define (edt-docs)
  "edt-docs documentation"
  (checkpoint :edt-docs))
(define (edt-decls)
  (declare (edt-decls 0))
  (checkpoint :edt-decls))
(define (edt-docs/decls)
  "edt-docs/decls documentation"
  (declare (edt-decls 0))
  (checkpoint :edt-docs/decls))


(define (edt-simple-1-arg x)
  (checkpoint :edt-simple-1-arg))
(define (edt-docs-1-arg x)
  "edt-docs-1-arg documentation"
  (checkpoint :edt-docs-1-arg))
(define (edt-decls-1-arg x)
  (declare (edt-decls 0))
  (checkpoint :edt-decls-1-arg))
(define (edt-docs/decls-1-arg x)
  "edt-docs/decls-1-arg documentation"
  (declare (edt-decls 0))
  (checkpoint :edt-docs/decls-1-arg))

(define (edt-simple-3-args x y z)
  (checkpoint :edt-simple-3-args))
(define (edt-docs-3-args x y z)
  "edt-docs-3-args documentation"
  (checkpoint :edt-docs-3-args))
(define (edt-decls-3-args x y z)
  (declare (edt-decls 0))
  (checkpoint :edt-decls-3-args))
(define (edt-docs/decls-3-args x y z)
  "edt-docs/decls-3-args documentation"
  (declare (edt-decls 0))
  (checkpoint :edt-docs/decls-3-args))


(define-test define-external
  (test-case (equal? (documentation edt-simple) #f))
  (test-case (equal? (documentation edt-docs) "edt-docs documentation"))
  (test-case (equal? (documentation edt-decls) #f))
  (test-case (equal? (documentation edt-docs/decls) "edt-docs/decls documentation"))
  
  (test-case (equal? (documentation edt-simple-1-arg) #f))
  (test-case (equal? (documentation edt-docs-1-arg) "edt-docs-1-arg documentation"))
  (test-case (equal? (documentation edt-decls-1-arg) #f))
  (test-case (equal? (documentation edt-docs/decls-1-arg) "edt-docs/decls-1-arg documentation"))
  
  (test-case (equal? (documentation edt-simple-3-args) #f))
  (test-case (equal? (documentation edt-docs-3-args) "edt-docs-3-args documentation"))
  (test-case (equal? (documentation edt-decls-3-args) #f))
  (test-case (equal? (documentation edt-docs/decls-3-args) "edt-docs/decls-3-args documentation"))

  (test-case (equal? (procedure-name edt-simple) 'edt-simple))
  (test-case (equal? (procedure-name edt-docs) 'edt-docs))
  (test-case (equal? (procedure-name edt-decls) 'edt-decls))
  (test-case (equal? (procedure-name edt-docs/decls) 'edt-docs/decls))

  (test-case (equal? (procedure-name edt-simple-1-arg) 'edt-simple-1-arg))
  (test-case (equal? (procedure-name edt-docs-1-arg) 'edt-docs-1-arg))
  (test-case (equal? (procedure-name edt-decls-1-arg) 'edt-decls-1-arg))
  (test-case (equal? (procedure-name edt-docs/decls-1-arg) 'edt-docs/decls-1-arg))

  (test-case (equal? (procedure-name edt-simple-3-args) 'edt-simple-3-args))
  (test-case (equal? (procedure-name edt-docs-3-args) 'edt-docs-3-args))
  (test-case (equal? (procedure-name edt-decls-3-args) 'edt-decls-3-args))
  (test-case (equal? (procedure-name edt-docs/decls-3-args) 'edt-docs/decls-3-args))
  
  (test-case (equal? (dtu-lambda-list edt-simple) ()))
  (test-case (equal? (dtu-lambda-list edt-docs) ()))
  (test-case (equal? (dtu-lambda-list edt-decls) ()))
  (test-case (equal? (dtu-lambda-list edt-docs/decls) ()))
  
  (test-case (equal? (dtu-lambda-list edt-simple-1-arg) '(x)))
  (test-case (equal? (dtu-lambda-list edt-docs-1-arg) '(x)))
  (test-case (equal? (dtu-lambda-list edt-decls-1-arg) '(x)))
  (test-case (equal? (dtu-lambda-list edt-docs/decls-1-arg) '(x)))
  
  (test-case (equal? (dtu-lambda-list edt-simple-3-args) '(x y z)))
  (test-case (equal? (dtu-lambda-list edt-docs-3-args) '(x y z)))
  (test-case (equal? (dtu-lambda-list edt-decls-3-args) '(x y z)))
  (test-case (equal? (dtu-lambda-list edt-docs/decls-3-args) '(x y z)))

  (test-case (equal? (scheme::procedure-decl? edt-simple 'edt-decls) #f))
  (test-case (equal? (scheme::procedure-decl? edt-docs 'edt-decls) #f))
  (test-case (equal? (scheme::procedure-decl? edt-decls 'edt-decls) '(0)))
  (test-case (equal? (scheme::procedure-decl? edt-docs/decls 'edt-decls) '(0)))
  
  (test-case (equal? (scheme::procedure-decl? edt-simple-1-arg 'edt-decls) #f))
  (test-case (equal? (scheme::procedure-decl? edt-docs-1-arg 'edt-decls) #f))
  (test-case (equal? (scheme::procedure-decl? edt-decls-1-arg 'edt-decls) '(0)))
  (test-case (equal? (scheme::procedure-decl? edt-docs/decls-1-arg 'edt-decls) '(0)))
  
  (test-case (equal? (scheme::procedure-decl? edt-simple-3-args 'edt-decls) #f))
  (test-case (equal? (scheme::procedure-decl? edt-docs-3-args 'edt-decls)  #f) )
  (test-case (equal? (scheme::procedure-decl? edt-decls-3-args 'edt-decls) '(0)))
  (test-case (equal? (scheme::procedure-decl? edt-docs/decls-3-args 'edt-decls) '(0)))

  (test-case/execution-order (:begin :edt-simple :end)
    (checkpoint :begin)
    (edt-simple)
    (checkpoint :end))

  (test-case/execution-order (:begin :edt-docs :end)
    (checkpoint :begin)
    (edt-docs)
    (checkpoint :end))

  (test-case/execution-order (:begin :edt-decls :end)
    (checkpoint :begin)
    (edt-decls)
    (checkpoint :end))

  (test-case/execution-order (:begin :edt-docs/decls :end)
    (checkpoint :begin)
    (edt-docs/decls)
    (checkpoint :end))

  (test-case/execution-order (:begin :edt-simple-1-arg :end)
    (checkpoint :begin)
    (edt-simple-1-arg 1)
    (checkpoint :end))

  (test-case/execution-order (:begin :edt-docs-1-arg :end)
    (checkpoint :begin)
    (edt-docs-1-arg 1)
    (checkpoint :end))

  (test-case/execution-order (:begin :edt-decls-1-arg :end)
    (checkpoint :begin)
    (edt-decls-1-arg 1)
    (checkpoint :end))

  (test-case/execution-order (:begin :edt-docs/decls-1-arg :end)
    (checkpoint :begin)
    (edt-docs/decls-1-arg 1)
    (checkpoint :end))


  (test-case/execution-order (:begin :edt-simple-3-args :end)
    (checkpoint :begin)
    (edt-simple-3-args 1 2 3)
    (checkpoint :end))

  (test-case/execution-order (:begin :edt-docs-3-args :end)
    (checkpoint :begin)
    (edt-docs-3-args 1 2 3)
    (checkpoint :end))

  (test-case/execution-order (:begin :edt-decls-3-args :end)
    (checkpoint :begin)
    (edt-decls-3-args 1 2 3 )
    (checkpoint :end))

  (test-case/execution-order (:begin :edt-docs/decls-3-args :end)
    (checkpoint :begin)
    (edt-docs/decls-3-args 1 2 3)
    (checkpoint :end))
  )

