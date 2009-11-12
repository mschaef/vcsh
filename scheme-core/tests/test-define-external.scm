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
  (test-case (equal? (documentation edt-simple) #f))
  (test-case (equal? (documentation edt-docs) "edt-docs documentation"))
  
  (test-case (equal? (documentation edt-simple-1-arg) #f))
  (test-case (equal? (documentation edt-docs-1-arg) "edt-docs-1-arg documentation"))
  
  (test-case (equal? (documentation edt-simple-3-args) #f))
  (test-case (equal? (documentation edt-docs-3-args) "edt-docs-3-args documentation"))

  (test-case (equal? (procedure-name edt-simple) 'edt-simple))
  (test-case (equal? (procedure-name edt-docs) 'edt-docs))

  (test-case (equal? (procedure-name edt-simple-1-arg) 'edt-simple-1-arg))
  (test-case (equal? (procedure-name edt-docs-1-arg) 'edt-docs-1-arg))

  (test-case (equal? (procedure-name edt-simple-3-args) 'edt-simple-3-args))
  (test-case (equal? (procedure-name edt-docs-3-args) 'edt-docs-3-args))
  
  (test-case (equal? (dtu-lambda-list edt-simple) ()))
  (test-case (equal? (dtu-lambda-list edt-docs) ()))
  
  (test-case (equal? (dtu-lambda-list edt-simple-1-arg) '(x)))
  (test-case (equal? (dtu-lambda-list edt-docs-1-arg) '(x)))
  
  (test-case (equal? (dtu-lambda-list edt-simple-3-args) '(x y z)))
  (test-case (equal? (dtu-lambda-list edt-docs-3-args) '(x y z)))

  (test-case/execution-order (:begin :edt-simple :end)
                             (checkpoint :begin)
                             (edt-simple)
                             (checkpoint :end))

  (test-case/execution-order (:begin :edt-docs :end)
                             (checkpoint :begin)
                             (edt-docs)
                             (checkpoint :end))

  (test-case/execution-order (:begin :edt-simple-1-arg :end)
                             (checkpoint :begin)
                             (edt-simple-1-arg 1)
                             (checkpoint :end))

  (test-case/execution-order (:begin :edt-docs-1-arg :end)
                             (checkpoint :begin)
                             (edt-docs-1-arg 1)
                             (checkpoint :end))

  (test-case/execution-order (:begin :edt-simple-3-args :end)
                             (checkpoint :begin)
                             (edt-simple-3-args 1 2 3)
                             (checkpoint :end))

  (test-case/execution-order (:begin :edt-docs-3-args :end)
                             (checkpoint :begin)
                             (edt-docs-3-args 1 2 3)
                             (checkpoint :end)))
