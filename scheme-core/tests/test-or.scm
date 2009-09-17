(use-package! "unit-test")

(define-test or
  (test-case (boolean? (or #f #f #f)))
  (test-case (boolean? (or #t #t #t)))
  (test-case (not (boolean? (or #f 4))))

  (test-case (eq? (or) #f))
  (test-case (eq? (or #f) #f))
  (test-case (eq? (or #t) #t))
  (test-case (eq? (or #f #f) #f))
  (test-case (eq? (or #f #t) #t))
  (test-case (eq? (or #t #f) #t))
  (test-case (eq? (or #t #t) #t))
  (test-case (eq? (or #f #f #f) #f))
  (test-case (eq? (or #f #f #t) #t))
  (test-case (eq? (or #f #t #f) #t))
  (test-case (eq? (or #f #t #t) #t))
  (test-case (eq? (or #t #f #f) #t))
  (test-case (eq? (or #t #f #t) #t))
  (test-case (eq? (or #t #t #f) #t))
  (test-case (eq? (or #t #t #t) #t))

  ; short circuit evaluation
  (test-case/execution-order (:second-leg)
    (or #f
        (checkpoint :second-leg #t)))

  (test-case/execution-order ()
    (or #t
        (checkpoint :second-leg #t)))

  (test-case/execution-order 3
    (or (checkpoint 1 #f)
        (checkpoint 2 #f)
        (checkpoint 3 #f)))

  (test-case/execution-order 1
    (or (checkpoint 1 #t)
        (checkpoint 2 #f)
        (checkpoint 3 #f)))

  (test-case/execution-order 1
    (or (checkpoint 1 #t)
        (checkpoint 2 #t)
        (checkpoint 3 #t)))

  (test-case (eq? (or :foo) :foo))
  (test-case (eq? (or #f :foo) :foo))
  (test-case (eq? (or #f #f :foo) :foo))
  )

(define-test or*
  (test-case (boolean? (or* #f #f #f)))
  (test-case (boolean? (or* #t #t #t)))

  (test-case (eq? (or*) #f))
  (test-case (eq? (or* #f) #f))
  (test-case (eq? (or* #t) #t))
  (test-case (eq? (or* #f #f) #f))
  (test-case (eq? (or* #f #t) #t))
  (test-case (eq? (or* #t #f) #t))
  (test-case (eq? (or* #t #t) #t))
  (test-case (eq? (or* #f #f #f) #f))
  (test-case (eq? (or* #f #f #t) #t))
  (test-case (eq? (or* #f #t #f) #t))
  (test-case (eq? (or* #f #t #t) #t))
  (test-case (eq? (or* #t #f #f) #t))
  (test-case (eq? (or* #t #f #t) #t))
  (test-case (eq? (or* #t #t #f) #t))
  (test-case (eq? (or* #t #t #t) #t))

  ; shor*t circuit evaluation
  (test-case/execution-order (:second-leg)
    (or* #f
         (checkpoint :second-leg #t)))

  (test-case/execution-order (:second-leg)
    (or* #t
         (checkpoint :second-leg #t)))

  (test-case/execution-order 3
    (or* (checkpoint 1 #f)
         (checkpoint 2 #f)
         (checkpoint 3 #f)))

  (test-case/execution-order 3
    (or* (checkpoint 1 #t)
         (checkpoint 2 #f)
         (checkpoint 3 #f)))

  (test-case/execution-order 3
    (or* (checkpoint 1 #t)
         (checkpoint 2 #t)
         (checkpoint 3 #t)))

  (test-case (eq? (or* :foo) #t))
  (test-case (eq? (or* #f :foo) #t))
  (test-case (eq? (or* #f #f :foo) #t))
  )

