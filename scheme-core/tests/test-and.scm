(use-package! "unit-test")

(define-test and
  (test-case (boolean? (and #t #f #t)))
  (test-case (boolean? (and #t #t #t)))
  (test-case (eq? (and) #t))
  (test-case (eq? (and #f) #f))
  (test-case (eq? (and #t) #t))
  (test-case (eq? (and #f #f) #f))
  (test-case (eq? (and #f #t) #f))
  (test-case (eq? (and #t #f) #f))
  (test-case (eq? (and #t #t) #t))
  (test-case (eq? (and #f #f #f) #f))
  (test-case (eq? (and #f #f #t) #f))
  (test-case (eq? (and #f #t #f) #f))
  (test-case (eq? (and #f #t #t) #f))
  (test-case (eq? (and #t #f #f) #f))
  (test-case (eq? (and #t #f #t) #f))
  (test-case (eq? (and #t #t #f) #f))
  (test-case (eq? (and #t #t #t) #t))

  ; Short circuit evaluation
  (test-case/execution-order (:second-leg)
    (and #t
         (checkpoint :second-leg #t)))

  (test-case/execution-order ()
    (and #f
         (checkpoint :second-leg #t)))

  (test-case/execution-order 3
    (and (checkpoint 1 #t)
         (checkpoint 2 #t)
         (checkpoint 3 #t)))

  (test-case/execution-order 1
    (and (checkpoint 1 #f)
         (checkpoint 2 #f)
         (checkpoint 3 #f)))

  (test-case (eq? (and :foo) :foo))
  (test-case (eq? (and :foo :bar :baz) :baz))
  )

(define-test and*
  (test-case (boolean? (and* #t #f #t)))
  (test-case (boolean? (and* #t #t #t)))
  (test-case (eq? (and*) #t))
  (test-case (eq? (and* #f) #f))
  (test-case (eq? (and* #t) #t))
  (test-case (eq? (and* #f #f) #f))
  (test-case (eq? (and* #f #t) #f))
  (test-case (eq? (and* #t #f) #f))
  (test-case (eq? (and* #t #t) #t))
  (test-case (eq? (and* #f #f #f) #f))
  (test-case (eq? (and* #f #f #t) #f))
  (test-case (eq? (and* #f #t #f) #f))
  (test-case (eq? (and* #f #t #t) #f))
  (test-case (eq? (and* #t #f #f) #f))
  (test-case (eq? (and* #t #f #t) #f))
  (test-case (eq? (and* #t #t #f) #f))
  (test-case (eq? (and* #t #t #t) #t))

  ; Non-Short circuit evaluation
  (test-case/execution-order (:second-leg)
    (and* #t
          (checkpoint :second-leg #t)))

  (test-case/execution-order (:second-leg)
    (and* #f
          (checkpoint :second-leg #t)))

  (test-case/execution-order 3
    (and* (checkpoint 1 #t)
          (checkpoint 2 #t)
          (checkpoint 3 #t)))

  (test-case/execution-order 3
    (and* (checkpoint 1 #f)
          (checkpoint 2 #f)
          (checkpoint 3 #f)))

  (test-case (eq? (and* :foo) #t))
  (test-case (eq? (and* :foo :bar :baz) #t))
  )
