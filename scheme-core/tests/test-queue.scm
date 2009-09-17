(use-package! "unit-test")

(define-test queue
  (test-case (runtime-error? (q-items :not-a-queue)))
  (test-case (runtime-error? (q-enqueue! x :not-a-queue)))
  (test-case (runtime-error? (q-dequeue! :not-a-queue)))
  (test-case (runtime-error? (q-empty? :not-a-queue)))

  (test-case (runtime-error? (q-dequeue! (make-queue))))

  (test-case (not (queue? :not-a-queue)))
  (test-case (queue? (make-queue)))

  (let* ((q (make-queue))
         (q0 q))

    (test-case (equal? () (q-items q)))
    (test-case (q-empty? q))

    (q-enqueue! 1 q)

    (test-case (eq? q q0))    
    (test-case (queue? q))
    (test-case (equal? '(1) (q-items q)))
    (test-case (eq? (q-items q) (q-items q)))

    (test-case (eq? 1 (q-dequeue! q)))
    (test-case (queue? q))
    (test-case (equal? () (q-items q)))
    (test-case (q-empty? q))
    
    (q-enqueue! 1 q)
    (q-enqueue! 2 q)

    (test-case (eq? q q0))    
    (test-case (queue? q))
    (test-case (equal? '(1 2) (q-items q)))
    (test-case (eq? (q-items q) (q-items q)))

    (test-case (eq? 1 (q-dequeue! q)))
    (test-case (eq? 2 (q-dequeue! q)))

    (test-case (queue? q))
    (test-case (equal? () (q-items q)))    
    (test-case (q-empty? q))))