(use-package! "unit-test")

(define-test queue
  (check (runtime-error? (q-items :not-a-queue)))
  (check (runtime-error? (q-enqueue! 'q :not-a-queue)))
  (check (runtime-error? (q-dequeue! :not-a-queue)))
  (check (runtime-error? (q-empty? :not-a-queue)))

  (check (runtime-error? (q-dequeue! (make-queue))))

  (check (not (queue? :not-a-queue)))
  (check (queue? (make-queue)))

  (let* ((q (make-queue))
         (q0 q))

    (check (equal? () (q-items q)))
    (check (q-empty? q))

    (q-enqueue! 1 q)

    (check (eq? q q0))    
    (check (queue? q))
    (check (equal? '(1) (q-items q)))
    (check (eq? (q-items q) (q-items q)))

    (check (eq? 1 (q-dequeue! q)))
    (check (queue? q))
    (check (equal? () (q-items q)))
    (check (q-empty? q))
    
    (q-enqueue! 1 q)
    (q-enqueue! 2 q)

    (check (eq? q q0))    
    (check (queue? q))
    (check (equal? '(1 2) (q-items q)))
    (check (eq? (q-items q) (q-items q)))

    (check (eq? 1 (q-dequeue! q)))
    (check (eq? 2 (q-dequeue! q)))

    (check (queue? q))
    (check (equal? () (q-items q)))    
    (check (q-empty? q))))
