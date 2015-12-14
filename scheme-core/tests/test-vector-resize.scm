(use-package! "unit-test")

(define-test vector-resize
  (check (runtime-error? (vector-resize 'not-a-vector 12 #f)))
  (check (runtime-error? (vector-resize [1 2] 'not-a-number #f)))
  (check (runtime-error? (vector-resize [1 2] -1 #f)))

  (let* ((a (vector))
         (b (vector-resize a 0 #f)))
    (check (not (eq? a b)))
    (check (= 0 (length b)))
    (check (equal? [] b)))

  (let* ((a (vector))
         (b (vector-resize a 5 #f)))
    (check (not (eq? a b)))
    (check (= 5 (length b)))
    (check (equal? [#f #f #f #f #f] b)))

  (let* ((a (vector #f #f #f #f #f))
         (b (vector-resize a 0 #f)))
    (check (not (eq? a b)))
    (check (= 0 (length b)))
    (check (equal? [] b)))

  (let* ((a (vector #f #f #f #f #f))
         (b (vector-resize a 3 #f)))
    (check (not (eq? a b)))
    (check (= 3 (length b)))
    (check (equal? [#f #f #f] b)))

  (let* ((a (vector #f #f #f #f #f))
         (b (vector-resize a 7 :new-element)))
    (check (not (eq? a b)))
    (check (= 7 (length b)))
    (check (equal? [#f #f #f #f #f :new-element :new-element] b))))




