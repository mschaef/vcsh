(use-package! "unit-test")

(define-test vector-resize
  (test-case (runtime-error? (vector-resize 'not-a-vector 12 #f)))
  (test-case (runtime-error? (vector-resize [1 2] 'not-a-number #f)))
  (test-case (runtime-error? (vector-resize [1 2] -1 #f)))

  (let* ((a (vector))
         (b (vector-resize a 0 #f)))
    (test-case (not (eq? a b)))
    (test-case (= 0 (length b)))
    (test-case (equal? [] b)))

  (let* ((a (vector))
         (b (vector-resize a 5 #f)))
    (test-case (not (eq? a b)))
    (test-case (= 5 (length b)))
    (test-case (equal? [#f #f #f #f #f] b)))

  (let* ((a (vector #f #f #f #f #f))
         (b (vector-resize a 0 #f)))
    (test-case (not (eq? a b)))
    (test-case (= 0 (length b)))
    (test-case (equal? [] b)))

  (let* ((a (vector #f #f #f #f #f))
         (b (vector-resize a 3 #f)))
    (test-case (not (eq? a b)))
    (test-case (= 3 (length b)))
    (test-case (equal? [#f #f #f] b)))

  (let* ((a (vector #f #f #f #f #f))
         (b (vector-resize a 7 :new-element)))
    (test-case (not (eq? a b)))
    (test-case (= 7 (length b)))
    (test-case (equal? [#f #f #f #f #f :new-element :new-element] b))))




