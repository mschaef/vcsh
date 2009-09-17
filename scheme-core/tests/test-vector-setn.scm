(use-package! "unit-test")

(define-test vector-set!
  (let ((a (vector :a :b :c :d :e)))
    (test-case (runtime-error? (vector-set! '(1 2 3 4) 2 :foo)))
    (test-case (runtime-error? (vector-set! a 'sym :foo)))
    (test-case (runtime-error? (vector-set! a -1 :foo)))
    (test-case (runtime-error? (vector-set! a 5 :foo)))
    (test-case (runtime-error? (vector-set! a -1.0 :foo)))
    (test-case (runtime-error? (vector-set! a 5.0 :foo)))
    (test-case (equal? #(:a :b :c :d :e) a)) ; None of the sets should've been destructive...
    (test-case (not (runtime-error? (vector-set! a 0 :a-0))))
    (test-case (eq? :a-0 (vector-ref a 0)))
    (test-case (not (runtime-error? (vector-set! a 1 :b-0))))
    (test-case (eq? :b-0 (vector-ref a 1)))
    (test-case (not (runtime-error? (vector-set! a 2 :c-0))))
    (test-case (eq? :c-0 (vector-ref a 2)))
    (test-case (not (runtime-error? (vector-set! a 3 :d-0))))
    (test-case (eq? :d-0 (vector-ref a 3)))
    (test-case (not (runtime-error? (vector-set! a 4 :e-0))))
    (test-case (eq? :e-0 (vector-ref a 4)))
    (test-case (equal? #(:a-0 :b-0 :c-0 :d-0 :e-0) a))
    (test-case (eq? a (vector-set! a 0 0)))
    (test-case (not (runtime-error? (vector-set! a 0.0 :a-1))))
    (test-case (eq? :a-1 (vector-ref a 0)))
    (test-case (not (runtime-error? (vector-set! a 4.0 :e-1))))
    (test-case (eq? :e-1 (vector-ref a 4)))
    ))

