(define-package "test-class-graph"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test class-graph
  (with-gensyms (tc-1 tc-2 tc-3 tc-4 tc-5 tc-6)
    (check (not (runtime-error? (make-class< tc-1 tc-2))))
    (check (not (runtime-error? (make-class< tc-2 tc-6))))
    (check (not (runtime-error? (make-class< tc-3 tc-6))))
    (check (not (runtime-error? (make-class< tc-5 tc-6))))
    (check (not (runtime-error? (make-class< tc-4 tc-5))))

    (check (runtime-error? (make-class< 1 tc-2)))
    (check (runtime-error? (make-class< tc-2 1)))

    (check (runtime-error? (make-class< tc-6 tc-6)))
    (check (runtime-error? (make-class< tc-6 tc-1)))

    (check (eq? (class-superclass tc-1) tc-2))
    (check (eq? (class-superclass tc-2) tc-6))
    (check (eq? (class-superclass tc-6) #f))

    (check (equal? (class-superclasses tc-1) (list tc-1 tc-2 tc-6)))
    (check (equal? (class-superclasses tc-2) (list tc-2 tc-6)))
    (check (equal? (class-superclasses tc-6) (list tc-6)))

    (check (class<=? tc-1 tc-1))
    (check (class<=? tc-1 tc-6))
    (check (class<=? tc-4 tc-6))

    (check (not (class<=? tc-6 tc-1)))
    (check (not (class<=? tc-4 tc-2)))

    (check (classes<=? (list tc-1) (list tc-1)))
    (check (classes<=? (list tc-1) (list tc-6)))
    (check (classes<=? (list tc-4) (list tc-6)))

    (check (not (classes<=? (list tc-6) (list tc-1))))
    (check (not (classes<=? (list tc-4) (list tc-2))))

    (check (classes<=? (list tc-1 tc-1) (list tc-1 tc-1)))
    (check (classes<=? (list tc-1 tc-1) (list tc-6 tc-6)))
    (check (classes<=? (list tc-4 tc-1) (list tc-6 tc-6)))

    (check (not (classes<=? (list tc-6 tc-6) (list tc-1 tc-1))))
    (check (not (classes<=? (list tc-6 tc-6) (list tc-1 tc-6))))
    (check (not (classes<=? (list tc-4 tc-4) (list tc-2 tc-2))))
    (check (not (classes<=? (list tc-6 tc-6) (list tc-4 tc-6))))

    (check (not (classes<=? (list tc-1) (list tc-1 tc-1)))) 
    (check (not (classes<=? (list tc-1) (list tc-6 tc-6))))
    (check (not (classes<=? (list tc-6) (list tc-6 tc-6))))
    (check (not (classes<=? (list tc-6) (list tc-4 tc-4))))

    (check (classes<=? (list tc-1 tc-2) (list tc-6 tc-6)))
    (check (classes<=? (list tc-2 tc-1) (list tc-6 tc-6)))

    (check (classes<=? (list tc-1 tc-6) (list tc-6 tc-6)))
    (check (classes<=? (list tc-6 tc-1) (list tc-6 tc-6)))

    (check (classes<=? (list tc-1 tc-1 tc-1)  (list tc-1 tc-1 tc-1)))
    (check (classes<=? (list tc-1 tc-1 tc-1)  (list tc-6 tc-1 tc-1)))
    (check (classes<=? (list tc-1 tc-1 tc-1)  (list tc-6 tc-6 tc-1)))
    (check (classes<=? (list tc-1 tc-1 tc-1)  (list tc-6 tc-6 tc-6)))
    (check (classes<=? (list tc-1 tc-1 tc-1)  (list tc-1 tc-1 tc-6)))

    (check (classes<=? (list tc-4 tc-1 tc-1)  (list tc-6 tc-6 tc-6)))
    (check (classes<=? (list tc-1 tc-4 tc-1)  (list tc-6 tc-6 tc-6)))
    (check (classes<=? (list tc-1 tc-1 tc-4)  (list tc-6 tc-6 tc-6)))
    (check (classes<=? (list tc-5 tc-1 tc-1)  (list tc-6 tc-6 tc-6)))
    (check (classes<=? (list tc-1 tc-5 tc-1)  (list tc-6 tc-6 tc-6)))
    (check (classes<=? (list tc-1 tc-1 tc-5)  (list tc-6 tc-6 tc-6)))
    
    (check (not (classes<=? (list tc-6 tc-1 tc-1) (list tc-1 tc-1 tc-1))))
    (check (not (classes<=? (list tc-1 tc-6 tc-1) (list tc-1 tc-1 tc-1))))
    (check (not (classes<=? (list tc-1 tc-1 tc-6) (list tc-1 tc-1 tc-1))))

    (check (not (classes<=? (list tc-5 tc-1 tc-1) (list tc-1 tc-1 tc-1))))
    (check (not (classes<=? (list tc-1 tc-5 tc-1) (list tc-1 tc-1 tc-1))))
    (check (not (classes<=? (list tc-1 tc-1 tc-5) (list tc-1 tc-1 tc-1))))

    (check (not (classes<=? (list tc-4 tc-1 tc-1) (list tc-1 tc-1 tc-1))))
    (check (not (classes<=? (list tc-1 tc-4 tc-1) (list tc-1 tc-1 tc-1))))
    (check (not (classes<=? (list tc-1 tc-1 tc-4) (list tc-1 tc-1 tc-1))))))

(define-test class-graph-numbers
  ;; Every real is a complex, so flonum sits under complex.
  (check (class<=? 'flonum 'complex))
  (check (class<=? 'complex 'number))
  (check (class<=? 'flonum 'number))
  (check (not (class<=? 'complex 'flonum)))
  (check (class<=? 'fixnum 'number)))

(define-generic-function (tcg-complex-only x)
  :default-handling)

(define-method (tcg-complex-only (x complex)) :complex)

(define-test class-graph-complex-dispatch
  ;; A method on complex applies to flonums, but not the reverse.
  (check (eq? (tcg-complex-only 3i) :complex))
  (check (eq? (tcg-complex-only 3.0) :complex))
  (check (eq? (tcg-complex-only 3) :default-handling)))
