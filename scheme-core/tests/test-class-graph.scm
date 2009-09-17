(use-package! "unit-test")

(define-test class-graph
  (with-gensyms (tc-1 tc-2 tc-3 tc-4 tc-5 tc-6)
    (test-case (not (runtime-error? (make-class< tc-1 tc-2))))
    (test-case (not (runtime-error? (make-class< tc-2 tc-6))))
    (test-case (not (runtime-error? (make-class< tc-3 tc-6))))
    (test-case (not (runtime-error? (make-class< tc-5 tc-6))))
    (test-case (not (runtime-error? (make-class< tc-4 tc-5))))

    (test-case (runtime-error? (make-class< 1 tc-2)))
    (test-case (runtime-error? (make-class< tc-2 1)))

    (test-case (runtime-error? (make-class< tc-6 tc-6)))
    (test-case (runtime-error? (make-class< tc-6 tc-1)))

    (test-case (eq? (class-superclass tc-1) tc-2))
    (test-case (eq? (class-superclass tc-2) tc-6))
    (test-case (eq? (class-superclass tc-6) #f))

    (test-case (equal? (class-superclasses tc-1) (list tc-1 tc-2 tc-6)))
    (test-case (equal? (class-superclasses tc-2) (list tc-2 tc-6)))
    (test-case (equal? (class-superclasses tc-6) (list tc-6)))

    (test-case (class<=? tc-1 tc-1))
    (test-case (class<=? tc-1 tc-6))
    (test-case (class<=? tc-4 tc-6))

    (test-case (not (class<=? tc-6 tc-1)))
    (test-case (not (class<=? tc-4 tc-2)))

    (test-case (classes<=? (list tc-1) (list tc-1)))
    (test-case (classes<=? (list tc-1) (list tc-6)))
    (test-case (classes<=? (list tc-4) (list tc-6)))

    (test-case (not (classes<=? (list tc-6) (list tc-1))))
    (test-case (not (classes<=? (list tc-4) (list tc-2))))

    (test-case (classes<=? (list tc-1 tc-1) (list tc-1 tc-1)))
    (test-case (classes<=? (list tc-1 tc-1) (list tc-6 tc-6)))
    (test-case (classes<=? (list tc-4 tc-1) (list tc-6 tc-6)))

    (test-case (not (classes<=? (list tc-6 tc-6) (list tc-1 tc-1))))
    (test-case (not (classes<=? (list tc-6 tc-6) (list tc-1 tc-6))))
    (test-case (not (classes<=? (list tc-4 tc-4) (list tc-2 tc-2))))
    (test-case (not (classes<=? (list tc-6 tc-6) (list tc-4 tc-6))))

    (test-case (not (classes<=? (list tc-1) (list tc-1 tc-1)))) 
    (test-case (not (classes<=? (list tc-1) (list tc-6 tc-6))))
    (test-case (not (classes<=? (list tc-6) (list tc-6 tc-6))))
    (test-case (not (classes<=? (list tc-6) (list tc-4 tc-4))))

    (test-case (classes<=? (list tc-1 tc-2) (list tc-6 tc-6)))
    (test-case (classes<=? (list tc-2 tc-1) (list tc-6 tc-6)))

    (test-case (classes<=? (list tc-1 tc-6) (list tc-6 tc-6)))
    (test-case (classes<=? (list tc-6 tc-1) (list tc-6 tc-6)))

    (test-case (classes<=? (list tc-1 tc-1 tc-1)  (list tc-1 tc-1 tc-1)))
    (test-case (classes<=? (list tc-1 tc-1 tc-1)  (list tc-6 tc-1 tc-1)))
    (test-case (classes<=? (list tc-1 tc-1 tc-1)  (list tc-6 tc-6 tc-1)))
    (test-case (classes<=? (list tc-1 tc-1 tc-1)  (list tc-6 tc-6 tc-6)))
    (test-case (classes<=? (list tc-1 tc-1 tc-1)  (list tc-1 tc-1 tc-6)))

    (test-case (classes<=? (list tc-4 tc-1 tc-1)  (list tc-6 tc-6 tc-6)))
    (test-case (classes<=? (list tc-1 tc-4 tc-1)  (list tc-6 tc-6 tc-6)))
    (test-case (classes<=? (list tc-1 tc-1 tc-4)  (list tc-6 tc-6 tc-6)))
    (test-case (classes<=? (list tc-5 tc-1 tc-1)  (list tc-6 tc-6 tc-6)))
    (test-case (classes<=? (list tc-1 tc-5 tc-1)  (list tc-6 tc-6 tc-6)))
    (test-case (classes<=? (list tc-1 tc-1 tc-5)  (list tc-6 tc-6 tc-6)))
    
    (test-case (not (classes<=? (list tc-6 tc-1 tc-1) (list tc-1 tc-1 tc-1))))
    (test-case (not (classes<=? (list tc-1 tc-6 tc-1) (list tc-1 tc-1 tc-1))))
    (test-case (not (classes<=? (list tc-1 tc-1 tc-6) (list tc-1 tc-1 tc-1))))

    (test-case (not (classes<=? (list tc-5 tc-1 tc-1) (list tc-1 tc-1 tc-1))))
    (test-case (not (classes<=? (list tc-1 tc-5 tc-1) (list tc-1 tc-1 tc-1))))
    (test-case (not (classes<=? (list tc-1 tc-1 tc-5) (list tc-1 tc-1 tc-1))))

    (test-case (not (classes<=? (list tc-4 tc-1 tc-1) (list tc-1 tc-1 tc-1))))
    (test-case (not (classes<=? (list tc-1 tc-4 tc-1) (list tc-1 tc-1 tc-1))))
    (test-case (not (classes<=? (list tc-1 tc-1 tc-4) (list tc-1 tc-1 tc-1))))

    ))
