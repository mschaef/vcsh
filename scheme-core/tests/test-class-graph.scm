(use-package! "unit-test")

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
