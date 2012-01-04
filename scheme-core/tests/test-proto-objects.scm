(use-package! "unit-test")

(define-test proto-objects
  (let* ((point-2 (make-instance))
         (point-3 (make-instance point-2))
         (point-4 (make-instance point-3)))
    
    (slot-set! point-2 'x 3)
    (slot-set! point-2 'y 4)
    (slot-set! point-2 'class-name 'point-2)

    (test-case (eq? (has-slot? point-2 'x) :local))
    (test-case (eq? (has-slot? point-2 'not-there) #f))

    (test-case (eq? (has-slot? point-3 'x) :inherited))
    (test-case (eq? (has-slot? point-3 'not-there) #f))

    (test-case (eq? (has-slot? point-4 'x) :inherited))
    (test-case (eq? (has-slot? point-4 'not-there) #f))

    (define-message point-2 (r)
      (sqrt (+ (* @x @x) 
               (* @y @y))))
    
    (test-case (inexact-= [point-2 r] 5.0))
    (test-case (inexact-= [point-3 r] 5.0))
    (test-case (inexact-= [point-4 r] 5.0))
    
    (test-case (eq? @(point-2 class-name) 'point-2))
    (test-case (eq? @(point-3 class-name) 'point-2))
    (test-case (eq? @(point-4 class-name) 'point-2))

    (test-case (slot-set! point-2 'x 6.0))
    (test-case (slot-set! point-2 'y 8.0))

    (test-case (inexact-= [point-2 r] 10.0))
    (test-case (inexact-= [point-3 r] 10.0))
    (test-case (inexact-= [point-4 r] 10.0))

    (test-case (eq? @(point-2 class-name) 'point-2))
    (test-case (eq? @(point-3 class-name) 'point-2))
    (test-case (eq? @(point-4 class-name) 'point-2))

    (slot-set! point-2 'class-name 'point-2a)
    (test-case (eq? @(point-2 class-name) 'point-2a))
    (test-case (eq? @(point-3 class-name) 'point-2a))
    (test-case (eq? @(point-4 class-name) 'point-2a))

    (slot-set! point-3 'class-name 'point-3)
    (test-case (eq? @(point-2 class-name) 'point-2a))
    (test-case (eq? @(point-3 class-name) 'point-3))
    (test-case (eq? @(point-4 class-name) 'point-3))

    (slot-set! point-4 'class-name 'point-4)
    (test-case (eq? @(point-2 class-name) 'point-2a))
    (test-case (eq? @(point-3 class-name) 'point-3))
    (test-case (eq? @(point-4 class-name) 'point-4))

    (slot-set! point-2 'class-name 'point-2)
    (test-case (eq? @(point-2 class-name) 'point-2))
    (test-case (eq? @(point-3 class-name) 'point-3))
    (test-case (eq? @(point-4 class-name) 'point-4))))

