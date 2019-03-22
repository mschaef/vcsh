(define-package "test-slots"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-structure slot-test x y)

(define-test slot-ref/implicit
  (let ((obj {:x 3 :y 4}))
    (check (eq? (:x obj) 3))
    (check (eq? (:bad obj) #f))

    (check (eq? (:x obj :default) 3))
    (check (eq? (:bad obj :default) :default))

    (check (eq? (obj :x) 3))
    (check (eq? (obj :bad) #f))

    (check (eq? (obj :x :default) 3))
    (check (eq? (obj :bad :default) :default)))

  (let ((obj (make-slot-test :x 3 :y 3)))
    (check (eq? (:x obj) 3))
    (check (runtime-error? (:bad obj)))

    (check (eq? (:x obj :default) 3))
    (check (eq? (:bad obj :default) :default))

    (check (eq? (obj :x) 3))
    (check (runtime-error? (obj :bad)))

    (check (eq? (obj :x :default) 3))
    (check (eq? (obj :bad :default) :default))))

(define-test slot-ref/explicit
  (let ((obj {:x 3 :y 4}))
    (check (eq? (slot-ref obj :x) 3))
    (check (eq? (slot-ref obj :bad) #f))

    (check (eq? (slot-ref obj :x :default) 3))
    (check (eq? (slot-ref obj :bad :default) :default)))
  
  (let ((obj (make-slot-test :x 3 :y 4)))
    (check (eq? (slot-ref obj :x) 3))
    (check (runtime-error? (slot-ref obj :bad)))
    
    (check (eq? (slot-ref obj :x :default) 3))
    (check (eq? (slot-ref obj :bad :default) :default))))
  
(define-test slot-set!
  (let ((obj {}))
    (slot-set! obj :x 3)
    (check (eq? (slot-ref obj :x) 3)))

  (let ((obj (make-slot-test)))
    (slot-set! obj :x 3)    
    (check (eq? (slot-ref obj :x) 3))

    (check (runtime-error? (slot-set! obj :bad 12)))))
