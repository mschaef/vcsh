;;;; EQUAL.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; The big, bad EQUAL function

(define (EQUAL? x y)
  "The most strict equality check, returns #t iff two objects are both equal? and
   have matching structure. This equality check is safe to use with circular
   structures."

  ;; EQUAL? reduces to equal? except in the case where an object has muitiple
  ;; self referential pointers. This happens when there is shared or circular
  ;; structure in the object. (let ((a '(1 2))) (cons a a)) is a simple example
  ;; of an expression that produces an object with shared structure. While
  ;; it is equal? to ((1 2) 1 2) it is not EQUAL? to ((1 2) 1 2).
  ;;
  ;; To make this check, EQUAL? keeps a record of each node it encounters
  ;; in both x and y, along with the corresponding node in the other object.
  ;; For x and y to be EQUAL?, any time a particular node of x is reached,
  ;; the same node of y needs to be reached each time. In the case of
  ;; (let ((a '(1 2))) (cons a a)), subexpression a is reached twice,
  ;; but each occurrance maps to a different node in ((1 2) 1 2). Therefore,
  ;; the two objects are not EQUAL?, since they share structure differently.
  ;;
  ;; Note that this also guards against circular structures. Whenever a node
  ;; is reached a second time, EQUAL? reduces to a shallow eq? check against
  ;; the corresponding node of the other object. This stops all infinite
  ;; traversals.

  (let ((mapping/x->y (make-hash :eq))
        (mapping/y->x (make-hash :eq))
        (unmapped-node (gensym "unmapped-node"))) ; guaranteed not to be in x or y...
    (let check-next ((x x) (y y))
      (define (hash-maps-onto? x y)
        (catch 'decided-early
          (dohash (x-k x-v x #t)
                  (let ((y-k/v (hash-ref* y x-k)))
                    (unless (and y-k/v
                                 (check-next x-k (car y-k/v))
                                 (check-next x-v (cdr y-k/v)))
                      (throw 'decided-early #f))))))
      (let ((x->y (hash-ref mapping/x->y x unmapped-node))
            (y->x (hash-ref mapping/y->x y unmapped-node)))
        (if (or (not (eq? unmapped-node x->y))
                (not (eq? unmapped-node y->x)))
            (and (not (eq? unmapped-node x->y))
                 (not (eq? unmapped-node y->x))
                 (eq? y x->y)
                 (eq? x y->x))
            (begin
              (hash-set! mapping/x->y x y)
              (hash-set! mapping/y->x y x)
              (and (eq? (type-of x) (type-of y))
                   (case (%representation-of x)
                     ((cons)
                      (and (check-next (car x) (car y))
                           (check-next (cdr x) (cdr y))))
                     ((vector)
                      (let ((lx (length x)))
                        (and (= lx (length y))
                             (let loop ((i 0))
                               (cond ((>= i lx)
                                      #t)
                                     ((check-next (vector-ref x i) (vector-ref y i))
                                      (loop (+ i 1)))
                                     (#t
                                      #f))))))
                     ((instance)
                      (and (check-next (%instance-proto x) (%instance-proto y))
                           (null? (set-diff/eq (direct-instance-slots x)
                                               (direct-instance-slots y)))
                           (null? (set-diff/eq (direct-instance-slots y)
                                               (direct-instance-slots x)))
                           (let loop ((slots (direct-instance-slots y)))
                             (if (null? slots)
                                 #t
                                 (and (check-next (slot-ref x (car slots))
                                                  (slot-ref y (car slots)))
                                      (loop (cdr slots)))))))
                     ((hash)
                      ;; There are four critera for hash tables to be EQUAL:
                      (and (hash? y)                         ; 1. They're both hash tables
                           (eq? (hash-type x) (hash-type y)) ; 2. They're of the same type
                           (hash-maps-onto? x y)             ; 3. y has all of x's key/value pairs
                           (hash-maps-onto? y x)))           ; 4. x has all of y's key/value pairs
                     ((structure)
                      (and (= (%structure-length x) (%structure-length y))
                           (eq? (%structure-layout x) (%structure-layout y))
                           (let loop ((ii 0))
                             (cond ((>= ii (%structure-length x))
                                    #t)
                                   ((check-next (%structure-ref x ii) (%structure-ref y ii))
                                    (loop (+ ii 1)))
                                   (#t
                                    #f)))))
                     (#t
                      (equal? x y))))))))))
