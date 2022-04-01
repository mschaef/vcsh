
;;;; sets.scm --
;;;;
;;;; A primitive set/bag data type.
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (make-set-hash key-type)
  (case key-type
    ((:eq) (make-identity-hash))
    ((:equal) (make-hash))
    (#t
     (error "Invalid set key type: ~s" key-type))))

(define (%set-union key-type xss)
  "Compute the union of the sets <xss>, where equivalence is determined
   using the <key-type> hash key type. The result is guaranteed to be a set."
  (let ((objects (make-set-hash key-type)))
    (dolist (xs xss)
      (dolist (x xs)
        (hash-set! objects x #t)))
    (hash-keys objects)))

(define (set-union . xss)
  "Compute the union of sets <xss>, with elements distinguished by equal."
  (%set-union :equal xss))

(define (set-union/eq . xss)
  "Compute the union of sets <xss>, with elements distinguished by eq."
  (%set-union :eq xss))

(define (list->set . xss)
  "Compute the set of unique, as distinguished by equal, objects in the list <xs>"
  (%set-union :equal xss))

(define (list->set/eq . xss)
  "Compute the set of unique, as distinguished by eq?, objects in the list <xs>"
  (%set-union :eq xss))

(define (%set-diff key-type xss)
  "Compute the difference of the sets <xss>, where equivalence is determined
   using the <key-type> hash key type. The result is guaranteed to be a set."
  (let ((objects (make-set-hash key-type)))
    (dolist (x (car xss)) (hash-set! objects x #t))
    (dolist (xs (cdr xss)) (dolist (x xs) (hash-set! objects x #f)))
    (hash-keys/t objects)))

(define (set-diff . xss)
  "Compute the difference of sets <xss>, with elements distinguished by equal."
  (%set-diff :equal xss))

(define (set-diff/eq . xss)
  "Compute the difference of sets <xss>, with elements distinguished by eq."
  (%set-diff :eq xss))

(define (set-same? x y)
  "Returns true if the two sets <x> and <y> have the same elements."
  (equal? (set-union x) (set-union y)))

(define (list->hash-set key-type xs)
  (let ((hash-set (make-set-hash key-type)))
    (dolist (x xs) (hash-set! hash-set x #t))
    hash-set))

(define (%set-isect key-type xss)
  (let ((hash-sets (map #L(list->hash-set key-type _) xss)))
    (list->set (filter (lambda (x) (every? #L(hash-has? _ x) hash-sets)) (car xss)))))

(define (set-isect . xss)
  "Compute the set intersection of sets <xss>, with elements distinguished by equal."
  (%set-isect :equal xss))

(define (set-isect/eq . xss)
  "Compute the set intersection of sets <xss>, with elements distinguished by eq."
  (%set-isect :eq xss))

(define (set-equivalent? xs ys :optional (key-type :eq))
  "Determines if <xs> and <ys> are set equivalent, that is, they both
   contain the same set of elements. <key-type> specifies the hash table
   key type used to determine element equivalence."
  (let ((xset (list->hash-set key-type xs))
        (yset (list->hash-set key-type ys)))
    (let loop ((xs xs) (ys ys))
      (if (and (null-list? xs) (null-list? ys))
          #t
          (and (not (null? xs)) (not (null? ys))
               (hash-has? xset (car ys)) (hash-has? yset (car xs))
               (loop (cdr xs) (cdr ys)))))))

(define (->bag xs)
  "Given a list <xs>, return a bag. The resultant bag is an a-list
   binding values in <xs> to the number of times they occur in
   the original list."
 (let ((hash (make-set-hash)))
   (dolist (x xs)
     (hash-set! hash x (+ 1 (hash-ref hash x 0))))
   (hash->a-list hash)))
