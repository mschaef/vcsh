
;;;; list-statistics.scm --
;;;;
;;;; Statistics functions over lists
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (list-sum pts)
  "Computes the sum of a list <pts> of points."
  (apply + pts))

(define (list-mean pts)
  "Computes the mean of a list <pts> of points."
  (/ (list-sum pts) (length pts)))

(define (list-mean-center pts)
  "Centers a list of data points, <pts> around its mean."
  (let ((mean (list-mean pts)))
    (map (lambda (x) (- x mean)) pts)))

(define (list-stat-sums pts) 
  "Retuns a list of sums over <pts> typically used in cumulative statistics. (count x-sum x2-sum y-sum y2-sum xy-sum)"
  (let sum-next ((pts pts) (x-sum 0.0) (x2-sum 0.0) (y-sum 0.0) (y2-sum 0.0) (xy-sum 0.0) (count 0))
    (cond ((null? pts) (values count x-sum x2-sum y-sum y2-sum xy-sum))
          ((atom? pts) (error "Invalid list to list-sum-stats [ ~a ]" pts))
          (#t
           (let ((x (real-part (car pts)))
                 (y (imag-part (car pts))))
             (sum-next (cdr pts) (+ x x-sum) (+ (* x x) x2-sum) (+ y y-sum) (+ (* y y) y2-sum) (+ (* x y) xy-sum) (+ count 1)))))))

(define (stats-list? pts)
  "Determines if <pts> is suitable for use with scan's built in descriptive
   statistics package. That is, it's a proper list of numbers."
  (and (list? pts)
       (every? number? pts)))

(define (list-2d? pts)
  "Returns <pts> if it is a proper list of 2-dimensional points, #f otherwise. To qualify,
   <pts> must be a proper list of numbers, at least one of which has a complex component."
  (if (and (list? pts)
           (every? number? pts)
           (any? complex? pts))
      pts
      #f))

(define (list-sdev pts)
  "Computes the standard deviation of the numbers in the list <pts>. If any point in <pts> i
   two dimentional (a complex number), then this function returns two standard deviations,
   one for each dimension. The two standard deviations are returned as the real and imaginary
   components of a complex number."
  (define (sdev su2 n) (sqrt (* (/ 1 (- n 1)) su2)))
  (check stats-list? pts)
  (mvbind (count x-sum x2-sum y-sum y2-sum xy-sum)
      (list-stat-sums (list-mean-center pts))
    (if (list-2d? pts)
        (make-rectangular
         (sdev x2-sum count)
         (sdev y2-sum count))
        (sdev x2-sum count))))

(define (list-lr pts)
  (check stats-list? pts)
  (dbind (x-mean y-mean) (rectangular->list (list-mean pts))
    (mvbind (count x-sum x2-sum y-sum y2-sum xy-sum) (list-stat-sums pts)
      (let ((b (/ xy-sum x2-sum)))
        (vector b (- y-mean (* b x-mean)))))))

(define (list-cov pts population?)
  (check stats-list? pts)
  (mvbind (count x-sum x2-sum y-sum y2-sum xy-sum) (list-stat-sums pts)
    (* (/ 1 (- count (if population? 0 1.0)))
       xy-sum)))

(define (list-corr pts)
  (check stats-list? pts)
  (mvbind (count x-sum x2-sum y-sum y2-sum xy-sum) (list-stat-sums pts)
    (/ xy-sum
       (sqrt (* x2-sum y2-sum)))))

