;;;; number.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; Number library procedures

(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (even? x) (= 0 (remainder x 2)))
(define (odd? x) (= 1 (remainder x 2)))

(define (bits->exact bits)
  "Given a list of bits <bits>, return an exact number with those
    bits set. <bits> is a list of exact numbers, numbering bits
    starting with 0 in the LSB."
  (let loop ((n 0) (remaining bits))
    (cond ((null? remaining)
           n)
          ((not (pair? remaining))
           (error "Bits must be specified in a proper list: ~a" bits))
          ((not (exact? (car remaining)))
           (error "Bits must be specified as exact numbers: ~a" (car remaining)))
          (#t
           (loop (bitwise-or n (bitwise-shift-left 1 (car remaining))) (cdr remaining))))))


(define *bits* 64) ;; TODO : pull from system-info

(define (exact->bits n)
  (check exact? n)
  (let loop ((n n) (position 0) (bits ()))
    (cond ((= n 0) bits)
          ((= 1 (bitwise-and n 1))
           (loop (bitwise-arithmatic-shift-right n 1) (+ position 1) (cons position bits)))
          (#t
           (loop (bitwise-arithmatic-shift-right n 1) (+ position 1) bits)))))

(define (population-count n)
  (check exact? n)
  (let loop ((n n) (bit-count 0))
    (cond ((= n 0) bit-count)
          ((= 1 (bitwise-and n 1))
           (loop (bitwise-arithmatic-shift-right n 1) (+ bit-count 1)))
          (#t
           (loop (bitwise-arithmatic-shift-right n 1) bit-count)))))

(define (bit-set? x b)
  "Returns #t if the <b>'th bit of <x> is set. Returns #f otherwise."
  (not (= 0 (bitwise-and (bitwise-arithmatic-shift-right x b) 1))))

(define (min/2 x y)
  "Returns the lesser of <x> and <y>."
  (if (< x y) x y))

(define (max/2 x y)
  "Returns the greater of <x> and <y>."
  (if (> x y) x y))

(define (min . xs) ; REVISIT: use reduce ?
  "Returns the least value of <xs>."
  (check (> 0) (length xs))
  (let loop ((current-min (car xs)) (xs (cdr xs)))
    (if (null? xs)
        current-min
        (loop (min/2 current-min (car xs))  (cdr xs)))))

(define (max . xs) ; REVISIT: use reduce ?
  "Returns the greatest value of <xs>."
  (check (> 0) (length xs))
  (let loop ((current-max (car xs)) (xs (cdr xs)))
    (if (null? xs)
        current-max
        (loop (max/2 current-max (car xs))  (cdr xs)))))


(define (within? x a b)
  "Determines if <x> is within [<a>, <b>], regardless of the relative order
   of <a> and <b>."
  (if (< a b)
      (and (<= a x) (<= x b))
      (and (<= b x) (<= x a))))

(define (abs x)
  (cond ((inexact? x) (magnitude x))
        ((> x 0) x)
        (#t (- x))))

(define *log-of-10* (log 10))

(define (log10 x) (/ (log x) *log-of-10*))
(define (exp10 x) (expt 10 x))

(define (rectangular->list r)
  (list (real-part r) (imag-part r)))

(define (rectangular-values r)
  (values (real-part r) (imag-part r)))

(define *precision* 0.0000000001)

(define (inexact-= x y)
  (< (abs (- x y)) *precision*))

;;; !! Implement Scheme versions of gcd and lcm

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

(define (list-stat-sums pts) ; TODO: return multiple values instead of a list.
  "Retuns a list of sums over <pts> typically used in cumulative statistics. (count x-sum x2-sum y-sum y2-sum xy-sum)"
  (let sum-next ((pts pts) (x-sum 0.0) (x2-sum 0.0) (y-sum 0.0) (y2-sum 0.0) (xy-sum 0.0) (count 0))
    (cond ((null? pts) (list count x-sum x2-sum y-sum y2-sum xy-sum))
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

(define (list-sdev pts)
  "Computes the standard deviation of the numbers in the list <pts>. If any point in <pts> i
   two dimentional (a complex number), then this function returns two standard deviations,
   one for each dimension. The two standard deviations are returned as the real and imaginary
   components of a complex number."
  (define (sdev su2 n) (sqrt (* (/ 1 (- n 1)) su2)))
  (check stats-list? pts)
  (dbind (count x-sum x2-sum y-sum y2-sum xy-sum)
         (list-stat-sums (list-mean-center pts))
         (if (list-2d? pts)
             (make-rectangular
              (sdev x2-sum count)
              (sdev y2-sum count))
             (sdev x2-sum count))))

(define (list-lr pts)
  (check stats-list? pts)
  (dbind (x-mean y-mean) (rectangular->list (list-mean pts))
         (dbind (count x-sum x2-sum y-sum y2-sum xy-sum) (list-stat-sums pts)
                (let ((b (/ xy-sum x2-sum)))
                  (vector b (- y-mean (* b x-mean)))))))

(define (list-cov pts population?)
  (check stats-list? pts)
  (dbind (count x-sum x2-sum y-sum y2-sum xy-sum) (list-stat-sums pts)
         (* (/ 1 (- count (if population? 0 1.0)))
            xy-sum)))

(define (list-corr pts)
  (check stats-list? pts)
  (dbind (count x-sum x2-sum y-sum y2-sum xy-sum) (list-stat-sums pts)
         (/ xy-sum
            (sqrt (* x2-sum y2-sum)))))

(defmacro (incr! var . value)
  "Increments <var> by <value>, where value defaults to 1"
  `(set! ,var (+ ,var ,(if (null? value) 1 (car value)))))

