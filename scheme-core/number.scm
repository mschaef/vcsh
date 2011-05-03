
;;;; number.scm --
;;;;
;;;; The numeric library
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (trap-fixnum-overflow trapno frp arg-0 arg-1)
  (error "Fixnum overflow (trapno=~a, arg-0=~a, arg-1=~a)" trapno arg-0 arg-1))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_OVERFLOW_FIXNUM_ADD trap-fixnum-overflow)
  (%set-trap-handler! system::TRAP_OVERFLOW_FIXNUM_MULTIPLY trap-fixnum-overflow)
  (%set-trap-handler! system::TRAP_OVERFLOW_FIXNUM_NEGATE trap-fixnum-overflow)
  (%set-trap-handler! system::TRAP_OVERFLOW_FIXNUM_SUBTRACT trap-fixnum-overflow)
  (%set-trap-handler! system::TRAP_OVERFLOW_FIXNUM_DIVIDE trap-fixnum-overflow)
  (%set-trap-handler! system::TRAP_OVERFLOW_FIXNUM_MODULO trap-fixnum-overflow)
  (%set-trap-handler! system::TRAP_OVERFLOW_FIXNUM_SHL trap-fixnum-overflow))

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

;;; TODO: Implement Scheme versions of gcd and lcm

(defmacro (incr! var . value)
  "Increments <var> by <value>, where value defaults to 1"
  `(set! ,var (+ ,var ,(if (null? value) 1 (car value)))))

