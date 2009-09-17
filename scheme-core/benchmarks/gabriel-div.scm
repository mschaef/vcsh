;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         div.sch
; Description:  DIV benchmarks
; Author:       Richard Gabriel
; Created:      8-Apr-85
; Modified:     19-Jul-85 18:28:01 (Bob Shaw)
;               23-Jul-87 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; DIV2 -- Benchmark which divides by 2 using lists of n ()'s.
;;; This file contains a recursive as well as an iterative test.
 
(define (create-n-list n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))
 
(define *ll* (create-n-list 200))
 
(define (iterative-div2 l)
  (do ((l l (cddr l))
       (a '() (cons (car l) a)))
      ((null? l) a)))
 
(define (recursive-div2 l)
  (cond ((null? l) '())
        (#t (cons (car l) (recursive-div2 (cddr l))))))
 
(define (iterative-div-test l)
  (do ((i 300 (- i 1)))
      ((= i 0))
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)))
 
(define (recursive-div-test l)
  (do ((i 300 (- i 1)))
      ((= i 0))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))
 
;;; for the iterative test call: (iterative-div-test *ll*)
;;; for the recursive test call: (recursive-div-test *ll*)

(defbench gabriel-div-iterative
  (account
   (iterative-div-test *ll*)))

(defbench gabriel-div-recursive
  (account
   (recursive-div-test *ll*)))


 
