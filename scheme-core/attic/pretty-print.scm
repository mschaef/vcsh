
;;;; pretty-print.scm --
;;;;
;;;; A simple pretty printer for scheme forms
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;;; A simple Scheme pretty printer. The design of this code is roughly
;;; inspired both by the Common Lisp XP pretty-printer and by GNU Emacs'
;;; autoformat logic for scheme-mode. From XP, this code takes a two stage
;;; approach to formatting. The first stage of pretty printing linearizes
;;; a form into a series of instructions for a second formatting stage.
;;; From GNU Emacs, this code takes an approach for specifying pretty print
;;; control variables. Each type of form gets a control object that
;;; specifies how it is to be formatted.

(define (debug-formatter os pass-through)
  (lambda (tok arg)
    (when *pp-debug*
      (format os "; ~s :: ~s\n" tok arg))
    (pass-through tok arg)))


(define *pretty-print-width* 72)

(define *pp-debug* #f)

(define (debug-formatter os pass-through)
  (lambda (tok arg)
    (when *pp-debug*
      (format os "; ~s :: ~s\n" tok arg))
    (pass-through tok arg)))


(define *pretty-print-width* 40)

(define (standard-formatter os)
  (define (current-x-pos)
    (cdr (port-location os)))
  (let* ((x-pos (current-x-pos))
         (wrapped? '(#f))
         (indents (list x-pos))
         (line-buffer (make-queue)))
    (define (next-line)
      (newline os)
      (indent (car indents) #\space os)
      (set! x-pos (current-x-pos)))
    (define (add-to-line! x)
      (when (> (+ (length x) x-pos) *pretty-print-width*)
        (set-car! wrapped? #t)
        (write-current-line!)
        (next-line))
      (q-enqueue! x line-buffer)
      (incr! x-pos (length x)))
    (define (write-current-line!)
      (for-each display (q-items line-buffer))
      (set! line-buffer (make-queue)))
    (lambda (tok arg)
      (case tok
        ((:enter)
         (add-to-line! arg)
         (push! #f wrapped?)
         (push! x-pos indents))
        ((:leave)
         (let ((need-new-line (car wrapped?)))
           (pop! wrapped?)
           (pop! indents)
           (add-to-line! arg)
           (write-current-line!)
           (when need-new-line
             (next-line))))
        ((:space)
         (add-to-line! arg))
        ((:atom)
         (let ((as-string (with-output-to-string (write arg))))
           (add-to-line! as-string)))
        (#t
         (error "Invalid pretty-print formatter token: ~a" tok))))))


; :enter <form-beginning>
; :leave <form-end>
; :atom <obj>


(define (pretty-print x :optional (os (current-output-port)))
  (let ((formatter (debug-formatter (current-debug-port) (standard-formatter os))))
    (let pp/next ((x x))
      (define (pp/seq xs)
        (let loop ((xs xs) (need-space? #f))
          (unless (null? xs)
            (when need-space?
              (formatter :space " "))
            (cond ((pair? xs)
                   (pp/next (car xs))
                   (loop (cdr xs) #t))
                  (#t
                   (formatter :atom '.)
                   (formatter :space " ")
                   (formatter :atom xs))))))
      (cond ((vector? x)
             (formatter :enter "#(")
             (for-each pp/next x)
             (formatter :leave ")"))
            ((pair? x)
             (formatter :enter "(")
             (pp/seq x)
             (formatter :leave ")"))
            ((hash? x)
             (formatter :enter "#h(")
             (pp/next (hash-type x))
             (formatter :space " ")
             (dohash (k v x)
               (formatter :atom k)
               (formatter :space " ")
               (formatter :atom v)
               (formatter :space " "))
             (formatter :leave ")"))
            (#t
             (formatter :atom x)))))
  os)


(define (pp x)
  (newline)
  (pretty-print x))

(define *test* '((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)
                 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)
                 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)))

; :enter <form-beginning>
; :leave <form-end>
; :atom <obj>


(define (pretty-print x :optional (os (current-output-port)))
  (let ((formatter (debug-formatter (current-debug-port) (standard-formatter os))))
    (let pp/next ((x x))
      (define (pp/seq xs)
        (let loop ((xs xs) (need-space? #f))
          (unless (null? xs)
            (when need-space?
              (formatter :space " "))
            (cond ((pair? xs)
                   (pp/next (car xs))
                   (loop (cdr xs) #t))
                  (#t
                   (formatter :atom '.)
                   (formatter :space " ")
                   (formatter :atom xs))))))
      (cond ((vector? x)
             (formatter :enter "#(")
             (for-each pp/next x)
             (formatter :leave ")"))
            ((pair? x)
             (formatter :enter "(")
             (pp/seq x)
             (formatter :leave ")"))
            ((hash? x)
             (formatter :enter "#h(")
             (pp/next (hash-type x))
             (formatter :space " ")
             (dohash (k v x)
               (formatter :atom k)
               (formatter :space " ")
               (formatter :atom v)
               (formatter :space " "))
             (formatter :leave ")"))
            (#t
             (formatter :atom x)))))
  os)


(define (pp x)
  (newline)
  (pretty-print x))

(define *test* '((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)
                 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)
                 (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
                    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)))