
;;;; unit-test-utils.scm -- 
;;;;
;;;; A set of utilities for common unit test scenarios.
;;;;
;;;; (C) Copyright 2001-2015 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define-package "unit-test-utils"
  (:uses "scheme"
         "unit-test")
  (:exports "checkpoint"
            "checkpoint-order-of"
            "read-error?"
            "runtime-error?"
            "can-fast-io-round-trip?"
            "can-read/write-round-trip?"
            "fast-io-round-trip"
            "load-tests"
            "values-eq?"
            "values-equal?"
            "dtu-lambda-list"))

;;;; Execution order checking.

(define *current-execution-order* #f)

(define (checkpoint-order fn)
  (dynamic-let ((*current-execution-order* '()))
    (fn)
    (reverse *current-execution-order*)))

(defmacro (checkpoint-order-of . code)
  `(checkpoint-order
    (lambda () ,@code)))

(define (checkpoint point-name :optional return-value)
  (unless *current-execution-order*
    (error "Cannot make a checkpoint outside of a checkpoint block."))
  (push! point-name *current-execution-order*)
  return-value)

;;;; Error validation

(define *runtime-error-escape* (gensym "error-escape"))

(define (throws-runtime-error? fn)
  "Returns #t if <fn> throws a runtime error, #f otherwise."
  (catch *runtime-error-escape*
    (handler-bind ((runtime-error (lambda arg
                                    (throw *runtime-error-escape*
                                           #t))))
      (fn)
      #f)))

(defmacro (runtime-error? . code)
  "Returns #t if the execution of <code> throws a runtime error, #f otherwise."
  `(throws-runtime-error? (lambda () ,@code)))

(define *read-error-escape* (gensym "error-escape"))

(define (throws-read-error? fn)
  "Returns #t if <fn> throws a read error, #f otherwise."
  (catch *read-error-escape*
    (handler-bind ((read-error (lambda arg
                                 (throw *read-error-escape*
                                        #t))))
      (fn)
      #f)))

(defmacro (read-error? . code)
  "Returns #t if the execution of <code> throws a read error, #f otherwise."
  `(throws-read-error? (lambda () ,@code)))

;;;; Utility functions

(define (read/write-round-trip obj)
  "Returns the result of writing <object> to a string and reading it back in.
   (Theoretically, this should be the same thing for printable objects.)"
  (catch-all
   (read-from-string (write-to-string obj))))

(define (can-read/write-round-trip? obj)
  "Given an object, determine if it can be written, and read back in,
   resulting in an equivalent object."
  (let ((obj2 (read/write-round-trip obj)))
    (equal? obj obj2)))

(define (fast-io-round-trip object :optional (dump-fasl-file? #f))
  "Returns the result of fast writing <object> to a binary file and
   reading it back in. (Theoretically, this should be the same thing.)"
  (let ((test-filename (temporary-file-name "sct")))
    (with-port p (open-file test-filename :mode :write :encoding :binary)
      (with-fasl-stream s p
                        (fasl-write s object)))
    (let ((object ()))
      (when dump-fasl-file?
        (system (format #f "../vm/fasl-dump ~a" test-filename)))
      (with-port p (open-file test-filename :encoding :binary)
        (set! object (fast-read (make-fasl-reader p))))
      (delete-file test-filename)
      object)))

(define (can-fast-io-round-trip? obj)
  (let ((obj2 (fast-io-round-trip obj)))
    (EQUAL? obj obj2)))

(define (shallow-list=? xs ys :optional (test eq?))
  (let loop ((xs xs) (ys ys))
    (cond ((and (null? xs) (null? ys))
           #t)
          ((and (pair? xs) (pair? ys))
           (and (test (car xs) (car ys))
                (loop (cdr xs) (cdr ys))))
          (#t
           (test xs ys)))))

(define (values-eq? actual-values . expecteds)
  (let ((values (mvbind xs actual-values xs)))
    (shallow-list=? values expecteds eq?)))

(define (values-equal? actual-values . expecteds)
  (let ((values (mvbind xs actual-values xs)))
    (shallow-list=? values expecteds equal?)))

(define (dtu-lambda-list procedure)
  (mvbind (real-ll source-ll) (procedure-lambda-list procedure)
    (if source-ll source-ll real-ll)))

