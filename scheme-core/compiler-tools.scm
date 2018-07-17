
;;;; compiler-tools.scm --
;;;;
;;;; Compiler utilities, including support for diagnostic
;;;; messages and global environment manipulation.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Option variables

;(scheme::eval-when (:compile-toplevel :load-toplevel :execute)
;  (host-scheme::repl))

(define *debug* #f)
(define *verbose* #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Diagnostic messages

(define (compiler-trace trace? . format-args)
  "Write a compiler trace message if <trace?> is true. <format-args> are passed
   into format to generate the output text, written to the compiler output port.
   If <trace?> is #f, nothing is done."
  (when trace?
    (apply scheme::trace-message format-args)))

(define (call-with-compiler-tracing trace? label fn . args)
  (let ((trace? (and trace? (list? args))))
    (define (parse-label)
      (cond ((string? label) (values label "TO"))
            ((list? label)
             (case (length label)
               ((0) (values "FROM" "TO"))
               ((1) (values (car label) "TO"))
               (#t (values (car label) (cadr label)))))
            (#t
             (error "Invalid trace label: ~s" label))))
    (define (message prefix label args)
      (compiler-trace trace? "~a ~a: ~s" prefix label args))
    (if trace?
        (mvbind (from-label to-label) (parse-label)
          (message ">" from-label args)
          (mvbind results (in-trace-level (apply fn args))
            (message "<" to-label results)
            (apply values results)))
        (apply fn args))))


;;;; Ways to signal compilation events

(define (compile-warning context-form message . args)
  (signal 'compile-warning context-form message args))

(define (compile-error context-form message . message-args)
  (signal 'compile-error context-form #f message message-args))

(define (compile-fatal-error context-form message . message-args)
  (signal 'compile-error context-form #t message message-args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Compiler environment

(define *show-actions* #f)

(define (compiler-evaluate form)
  "Evaluates <form>, signaling a compiler-error in the event of a failure."
  (catch 'end-compiler-evaluate
    (compiler-trace *show-actions* "==> COMPILER-EVALUATE: ~s\n" form)
    (handler-bind  ((runtime-error
                     (if *debug*
                         handle-runtime-error
                         (lambda args
                           (compile-error form "Runtime error while evaluating toplevel form: ~s" args)
                           (throw 'end-compiler-evaluate)))))
      (eval form))))
