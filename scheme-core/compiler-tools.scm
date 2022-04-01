
;;;; compiler-tools.scm --
;;;;
;;;; Compiler utilities, including support for diagnostic
;;;; messages and global environment manipulation.
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

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
  (if (and trace? (list? args))
      (begin
        (scheme::trace-message ">>> ~a: ~s" label args)
        (let ((result (in-trace-level (fn))))
          (scheme::trace-message "<<< ~a: ~s" label result)
          result))
      (fn)))

(defmacro (with-compiler-tracing trace? arglist . body)
  (dbind (label . args) arglist
    `(call-with-compiler-tracing ,trace? ',label (lambda () ,@body) ,@args)))

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
