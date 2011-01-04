;;;; compiler-tools.scm
;;;;
;;;; Compiler utilities, including support for diagnostic
;;;; messages and global environment manipulation.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Option variables

;(scheme::eval-when (:compile-toplevel :load-toplevel :execute)
;  (host-scheme::repl))

(define *debug* #f)
(define *verbose* #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Diagnostic messages

(define *compiler-output-port* (current-error-port))
(define *compiler-error-port* (current-error-port))

(define (trace-message write? . format-args)
  "Write a compiler trace message if <write?> is true. <format-args> are passed
   into format to generate the output text, written to the compiler output port.
   If <write?> is #f, nothing is done."
  (when write?
    (trace-indent *compiler-output-port*)
    (apply format *compiler-output-port* format-args)))

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
      (when trace?
        (trace-indent *compiler-output-port*)
        (format  *compiler-output-port* "~a ~a:" prefix label)
        (dolist (arg args)
          (format *compiler-output-port* " ~s"arg))
        (newline *compiler-output-port*)))
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

(define (compiler-evaluate form genv)
  "Evaluates <form> in global environment <genv>, signaling a compiler-error in
   the event of a failure."
  (catch 'end-compiler-evaluate
    (trace-message *show-actions* "==> COMPILER-EVALUATE: ~s genv=~@\n" form genv)
    (handler-bind  ((runtime-error
                     (if *debug*
                         handle-runtime-error
                         (lambda args
                           (compile-error form "Runtime error while evaluating toplevel form: ~s" args)
                           (throw 'end-compiler-evaluate)))))
      (eval form))))



(define (symbol-value-with-bindings symbol bindings :optional (unbound-value #f))
  (check symbol? symbol)
  (if (symbol-bound? symbol)
      (symbol-value symbol)
      unbound-value))
