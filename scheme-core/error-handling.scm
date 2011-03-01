
;;;; error-handling.scm --
;;;;
;;;; Error/Condition/Signal handling.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (valid-signal-handler-list? handler-list)
  (or (null? handler-list)
      (and (pair? handler-list)
           (length=2? (car handler-list))
           (symbol? (caar handler-list))
           (valid-signal-handler-list? (cdr handler-list)))))

(defmacro (with-handler-frames new-frames . code)
  (with-gensyms (old-frames-sym)
    `(let ((,old-frames-sym (%%get-hframes)))
       (unwind-protect
        (lambda ()
          (%%set-hframes ,new-frames)
          ,@code)
        (lambda ()
          (%%set-hframes ,old-frames-sym))))))

(defmacro (handler-bind handler-forms . code) ; REVISIT: Allow for implicit lambdas in handler frames
  "Executes <code> with a handler frame established with the signal handlers
   defined in <handler-forms>. <handler-forms> is a list with elements of the
   formm (<condition-symbol> <handler-form>)."
  (define (handler-forms->frame-expression handler-forms)
    `(cons (list ,@(map (lambda (handler-form)
                          `(list ',(car handler-form) ,(cadr handler-form)))
                        handler-forms))
           (%%get-hframes)))
  (unless (valid-signal-handler-list? handler-forms)
    (error "Invalid signal hander list: ~a" handler-forms))
  `(with-handler-frames ,(handler-forms->frame-expression handler-forms)
                        ,@code))

(define (handle-condition condition args :optional (last-resort-handler #f))
  (unless (symbol? condition)
    (%panic (format #f "Condition ~s is not a symbol" condition)))
  (let loop ((remaining-handler-frames (%%get-hframes))
             (remaining-handlers '()))
    (cond ((not (null? remaining-handlers))
           (when (eq? condition (caar remaining-handlers))
             (with-handler-frames remaining-handler-frames
                                  (apply (cadar remaining-handlers) args)))
           (loop remaining-handler-frames (cdr remaining-handlers)))
          ((not (null? remaining-handler-frames))
           (loop (cdr remaining-handler-frames) (car remaining-handler-frames)))
          (#t
           (when last-resort-handler (last-resort-handler))))))

(define (show-signal-handlers)
  "Shows a list of all current signal handlers"
  (format (current-error-port) "Current Signal Handlers:\n--------------------------------\n")
  (table (%%get-hframes) #f (current-error-port)))

(define (signal condition . args)
  "Signals condition <condition> with the specified arguments. The signal
   is signaled for informational purposes only: execution will continue
   normally after the signal is processed."
  (handle-condition condition args))

(define (abort condition . args)
  "Signals condition <condition> with the specified arguments. The signal
   is signaled as an error. If the the condition itself is not handled,
   it will be resignaled as an unhandled-abort. If the unhandled-abort is
   not handled, the interpreter will panic."
  (handle-condition condition args
                    (lambda ()
                      (signal 'unhandled-abort condition args)
                      (%panic (format #f "Unhandled Abort: ~s, args=~s" condition args)))))


(define *info* #t)
(define *error* #t)

(define (show-runtime-error message args)
  (when *error*
    (dynamic-let ((*print-readably* #f))
      (if *last-error-stack-trace*
          (show-frames *last-error-stack-trace* (current-error-port))
          (format (current-error-port) "--- NO STACK TRACE ---"))
      (format (current-error-port) "; Error: ~I\n" message args))))


(define *enter-repl-on-runtime-error* #f)

(define errobj #f)

(define (handle-runtime-error message args)
  (handler-bind
      ((runtime-error (lambda (msg-2 args-2)
                        (%panic "Error during default error error handling!"))))
    (show-runtime-error message args)
    (set! errobj (cons message args))
    (when *enter-repl-on-runtime-error*
      (format (current-error-port) "Entering debug REPL\n")
      (handler-bind ((runtime-error handle-runtime-error))
        (repl)))
    (throw 'error-escape)))

(define (ignore-user-break)
  (throw 'ignore-user-break))

(define *last-error-stack-trace* #f)

(define (error message . args)
  (unless *last-error-stack-trace*
    (set! *last-error-stack-trace* (reverse (%get-current-frames 5)))) ; dynamic-let would complicate the stack trace
  (unwind-protect
   (lambda ()
     (catch 'ignore-error
       (abort 'runtime-error message args)))
   (lambda ()
     (set! *last-error-stack-trace* #f))))

(define (ignore-error)
  (throw 'ignore-error))

(define (info message . args)
  (when *info*
    (format (current-error-port) "; Info: ~I\n" message args)))

(define (user-break-handler trapno)
  (catch 'ignore-user-break
    (signal 'user-break)))

(define (uncaught-throw-handler trapno tag retval)
  (abort 'scheme::uncaught-throw tag retval))

(define (trap-signal trapno fsp tag retval)
  (abort tag retval))

(define (trap-wrong-type trapno fsp subr argno errval)
  (if (< argno 0)
      (error "Invalid argument ~a to ~a: ~s" argno subr errval)
      (error "Invalid argument to ~a: ~s" subr errval)))

(define (trap-index-out-of-bounds trapno fsp subr bad-index obj)
  (error "Index ~a out of bounds while accessing ~s with ~s" bad-index obj subr))

(define (trap-arg-out-of-range trapno fsp subr bad-arg range-desc)
  (if (null? range-desc)
      (error "Argument to ~s out of range: ~s"  subr bad-arg)
      (error "Argument to ~s out of range (~a): ~s" subr range-desc bad-arg)))

(define (trap-unimplemented trapno fsp subr desc)
  (error "Operation in ~s not yet implemented: ~a" subr desc))

(define (trap-unsupported trapno fsp subr desc)
  (error "Operation in ~s unsupported: ~a" subr desc))

(define (trap-divide-by-zero trapno fsp subr)
  (error "Divide by zero in ~s" subr))

(define (trap-io-error trapno fsp subr desc info)
  (if (null? info)
      (error "Input/Output error in ~s: ~a" subr desc)
      (error "Input/Output error in ~s: ~a (~s)" subr desc info)))

(define (trap-unbound-global trapno fsp subr var)
  (error "unbound global: ~s" var))

(define (trap-fast-read-error trapno fsp subr desc port location details)
  (error "Error Reading FASL File: ~s @ ~s:~s" desc port location))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_WRONG_TYPE trap-wrong-type)
  (%set-trap-handler! system::TRAP_INDEX_OUT_OF_BOUNDS trap-index-out-of-bounds)
  (%set-trap-handler! system::TRAP_ARG_OUT_OF_RANGE trap-arg-out-of-range)
  (%set-trap-handler! system::TRAP_UNSUPPORTED trap-unsupported)
  (%set-trap-handler! system::TRAP_UNIMPLEMENTED trap-unimplemented)
  (%set-trap-handler! system::TRAP_DIVIDE_BY_ZERO trap-divide-by-zero)
  (%set-trap-handler! system::TRAP_IO_ERROR trap-io-error)
  (%set-trap-handler! system::TRAP_UNBOUND_GLOBAL trap-unbound-global)
  (%set-trap-handler! system::TRAP_FAST_READ_ERROR trap-fast-read-error)

  (%set-trap-handler! system::TRAP_SIGNAL trap-signal)
  (%set-trap-handler! system::TRAP_USER_BREAK user-break-handler)
  (%set-trap-handler! system::TRAP_UNCAUGHT_THROW uncaught-throw-handler))

(define *show-system-frames* #f)

(define *system-stack-boundary* #f)

(defmacro (with-preserved-stack-boundary . code)
  `(dynamic-let ((*system-stack-boundary* *system-stack-boundary*))
     ,@code))

(defmacro (begin-user-stack form)
  `(scheme::%preserve-initial-fsp *system-stack-boundary* ,form))

(define (show-frames frame-list p)
  (define (maybe-remove-system-frames frame-list)
    (if (or *show-system-frames* (not *system-stack-boundary*))
        frame-list
        (take-while #L(> (second _) scheme::*system-stack-boundary*) frame-list)))
  (let ((frame-list (filter (lambda (frame) (= (car frame) system::FRAME_EVAL))
                            (maybe-remove-system-frames frame-list))))
    (let loop ((frames (reverse frame-list))
               (frame-index 0))
      (unless (null? frames)
        (let ((frame (car frames)))
          (format p "~a> ~s\n\n" frame-index (third frame))
          (unless (eq? (third frame) (fourth frame))
            (format p "   -> ~s\n\n" (third frame))))
        (loop (cdr frames) (+ frame-index 1))))))

(defmacro (check predicate value . msg)
  "Applies <predicate> to <value> and signals an error if false. <predicate>
   can either be a symbol naming a predicate, a list describing a composite
   predicate, or a comparison predicate. A composite predicate is specified by
   a list beginning with the symbol and, or, or not; Subsequent elements in the
   list are other predicates to be evaluated.  A relations predicate begins
   with >, >=, <, <=, member, eq?, or equal? and is followed by the value to test
   against. If the predicate specified by the <predicate> expression is
   not satified by <value>, an error is thrown with error text containing both
   <predicate> and <value>. <msg>, if specified, contains clarifying text (and
   arguments to any format codes) that will also be displayed with the error."
  (define (expand-predicate predicate value)
    "Expands the predicate form <predicate> into an expression on <value>."
    (cond ((symbol? predicate)
           `(,predicate ,value))
          ((not (list? predicate))
           (error "Invalid predicate form in check: ~a" predicate))
          ((member (car predicate) '(and or not))
           `(,(car predicate) ,@(map #L(expand-predicate _ value) (cdr predicate))))
          ((member (car predicate) '(> >= < <= = eq? equal? member))
           (unless (length=2? predicate)
             (error "Invalid conditional predicate: ~a" predicate))
           `(,(car predicate) ,value ,(cadr predicate)))
          (#t
           (error "Invalid predicate clause, bad initial symbol: ~a" predicate))))

  (let ((msg-text (if (null? msg) #f (car msg)))
        (msg-args (if (null? msg) () (cdr msg))))

    (with-gensyms (value-sym)
      `(let ((,value-sym ,value))
         `(unless ,(expand-predicate predicate value-sym)
            ,(if msg-text
                 `(error ,#"${msg-text} (Check of ~s failed: ~s. Value is ~s)"
                         ,@msg-args ',value ',predicate ,value-sym)
                 `(error "Check of ~s failed: ~s. Value is ~s"
                         ',value ',predicate ,value-sym)))))))

