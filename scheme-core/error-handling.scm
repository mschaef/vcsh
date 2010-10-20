;;;; error-handling.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;
;; Error/Condition/Signal handling.


(define (valid-signal-handler-list? handler-list)
  (or (null? handler-list)
      (and (pair? handler-list)
           (length=2? (car handler-list))
           (symbol? (caar handler-list))
           (valid-signal-handler-list? (cdr handler-list)))))

(defmacro (with-handler-frames new-frames . code)
  (with-gensyms (old-frames-sym)
    `(let ((,old-frames-sym (%handler-frames)))
       (unwind-protect
        (lambda ()
          (%set-handler-frames ,new-frames)
          ,@code)
        (lambda ()
          (%set-handler-frames ,old-frames-sym))))))

(defmacro (handler-bind handler-forms . code) ; REVISIT: Allow for implicit lambdas in handler frames
  "Executes <code> with a handler frame established with the signal handlers
   defined in <handler-forms>. <handler-forms> is a list with elements of the
   formm (<condition-symbol> <handler-form>)."
  (define (handler-forms->frame-expression handler-forms)
    `(cons (list ,@(map (lambda (handler-form)
                          `(list ',(car handler-form)
                                 (capture-global-environment ,(cadr handler-form))))
                        handler-forms))
           (%handler-frames)))
  (unless (valid-signal-handler-list? handler-forms)
    (error "Invalid signal hander list: ~a" handler-forms))
  `(with-handler-frames ,(handler-forms->frame-expression handler-forms)
                        ,@code))

(define (handle-condition condition args :optional (last-resort-handler #f))
  (unless (symbol? condition)
    (%panic (format #f "Condition ~s is not a symbol" condition)))
  (let loop ((remaining-handler-frames (%handler-frames))
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
  (table (%handler-frames) #f (current-error-port)))

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

(define (vm-runtime-error-handler trapno message primitive new-error-object)
  (set! *last-error-stack-trace* (reverse (%get-current-frames 6))) ; dynamic-let would complicate the stack trace
  (error (format #f "in ~a, ~S" (procedure-name primitive) message new-error-object) new-error-object))


(define (user-break-handler trapno)
  (catch 'ignore-user-break
    (signal 'user-break)))

(define (uncaught-throw-handler trapno tag retval)
  (abort 'scheme::uncaught-throw tag retval))

(define (trap-signal trapno tag retval)
  (abort tag retval))

(define (trap-vmerror-wrong-type trapno subr argno errval)
  (if (< argno 0)
      (error "Invalid argument ~a to ~a: ~s" argno (%subr-name subr) errval)
      (error "Invalid argument to ~a: ~s" (%subr-name subr) errval)))

(define (trap-vmerror-index-out-of-bounds trapno bad-index obj)
  (error "Index ~a out of bounds while accessing ~s" bad-index obj))

(define (trap-vmerror-arg-out-of-range trapno bad-arg range-desc)
  (if (null? range-desc)
      (error "Argument out of range: ~s" bad-arg)
      (error "Argument out of range ~a: ~s" range-desc bad-arg)))

(define (trap-vmerror-unimplemented trapno desc)
  (error "Operation not yet implemented: ~a" desc))

(define (trap-vmerror-unsupported trapno desc)
  (error "Operation unsupported: ~a" desc))

(define (trap-vmerror-divide-by-zero trapno)
  (error "Divide by zero"))

(define (trap-vmerror-io-error trapno desc info)
  (if (null? info)
      (error "Input/Output error: ~a" desc)
      (error "Input/Output error: ~a (~s)" desc info)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_RUNTIME_ERROR vm-runtime-error-handler)

  (%set-trap-handler! system::TRAP_VMERROR_WRONG_TYPE trap-vmerror-wrong-type)
  (%set-trap-handler! system::TRAP_VMERROR_INDEX_OUT_OF_BOUNDS trap-vmerror-index-out-of-bounds)
  (%set-trap-handler! system::TRAP_VMERROR_ARG_OUT_OF_RANGE trap-vmerror-arg-out-of-range)
  (%set-trap-handler! system::TRAP_VMERROR_UNSUPPORTED trap-vmerror-unsupported)
  (%set-trap-handler! system::TRAP_VMERROR_UNIMPLEMENTED trap-vmerror-unimplemented)
  (%set-trap-handler! system::TRAP_VMERROR_DIVIDE_BY_ZERO trap-vmerror-divide-by-zero)
  (%set-trap-handler! system::TRAP_VMERROR_IO_ERROR trap-vmerror-io-error)

  (%set-trap-handler! system::TRAP_SIGNAL trap-signal)
  (%set-trap-handler! system::TRAP_USER_BREAK user-break-handler)
  (%set-trap-handler! system::TRAP_UNCAUGHT_THROW uncaught-throw-handler))

(define *show-system-frames* #f)

(define (show-frames frame-list p)
  (define (maybe-remove-system-frames frame-list)
    (if *show-system-frames*
        frame-list
        (take-while #L(not (and (= (car _) system::FRAME_MARKER)
                                (eq? (cadr _) 'system-stack-boundary)))
                    frame-list)))
  (let ((frame-list (filter (lambda (frame) (= (car frame) system::FRAME_EVAL))
                            (maybe-remove-system-frames frame-list))))
    (doiterate ((list frame (reverse frame-list))
                (count frame-index))
      (format p "~a> ~s\n\n" frame-index (third frame))
      (unless (eq? (second frame) (third frame))
        (format p "   -> ~s\n\n" (second frame))))))

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

