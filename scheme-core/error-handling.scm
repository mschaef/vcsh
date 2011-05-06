
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

;;;; Signal handling

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


;;;; Info messaging 

(define *info* #t)

(define (info message . args)
  (when *info*
    (format (current-error-port) "; Info: ~I\n" message args)))

;;;; System Stack Walker

(define (frame-ref frp ofs ref-type)
  (let ((raw-value (%memref (+ frp (* ofs (system-info :size-of-lref))))))
    (case ref-type
      ((:raw) raw-value)
      ((:lref) (%sysob raw-value))
      ((:lref-ptr) (%sysob (%memref raw-value)))
      (#t (error "Bad ref-type in frame-ref: ~s" ref-type)))))

(define (frame-link frp)
  (let ((next-frp (frame-ref frp system::FOFS_LINK :raw)))
    (if (= next-frp 0)
        #f
      next-frp)))

(define (frame-type frp)
  (case (frame-ref frp system::FOFS_FTYPE :raw)
    ((#.system::FRAME_SUBR  ) 'system::FRAME_SUBR  )
    ((#.system::FRAME_EVAL  ) 'system::FRAME_EVAL  )
    ((#.system::FRAME_ESCAPE) 'system::FRAME_ESCAPE)
    ((#.system::FRAME_UNWIND) 'system::FRAME_UNWIND)
    (#t #f)))

(define (frame-decode frp)
  (let ((frame (make-hash :eq)))
    (hash-set! frame :frp frp)
    (let ((ftype (frame-type frp)))
      (hash-set! frame :frame-type ftype)
      (case ftype
        ((system::FRAME_SUBR)
         (hash-set! frame :subr         (frame-ref frp system::FOFS_SUBR_SUBR :lref)))
        ((system::FRAME_EVAL)
         (hash-set! frame :environment  (frame-ref frp system::FOFS_EVAL_ENV :lref))
         (hash-set! frame :initial-form (frame-ref frp system::FOFS_EVAL_IFORM :lref))
         (hash-set! frame :current-form (frame-ref frp system::FOFS_EVAL_FORM_PTR :lref-ptr)))
        ((system::FRAME_UNWIND)
         (hash-set! frame :after-thunk  (frame-ref frp system::FOFS_UNWIND_AFTER :lref)))
        ((system::FRAME_ESCAPE)
         (hash-set! frame :tag          (frame-ref frp system::FOFS_ESCAPE_TAG :lref))
         (hash-set! frame :escape-frp   (frame-ref frp system::FOFS_ESCAPE_FRAME :raw)))))
    frame))

(define (fold-frames kons knil frp :keyword (stop-at-frp? #f))
  (let loop ((knil knil) (frp frp))
    (if (and frp
             (or (not (number? stop-at-frp?))
                 (not (= frp stop-at-frp?))))
        (loop (kons frp knil) (frame-link frp))
        knil)))

(define (fold-decoded-frames kons knil frp :keyword (stop-at-frp? #f))
  (fold-frames (lambda (frp rest)
                 (kons (frame-decode frp) rest))
               knil
               frp
               :stop-at-frp? stop-at-frp?))

(define (capture-stack frp :keyword (stop-at-frp? #f))
  (reverse!
   (fold-decoded-frames (lambda (frame rest)
                          (cons frame rest))
                        ()
                        frp
                        :stop-at-frp? stop-at-frp?)))

;;;; Stack Trace Capture and Display

(define *capture-system-frames* #f)

(define *system-stack-boundary* #f)

(defmacro (with-preserved-stack-boundary . code)
  `(dynamic-let ((*system-stack-boundary* *system-stack-boundary*))
     ,@code))

(defmacro (begin-user-stack form)
  `(scheme::%preserve-initial-frame *system-stack-boundary* ,form))

(define *current-frp* #f)

(define (capture-stack-for-error)
  (dynamic-let ((*current-frp* *current-frp*))
    (scheme::%preserve-initial-frame
     *current-frp*
     (capture-stack *current-frp*))))

(define (show-frames frames op)
  (dolist (frame frames)
    (format op "> ~s\n" frame)))

;;;; Error handling

(define *error* #t)

(define *enter-repl-on-runtime-error* #f)

(define *last-error-stack-trace* #f)

(define (show-runtime-error message args)
  (when *error*
    (dynamic-let ((*print-readably* #f))
      (if *last-error-stack-trace*
          (show-frames *last-error-stack-trace* (current-error-port))
          (format (current-error-port) "--- NO STACK TRACE ---"))
      (format (current-error-port) "; Error: ~I\n" message args))))

(define *last-error* #f)

(define (handle-runtime-error message args)
  (handler-bind
      ((runtime-error (lambda (msg-2 args-2)
                        (%panic "Error during default error error handling!"))))
    (show-runtime-error message args)
    (set! *last-error* (cons message args))
    (when *enter-repl-on-runtime-error*
      (format (current-error-port) "Entering debug REPL\n")
      (handler-bind ((runtime-error handle-runtime-error))
        (repl)))
    (throw 'error-escape)))

(define (ignore-user-break)
  (throw 'ignore-user-break))

(define (error message . args)
  (unless *last-error-stack-trace*
    (set! *last-error-stack-trace* (capture-stack-for-error))) ; dynamic-let would complicate the stack trace
  (unwind-protect
   (lambda ()
     (catch 'ignore-error
       (abort 'runtime-error message args)))
   (lambda ()
     (set! *last-error-stack-trace* #f))))

(define (ignore-error)
  (throw 'ignore-error))

;;;; System trap handlers

(define (trap-user-break trapno fsp)
  (catch 'ignore-user-break
    (signal 'user-break)))

(define (trap-uncaught-throw trapno fsp tag retval)
  (abort 'scheme::uncaught-throw tag retval))

(define (trap-signal trapno frp tag retval)
  (abort tag retval))

(define (trap-wrong-type trapno frp subr argno errval)
  (if (< argno 0)
      (error "Invalid argument ~a to ~a: ~s" argno subr errval)
      (error "Invalid argument to ~a: ~s" subr errval)))

(define (trap-index-out-of-bounds trapno frp subr bad-index obj)
  (error "Index ~a out of bounds while accessing ~s with ~s" bad-index obj subr))

(define (trap-arg-out-of-range trapno frp subr bad-arg range-desc)
  (if (null? range-desc)
      (error "Argument to ~s out of range: ~s"  subr bad-arg)
      (error "Argument to ~s out of range (~a): ~s" subr range-desc bad-arg)))

(define (trap-unimplemented trapno frp subr desc)
  (error "Operation in ~s not yet implemented: ~a" subr desc))

(define (trap-unsupported trapno frp subr desc)
  (error "Operation in ~s unsupported: ~a" subr desc))

(define (trap-divide-by-zero trapno frp subr)
  (error "Divide by zero in ~s" subr))

(define (trap-io-error trapno frp subr desc info)
  (if (null? info)
      (error "Input/Output error in ~s: ~a" subr desc)
      (error "Input/Output error in ~s: ~a (~s)" subr desc info)))

(define (trap-unbound-global trapno frp subr var)
  (error "unbound global: ~s" var))

(define (trap-fast-read-error trapno frp subr desc port location details)
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
  (%set-trap-handler! system::TRAP_USER_BREAK trap-user-break)
  (%set-trap-handler! system::TRAP_UNCAUGHT_THROW trap-uncaught-throw))

;;;; Predicate checking

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
                 ;;; TODO: These explicit error calls should be refactored into a check-error call of some sort
                 `(error ,#"${msg-text} (Check of ~s failed: ~s. Value is ~s)"
                         ,@msg-args ',value ',predicate ,value-sym)
                 `(error "Check of ~s failed: ~s. Value is ~s"
                         ',value ',predicate ,value-sym)))))))

