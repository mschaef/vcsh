;;;; deferred-execution.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; Deferred execution tools. Tools for scheduling code to run in the
;;; future either as time permits or at scheduled times.

(defmacro (%with-disabled-interrupts . code)
  "Runs <code> with interrupts disabled. Interrupts are restored
   to their original state afterwards."
  (with-gensyms (old-mask-sym)
    `(let ((,old-mask-sym (%set-interrupt-mask! #t)))
       (unwind-protect
        (lambda () ,@code)
        (lambda () (%set-interrupt-mask! ,old-mask-sym))))))

(define (%set-timer-event-time time)
  (error "event timer unsupported"))

(define (cancel-scheduled-events)
  "Cancels all currently scheduled events"
  (set! *scheduled-event-list* ())
  (%set-timer-event-time #f))

(define realtime->seconds)

(define (show-scheduled-events)
  "Shows a list of all currently scheduled events"
  (format #t "Currently Scheduled Events:\n--------------------------------\n")
  (let ((now (realtime)))
    (let loop ((remaining *scheduled-event-list*))
      (unless (null? remaining)
        (format #t "T+~a: ~a\n" (realtime->seconds (- (caar remaining) now)) (cdar remaining))
        (loop (cdr remaining))))))

(define *scheduled-event-list* ())

(define (%set-scheduled-event-list! new-list)
  "Sets a new scheduled event list.  A scheduled event list is a list of conses
  (<realtime> . <closure>). No validity checking is performed during the update."
  (set! *scheduled-event-list* new-list)
  (%set-timer-event-time (if (null? new-list) #f (caar new-list)))
  *scheduled-event-list*)

(define *currently-processing-events* #f)

(define (process-scheduled-events)
  "Process all events in the scheduled event list.  All events due or past due
   are evaluated in the current scope."

  ;; REVISIT: The vcalc implementation of timer events works by invoking timer
  ;; thunks in the WM_TIMER callback. This code path is not disabled by disabling
  ;; interrupts. The other way this could be done is by having WM_TIMER set an
  ;; interrupt that the evaluator then detects and uses as a signal to dispatch to
  ;; an ISR.

  (%with-disabled-interrupts
   (dynamic-let ((*currently-processing-events* #t))
     (let ((now (realtime)))
       (let loop ((remaining-events *scheduled-event-list*))
         (cond ((null? remaining-events)
                (%set-scheduled-event-list! ()))
               ((not (pair? remaining-events))
                (%panic "Invalid scheduled event list"))
               ((< (caar remaining-events) now)
                ((cdar remaining-events))
                (loop (cdr remaining-events)))
               (#t
                (%set-scheduled-event-list! remaining-events))))))
   (%schedule-all-deferred-events)))


(define (timer-event-handler)
  (process-scheduled-events))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_TIMER_EVENT timer-event-handler))

;;; !! Remove deferred scheduled events
;;;
;;; This can be done by having process-*-events make its list of pending events
;;; and updating the scheduled event list prior to actually executing the pending
;;; events)

(define *deferred-schedule-events* ())

(define (%schedule-event realtime closure)
  "Schedules <closure> to be called at <realtime>. If events are currently being
   processed, then the schedule add is deferred until the next call to
   %schedule-all-deferred-events."
  (check number? realtime)
  (check closure? closure)
  (if *currently-processing-events*
      (push! (cons realtime closure) *deferred-schedule-events*)
      (%set-scheduled-event-list! (insert-ordered *scheduled-event-list* (cons realtime closure) < car))))

(define (%schedule-all-deferred-events)
  (assert (not *currently-processing-events*))
  (dolist (event *deferred-schedule-events*)
    (%schedule-event (car event) (cdr event)))
  (set! *deferred-schedule-events* ()))

;;; REVISIT: Blocking system calls (I/O, etc.) also block scheduled/idle processing

(define *on-idle-list* ()
  "A list of closures to be evaluated the next time the interpreter is idle.")

(define (at realtime closure) ;; REVISIT; alter to use date object
  "Schedules <closre> to run at time <realtime>. <realtime> can also be :idle,
   in which case <closure> is scheduled to be run at the first time the system
   event loop is idle."
  (cond
   ((eq? realtime :idle)
    (push! closure *on-idle-list*))
   ((number? realtime)
    (%schedule-event realtime closure))
   (#t
    (error "Schedule times must be exact numbers: ~a" realtime))))

(define (in seconds closure) ;; REVISIT: alter to use duration object
  "Schedules <closre> to run in <seconds> seconds. <seconds> can also be :idle,
   in which case <closure> is scheduled to be run at the first time the system
   event loop is idle."
  (cond ((eq? seconds :idle)
         (at :idle closure))
        ((number? seconds)
         (at (+ (realtime) seconds) closure))
        (#t
         (error "Schedule times must be exact numbers: ~a" realtime))))

(defmacro (defer-until-idle . code)
  "<code> will be run the next time the event loop is idle."
  `(at :idle (lambda () ,@code)))

(define (process-idle-list)
  "Processes all current thunks on the idle list."
  (let ((idle-thunks *on-idle-list*))
    (set! *on-idle-list* ())
    (dolist (thunk idle-thunks)
      ;; REVISIT: errors from idle thunks need to be sequestered
      ;; away and reported in another idle process.
      (thunk))))


(define *idle-hook* ())

(define (do-idle-processing)
  (invoke-hook '*idle-hook*))

(eval-when (:load-toplevel :execute)
  (add-hook-function! '*idle-hook* 'process-idle-list))

