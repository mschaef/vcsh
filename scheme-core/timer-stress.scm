; timer-stress.scm
; April 25th, 2006
;
;  A stress test for the timer functionality.

(define-package "timer-stress"
  (:uses "scheme"))

(define *timer-count* 0)

(define (make-timer op endtime)
  (let ((timer-id *timer-count*)
	(planned-duration (+ 1 (random 15.0)))
	(set-time (realtime)))
    (incr! *timer-count*)
    (format op "timer_created, ~a, ~a.\n" timer-id planned-duration)
    (in planned-duration
	(lambda ()
	  (let ((actual-duration (realtime->seconds (- (realtime) set-time ))))
	    (format op "timer_expired, ~a, ~a, ~a\n" timer-id planned-duration actual-duration))
	  (when (< (realtime) (- endtime (seconds->realtime (+ 1.0 15.0))))
	    (make-timer op endtime))))))

(define (go :optional
	    (duration 60)
	    (parallel-timers 10))
  (with-port op (open-output-file "timer-stress-results.csv")
    (let ((endtime (+ (realtime) (seconds->realtime duration))))
      (let loop ((i 0))
	(unless (>= i parallel-timers)
	  (make-timer op endtime)
	  (loop (+ i 1))))
      (format op "--- BEGIN TEST ---\n")
      (while (< (realtime) endtime)
	(+ 1 1))
      (set! *timers-running* #f))
    (format op "--- END TEST ---\n")
    ()))


