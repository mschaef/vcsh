
;;;; date-time.scm --
;;;;
;;;; SRFI-19: Time Data Types and Procedures.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;; (C) Portions Copyright I/NET, Inc. (2000, 2002, 2003). All Rights Reserved.
;;;;
;;;; This document and translations of it may be copied and furnished to others,
;;;; and derivative works that comment on or otherwise explain it or assist in its
;;;; implementation may be prepared, copied, published and distributed, in whole or
;;;; in part, without restriction of any kind, provided that the above copyright
;;;; notice and this paragraph are included on all such copies and derivative works.
;;;; However, this document itself may not be modified in any way, such as by
;;;; removing the copyright notice or references to the Scheme Request For
;;;; Implementation process or editors, except as needed for the purpose of
;;;; developing SRFIs in which case the procedures for copyrights defined in the SRFI
;;;; process must be followed, or as required to translate it into languages other
;;;; than English.
;;;;
;;;; The limited permissions granted above are perpetual and will not be revoked
;;;; by the authors or their successors or assigns.
;;;;
;;;; This document and the information contained herein is provided on an "AS IS"
;;;; basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL WARRANTIES, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTY THAT THE USE OF THE
;;;; INFORMATION HEREIN WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

;; example of extension (MZScheme specific)
(define time-gc 'time-gc) ;; SRFI-19

;;-- LOCALE dependent constants

(define-text
  tm%locale-abbr-weekday-0 "Sun"
  tm%locale-abbr-weekday-1 "Mon"
  tm%locale-abbr-weekday-2 "Tue"
  tm%locale-abbr-weekday-3 "Wed"
  tm%locale-abbr-weekday-4 "Thu"
  tm%locale-abbr-weekday-5 "Fri"
  tm%locale-abbr-weekday-6 "Sat"

  tm%locale-long-weekday-0 "Sunday"
  tm%locale-long-weekday-1 "Monday"
  tm%locale-long-weekday-2 "Tuesday"
  tm%locale-long-weekday-3 "Wednesday"
  tm%locale-long-weekday-4 "Thursday"
  tm%locale-long-weekday-5 "Friday"
  tm%locale-long-weekday-6 "Saturday"
  )

(define tm%locale-abbr-weekday-vector
  (vector 'tm%locale-abbr-weekday-0
          'tm%locale-abbr-weekday-1
          'tm%locale-abbr-weekday-2
          'tm%locale-abbr-weekday-3
          'tm%locale-abbr-weekday-4
          'tm%locale-abbr-weekday-5
          'tm%locale-abbr-weekday-6))

(define tm%locale-long-weekday-vector
  (vector 'tm%locale-long-weekday-0
          'tm%locale-long-weekday-1
          'tm%locale-long-weekday-2
          'tm%locale-long-weekday-3
          'tm%locale-long-weekday-4
          'tm%locale-long-weekday-5
          'tm%locale-long-weekday-6))

(define-text
  tm%locale-abbr-month-1 "Jan"
  tm%locale-abbr-month-2 "Feb"
  tm%locale-abbr-month-3 "Mar"
  tm%locale-abbr-month-4 "Apr"
  tm%locale-abbr-month-5 "May"
  tm%locale-abbr-month-6 "Jun"
  tm%locale-abbr-month-7 "Jul"
  tm%locale-abbr-month-8 "Aug"
  tm%locale-abbr-month-9 "Sep"
  tm%locale-abbr-month-10 "Oct"
  tm%locale-abbr-month-11 "Nov"
  tm%locale-abbr-month-12 "Dec"

  tm%locale-long-month-1 "January"
  tm%locale-long-month-2 "February"
  tm%locale-long-month-3 "March"
  tm%locale-long-month-4 "April"
  tm%locale-long-month-5 "May"
  tm%locale-long-month-6 "June"
  tm%locale-long-month-7 "July"
  tm%locale-long-month-8 "August"
  tm%locale-long-month-9 "September"
  tm%locale-long-month-10 "October"
  tm%locale-long-month-11 "November"
  tm%locale-long-month-12 "December"
  )

;; note empty string in 0th place.
(define tm%locale-abbr-month-vector
  (vector ""
          'tm%locale-abbr-month-1
          'tm%locale-abbr-month-2
          'tm%locale-abbr-month-3
          'tm%locale-abbr-month-4
          'tm%locale-abbr-month-5
          'tm%locale-abbr-month-6
          'tm%locale-abbr-month-7
          'tm%locale-abbr-month-8
          'tm%locale-abbr-month-9
          'tm%locale-abbr-month-10
          'tm%locale-abbr-month-11
          'tm%locale-abbr-month-12))


(define tm%locale-long-month-vector
  (vector ""
          'tm%locale-long-month-1
          'tm%locale-long-month-2
          'tm%locale-long-month-3
          'tm%locale-long-month-4
          'tm%locale-long-month-5
          'tm%locale-long-month-6
          'tm%locale-long-month-7
          'tm%locale-long-month-8
          'tm%locale-long-month-9
          'tm%locale-long-month-10
          'tm%locale-long-month-11
          'tm%locale-long-month-12))

;; See date->string
(define-text
  tm%locale-pm                "PM"                      ;; SRFI-19
  tm%locale-am                "AM"                      ;; SRFI-19
  tm%locale-number-separator  "."                       ;; SRFI-19
  tm%locale-date-time-format   "~a ~b ~d ~H:~M:~S~z ~Y" ;; SRFI-19
  tm%locale-short-date-format  "~m/~d/~y"               ;; SRFI-19
  tm%locale-time-format        "~H:~M:~S"               ;; SRFI-19
  tm%iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")   ;; SRFI-19

;;-- Miscellaneous Constants.
;;-- only the tm%tai-epoch-in-jd might need changing if
;;   a different epoch is used.

(define tm%nano 1000000000) ;; SRFI-19
(define tm%nano/msec 1000000)
(define tm%sid  86400)    ; seconds in a day ;; SRFI-19
(define tm%sihd 43200)    ; seconds in a half day ;; SRFI-19
(define tm%tai-epoch-in-jd (/ 4881175 2)) ; julian day number for 'the epoch' ;; SRFI-19


;;; A Very simple Error system for the time procedures
;;;
(define tm%time-error-types ;; SRFI-19
  '(invalid-clock-type
    unsupported-clock-type
    incompatible-time-types
    not-duration
    dates-are-immutable
    bad-date-format-string
    bad-date-template-string
    invalid-month-specification
    ))

(define (tm%time-error caller type :optional (value :no-value)) ;; SRFI-19
  (if (member type tm%time-error-types)
      (if (not (eq? value :no-value))
	  (error "TIME-ERROR type ~s in ~s: ~s" type caller value)
	  (error "TIME-ERROR type ~s in ~s" type caller))
      (error "TIME-ERROR in ~s unsupported error type ~s" caller type)))


;; each entry is ( utc seconds since epoch . # seconds to add for tai )
;; note they go higher to lower, and end in 1972.
(define tm%leap-second-table ;; SRFI-19
 '((1136073600 . 33)
  (915148800 . 32)
  (867715200 . 31)
  (820454400 . 30)
  (773020800 . 29)
  (741484800 . 28)
  (709948800 . 27)
  (662688000 . 26)
  (631152000 . 25)
  (567993600 . 24)
  (489024000 . 23)
  (425865600 . 22)
  (394329600 . 21)
  (362793600 . 20)
  (315532800 . 19)
  (283996800 . 18)
  (252460800 . 17)
  (220924800 . 16)
  (189302400 . 15)
  (157766400 . 14)
  (126230400 . 13)
  (94694400 . 12)
  (78796800 . 11)
  (63072000 . 10)))


(define (tm%leap-second-delta utc-seconds) ;; SRFI-19
  (letrec ((lsd (lambda (table)
                  (cond
                   ((>= utc-seconds (caar table))
                    (cdar table))
                   (#t (lsd (cdr table)))))) )
    (if (< utc-seconds  (* (- 1972 1970) 365 tm%sid))
        0
        (lsd tm%leap-second-table))))

;; going from tai seconds to utc seconds ...
(define (tm%leap-second-neg-delta tai-seconds) ;; SRFI-19
  (letrec ((lsd (lambda (table)
                  (cond ((null? table) 0)
                        ((<= (cdar table) (- tai-seconds (caar table)))
                         (cdar table))
                        (#t (lsd (cdr table)))))) )
    (if (< tai-seconds  (* (- 1972 1970) 365 tm%sid))
        0
        (lsd tm%leap-second-table))))


;;; the time structure; creates the accessors, too.
;;; wf: changed to match srfi documentation. uses mzscheme structures & inspectors

(define (make-time args)
  (hash-merge! {'type-of 'time} args))

(define (time? obj)
  (eq? (type-of obj) 'time))

(define (time-of-type? obj expected-type)
  (and (time? obj)
       (eq? (:type obj) expected-type)))

;;; current-time

;;; specific time getters.
;;; these should be rewritten to be os specific.
;;
;; -- using gnu gettimeofday() would be useful here -- gets
;;    second + millisecond
;;    let's pretend we do, using mzscheme's current-seconds & current-milliseconds
;;    this is supposed to return utc.
;;

(define (tm%parse-realtime seconds) ;; SRFI-19
  "Split a number of <seconds> into two values: an exact number of
   seconds and an exact number of nanoseconds."
  (let ((t seconds))
    (values
     (inexact->exact (floor t)) ; sec
     (inexact->exact (floor (* tm%nano (- t (floor t))))) ; nsec
     )))

(define (tm%realtime->time-utc rt)
   (mvbind (seconds nsec) (tm%parse-realtime rt)
	   (make-time {:type       :time-utc
                   :nanosecond nsec
                   :second     seconds})))

(define (tm%current-time-utc) ;; SRFI-19
  (tm%realtime->time-utc (realtime)))

(define (tm%realtime->time-tai rt) ;; SRFI-19
  (mvbind (seconds nsec)  (tm%parse-realtime rt)
	   (make-time {:type       :time-tai
                   :nanosecond nsec
                   :second     (+ seconds (tm%leap-second-delta seconds))})))

(define (tm%current-time-tai) ;; SRFI-19
  (tm%realtime->time-tai (realtime)))

(define (tm%current-time-ms-time time-type current-ms) ;; SRFI-19
  (make-time {:type       time-type
              :nanosecond (* (remainder current-ms 1000) tm%nano/msec)
              :second     (quotient current-ms 1000)}))

(define (tm%current-time-monotonic) ;; SRFI-19
  ;; -- we define it to be the same as tai.
  ;;    a different implemation of current-time-montonic
  ;;    will require rewriting all of the time-monotonic converters,
  ;;    of course.
  (tm%current-time-tai))

(define (tm%current-time-process) ;; SRFI-19
  (tm%current-time-ms-time :time-process (* 1000 (runtime))))

(define (tm%current-time-gc) ;; SRFI-19
  (tm%current-time-ms-time :time-gc (* 1000 (gc-runtime))))

(define (current-time :optional (clock-type :time-utc)) ;; SRFI-19
  (case clock-type
   ((:time-tai)       (tm%current-time-tai))
   ((:time-utc)       (tm%current-time-utc))
   ((:time-monotonic) (tm%current-time-monotonic))
   ((:time-process)   (tm%current-time-process))
   ((:time-gc)        (tm%current-time-gc))
   (#t (tm%time-error 'current-time 'invalid-clock-type clock-type))))

(define (time-resolution :optional (clock-type :time-utc)) ;; SRFI-19
  (case clock-type
   ((:time-tai
     :time-utc
     :time-monotonic
     :time-process
     :time-gc)
    (system-info :runtime-resolution))
   (#t (tm%time-error 'time-resolution 'invalid-clock-type clock-type))))

;; -- time comparisons

(define (tm%compatible-times? time1 time2) ;; SRFI-19
  (and (time? time1)
       (time? time2)
       (eq? (:type time1) (:type time2))))

(define (tm%time-compare-check time1 time2 caller) ;; SRFI-19
  (unless (tm%compatible-times? time1 time2)
    (tm%time-error caller 'incompatible-time-types (cons time1 time2))))

(define (time=? time1 time2) ;; SRFI-19
  (tm%time-compare-check time1 time2 'time=?)
  (and (= (:second time1) (:second time2))
       (= (:nanosecond time1) (:nanosecond time2))))

(define (time>? time1 time2) ;; SRFI-19
  (tm%time-compare-check time1 time2 'time>?)
  (or (> (:second time1) (:second time2))
      (and (= (:second time1) (:second time2))
	   (> (:nanosecond time1) (:nanosecond time2)))))

(define (time<? time1 time2) ;; SRFI-19
  (tm%time-compare-check time1 time2 'time<?)
  (or (< (:second time1) (:second time2))
      (and (= (:second time1) (:second time2))
	   (< (:nanosecond time1) (:nanosecond time2)))))

(define (time>=? time1 time2) ;; SRFI-19
  (tm%time-compare-check time1 time2 'time>=?)
  (or (>= (:second time1) (:second time2))
      (and (= (:second time1) (:second time2))
	   (>= (:nanosecond time1) (:nanosecond time2)))))

(define (time<=? time1 time2) ;; SRFI-19
  (tm%time-compare-check time1 time2 'time<=?)
  (or (<= (:second time1) (:second time2))
      (and (= (:second time1) (:second time2))
	   (<= (:nanosecond time1) (:nanosecond time2)))))

;; -- time arithmetic

(define (tm%time->nanoseconds time) ;; SRFI-19
  (define (sign1 n) ;; SRFI-19
    (if (negative? n) -1 1))
  (+ (* (:second time) tm%nano)
     (:nanosecond time)))

(define (tm%nanoseconds->time time-type nanoseconds) ;; SRFI-19
  (make-time {:type       time-type
              :nanosecond (remainder nanoseconds tm%nano)
              :second     (quotient nanoseconds tm%nano)}))

(define (tm%nanoseconds->values nanoseconds) ;; SRFI-19
  (values (abs (remainder nanoseconds tm%nano))
          (quotient nanoseconds tm%nano)))

(define (tm%time-difference time1 time2 result-time) ;; SRFI-19
  (unless (tm%compatible-times? time1 time2)
     (tm%time-error 'time-difference 'incompatible-time-types))
  (hash-set! result-time :type :time-duration)
  (if (time=? time1 time2)
      (-> result-time
          (hash-set! :second 0)
          (hash-set! :nanosecond 0))
      (mvbind (nanos secs)
          (tm%nanoseconds->values (- (tm%time->nanoseconds time1)
                                     (tm%time->nanoseconds time2)))
        (-> result-time
            (hash-set! :second secs)
            (hash-set! :nanosecond nanos))))
  result-time)

(define (time-difference time1 time2) ;; SRFI-19
  (tm%time-difference time1 time2 (make-time {})))

(define (time-difference! time1 time2) ;; SRFI-19
  (tm%time-difference time1 time2 time1))

(define (tm%add-duration time1 duration time3) ;; SRFI-19
  (unless (and (time? time1) (time? duration))
      (tm%time-error 'add-duration 'incompatible-time-types))
  (if (not (time-of-type? duration :time-duration))
      (tm%time-error 'add-duration 'not-duration duration)
      (let ((sec-plus (+ (:second time1) (:second duration)))
            (nsec-plus (+ (:nanosecond time1) (:nanosecond duration))) )
        (let ((r (remainder nsec-plus tm%nano))
              (q (quotient nsec-plus tm%nano)))
          ;; (set-time-type! time3 (time-type time1))
          (if (negative? r)
              (-> time3
                  (hash-set! :second (+ sec-plus q -1))
                  (hash-set! :nanosecond (+ tm%nano r)))
              (-> time3
                  (hash-set! :second (+ sec-plus q))
                  (hash-set! :nanosecond r)))
          time3))))

(define (add-duration time1 duration) ;; SRFI-19
  (tm%add-duration time1 duration
                   (make-time {:type (time-type time1)})))

(define (add-duration! time1 duration) ;; SRFI-19
  (tm%add-duration time1 duration time1))

(define (tm%subtract-duration time1 duration time3) ;; SRFI-19
  (unless (and (time? time1) (time? duration))
      (tm%time-error 'add-duration 'incompatible-time-types))
  (if (not (time-of-type? duration :time-duration))
      (tm%time-error 'tm%subtract-duration 'not-duration duration)
      (let ((sec-minus  (- (:second time1) (:second duration)))
            (nsec-minus (- (:nanosecond time1) (:nanosecond duration))))
	(let ((r (remainder nsec-minus tm%nano))
	      (q (quotient nsec-minus tm%nano)))
	  (if (negative? r)
          (-> time3
              (hash-set! :second (- sec-minus q 1))
              (hash-set! :nanosecond (+ tm%nano r)))
          (-> time3
              (hash-set! :second (- sec-minus q))
              (hash-set! :nanosecond r)))
	  time3))))

(define (subtract-duration time1 duration) ;; SRFI-19
  (tm%subtract-duration time1 duration (make-time {:type (:type time1)})))

(define (subtract-duration! time1 duration) ;; SRFI-19
  (tm%subtract-duration time1 duration time1))

;; -- converters between types.

(define (tm%time-tai->time-utc! time-in time-out caller) ;; SRFI-19
  (unless (time-of-type? time-in :time-tai)
    (tm%time-error caller 'incompatible-time-types time-in))
  (-> time-out
      (hash-set! :type :time-utc)
      (hash-set! :second (- (:second time-in)
                            (tm%leap-second-neg-delta
                             (:second time-in))))
      (hash-set! :nanosecond (:nanosecond time-in) ))
  time-out)

(define (time-tai->time-utc time-in) ;; SRFI-19
  (tm%time-tai->time-utc! time-in (make-time {}) 'time-tai->time-utc))

(define (time-tai->time-utc! time-in) ;; SRFI-19
  (tm%time-tai->time-utc! time-in time-in 'time-tai->time-utc!))

(define (tm%time-utc->time-tai! time-in time-out caller) ;; SRFI-19
  (unless (time-of-type? time-in :time-utc)
    (tm%time-error caller 'incompatible-time-types time-in))
  (-> time-out
      (hash-set! :time :time-tai)
      (hash-set! :second (+ (:second time-in)
                            (tm%leap-second-delta
                             (:second time-in))))
      (hash-set! :nanosecond (:nanosecond time-in)))
  time-out)

(define (time-utc->time-tai time-in) ;; SRFI-19
  (tm%time-utc->time-tai! time-in (make-time {}) 'time-utc->time-tai))

(define (time-utc->time-tai! time-in) ;; SRFI-19
  (tm%time-utc->time-tai! time-in time-in 'time-utc->time-tai!))

;; -- these depend on time-monotonic having the same definition as time-tai!
(define (time-monotonic->time-utc time-in) ;; SRFI-19
  (unless (time-of-type? time-in :time-monotonic)
      (tm%time-error 'time-monotoinc->time-utc 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (hash-set! ntime :type :time-tai)
    (tm%time-tai->time-utc! ntime ntime 'time-monotonic->time-utc)))

(define (time-monotonic->time-utc! time-in) ;; SRFI-19
  (unless (time-of-type? time-in :time-monotonic)
    (tm%time-error 'time-monotonic->time-utc! 'incompatible-time-types time-in))
  (hash-set! time-in :type :time-tai)
  (tm%time-tai->time-utc! time-in time-in 'time-monotonic->time-utc))

(define (time-monotonic->time-tai time-in) ;; SRFI-19
  (unless (time-of-type? time-in :time-monotonic)
      (tm%time-error 'time-monotonic->time-tai 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (hash-set! ntime :type :time-tai)
    ntime))

(define (time-monotonic->time-tai! time-in) ;; SRFI-19
  (unless (eq? (time-type time-in) :time-monotonic)
    (tm%time-error 'time-monotonic->time-tai! 'incompatible-time-types time-in))
  (hash-set! time-in :type :time-tai)
  time-in)

(define (time-utc->time-monotonic time-in) ;; SRFI-19
  (unless (time-of-type? time-in :time-utc)
      (tm%time-error 'time-utc->time-monotonic 'incompatible-time-types time-in))
  (let ((ntime (tm%time-utc->time-tai! time-in (make-time {})
                                       'time-utc->time-monotonic)))
    (hash-set! ntime :type :time-monotonic)
    ntime))

(define (time-utc->time-monotonic! time-in) ;; SRFI-19
  (unless (time-of-type? time-in :time-utc)
      (tm%time-error 'time-utc->time-montonic! 'incompatible-time-types time-in))
  (let ((ntime (tm%time-utc->time-tai! time-in time-in
                                       'time-utc->time-monotonic!)))
    (hash-set! ntime :type :time-monotonic)
    ntime))

(define (time-tai->time-monotonic time-in) ;; SRFI-19
  (unless (time-of-type? time-in :time-tai)
      (tm%time-error 'time-tai->time-monotonic 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (hash-set! ntime :type :time-monotonic)
    ntime))

(define (time-tai->time-monotonic! time-in) ;; SRFI-19
  (unless (time-of-type? time-in :time-tai)
    (tm%time-error 'time-tai->time-monotonic!  'incompatible-time-types time-in))
  (hash-set! time-in :type :time-monotonic)
  time-in)

;; -- date structures

(define (make-date args)
  (hash-merge! {'type-of 'date} args))

;; gives the julian day which starts at noon.
(define (tm%encode-julian-day-number day month year) ;; SRFI-19
  (let* ((a (quotient (- 14 month) 12))
         (y (- (- (+ year 4800) a) (if (negative? year) -1 0)))
         (m (- (+ month (* 12 a)) 3)))
    (+ day
       (quotient (+ (* 153 m) 2) 5)
       (* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       -32045)))

(define (tm%char-pos char str index len) ;; SRFI-19
  (cond
   ((>= index len) #f)
   ((char=? (string-ref str index) char)
    index)
   (#t
    (tm%char-pos char str (+ index 1) len))))


(define (tm%fractional-part r) ;; SRFI-19
  (if (integer? r)
      "0"
      (let ((str (number->string (exact->inexact r))))
        (let ((ppos (tm%char-pos #\. str 0 (string-length str))))
          (substring str (+ ppos 1) (string-length str))))))

;; gives the seconds/date/month/year
(define (tm%decode-julian-day-number jdn) ;; SRFI-19
  (let* ((days (truncate jdn))
         (a (+ days 32044))
         (b (quotient (+ (* 4 a) 3) 146097))
         (c (- a (quotient (* 146097 b) 4)))
         (d (quotient (+ (* 4 c) 3) 1461))
         (e (- c (quotient (* 1461 d) 4)))
         (m (quotient (+ (* 5 e) 2) 153))
         (y (+ (* 100 b) d -4800 (quotient m 10))))
    (values ; seconds date month year
     (* (- jdn days) tm%sid)
     (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
     (+ m 3 (* -12 (quotient m 10)))
     (if (>= 0 y) (- y 1) y))))

(define (tm%local-tz-offset) ;; SRFI-19
  (inexact->exact (- (scheme::realtime-time-zone-offset))))

;; special thing -- ignores nanos
(define (tm%time->julian-day-number seconds tz-offset) ;; SRFI-19
  (+ (/ (+ seconds
           tz-offset
           tm%sihd)
        tm%sid)
     tm%tai-epoch-in-jd))

(define (tm%tai-before-leap-second? second) ;; SRFI-19
  (find (lambda (x)
	  (= second (- (+ (car x) (cdr x)) 1)))
	tm%leap-second-table))

(define (tm%time->date time :optional (tz-offset (tm%local-tz-offset)) (ttype :time-utc)) ;; SRFI-19
  (unless (time-of-type? time ttype)
    (tm%time-error 'time->date 'incompatible-time-types  time))
  (mvbind (secs date month year)
           (tm%decode-julian-day-number
            (tm%time->julian-day-number (:second time) tz-offset))
           (let* ((hours    (quotient secs (* 60 60)))
                  (rem      (remainder secs (* 60 60)))
                  (minutes  (quotient rem 60))
                  (seconds  (remainder rem 60)) )
             (make-date {:nanosecond  (:nanosecond time)
                         :second      (inexact->exact seconds)
                         :minute      (inexact->exact minutes)
                         :hour        (inexact->exact hours)
                         :day         (inexact->exact date)
                         :month       (inexact->exact month)
                         :year        (inexact->exact year)
                         :zone-offset tz-offset}))))

(define (duration :keyword (days 0) (hours 0) (minutes 0) (seconds 0))
  (runtime-check exact? days)
  (runtime-check exact? hours)
  (runtime-check exact? minutes)
  (runtime-check real? seconds)
  (mvbind (sec nsec) (tm%parse-realtime (+ seconds
                                            (* 60 (+ minutes
                                                     (* 60 (+ hours
                                                              (* 24 days)))))))
    (make-time {:type       :time-duration ;; REVISIT: should durations be a separate type?
                :nanosecond nsec
                :second     sec})))


(define (time-tai->date time . tz-offset) ;; SRFI-19
  (if (tm%tai-before-leap-second? (:second time))
      ;; if it's *right* before the leap, we need to pretend to subtract a second ...
      (let ((d (tm%time->date (subtract-duration! (time-tai->time-utc time)
                                                  (make-time {:type       :time-duration
                                                              :nanosecond 0
                                                              :second     1}))
                              tz-offset
                              :time-utc)))
        (hash-set! d :second 60)
        d)
      (tm%time->date (time-tai->time-utc time) tz-offset :time-utc)))

(define (time-utc->date time :optional (tz-offset 0)) ;; SRFI-19
  (tm%time->date time tz-offset :time-utc))

;; again, time-monotonic is the same as time tai
(define (time-monotonic->date time  :optional (tz-offset 0)) ;; SRFI-19
  (tm%time->date time tz-offset :time-monotonic))

(define (date->time-utc date) ;; SRFI-19
  (let ((nanosecond (:nanosecond date))
        (second (:second date))
        (minute (:minute date))
        (hour (:hour date))
        (day (:day date))
        (month (:month date))
        (year (:year date))
        (offset (:zone-offset date)))
    (let ((jdays (- (tm%encode-julian-day-number day month year)
                    tm%tai-epoch-in-jd)))
      (make-time {:type       :time-utc
                  :nanosecond nanosecond
                  :second     (+ (* (- jdays (/ 1 2)) 24 60 60)
                                 (* hour 60 60)
                                 (* minute 60)
                                 second
                                 (- offset))}))))

(define (date->time-tai d) ;; SRFI-19
  (if (= (date-second d) 60)
      (subtract-duration! (time-utc->time-tai! (date->time-utc d))
                          (make-time {:type       :time-duration
                                      :nanosecond 0
                                      :second     1}))
      (time-utc->time-tai! (date->time-utc d))))

(define (date->time-monotonic date) ;; SRFI-19
  (time-utc->time-monotonic! (date->time-utc date)))

(define (tm%leap-year? year) ;; SRFI-19
  (or (= (modulo year 400) 0)
      (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))

(define (leap-year? date) ;; SRFI-19
  (tm%leap-year? (:year date)))

;; tm%year-day fixed: adding wrong number of days.
(define  tm%month-assoc '((0 . 0) (1 . 31)  (2 . 59)   (3 . 90)   (4 . 120)  ;; SRFI-19
                          (5 . 151) (6 . 181)  (7 . 212)  (8 . 243)
                          (9 . 273) (10 . 304) (11 . 334)))

(define (tm%year-day day month year) ;; SRFI-19
  (let ((days-pr (assoc (- month 1) tm%month-assoc)))
    (if (not days-pr)
        (tm%time-error 'date-year-day 'invalid-month-specification month))
    (if (and (tm%leap-year? year) (> month 2))
        (+ day (cdr days-pr) 1)
        (+ day (cdr days-pr)))))

(define (date-year-day date) ;; SRFI-19
  (tm%year-day (:day date) (:month date) (:year date)))

;; from calendar faq
(define (tm%week-day day month year) ;; SRFI-19
  (let* ((a (quotient (- 14 month) 12))
         (y (- year a))
         (m (+ month (* 12 a) -2)))
    (modulo (+ day y (quotient y 4) (- (quotient y 100))
               (quotient y 400) (quotient (* 31 m) 12))
            7)))

(define (date-week-day date) ;; SRFI-19
  (tm%week-day (:day date) (:month date) (:year date)))

(define (tm%days-before-first-week date day-of-week-starting-week) ;; SRFI-19
  (let* ((first-day (make-date {:nanosecond  0
                                :second      0
                                :minute      0
                                :hour        0
                                :day         1
                                :month       1
                                :year        (:year date)
                                :zone-offset #f}))
         (fdweek-day (date-week-day first-day)))
    (modulo (- day-of-week-starting-week fdweek-day) 7)))

(define (date-week-number date day-of-week-starting-week) ;; SRFI-19
  (quotient (- (date-year-day date)
               (tm%days-before-first-week  date day-of-week-starting-week))
            7))

(define (current-date :optional (tz-offset (tm%local-tz-offset))) ;; SRFI-19
  (time-utc->date (current-time :time-utc) tz-offset))

(define (tm%realtime->date rt :optional (tz-offset (tm%local-tz-offset)))
  (time-utc->date (tm%realtime->time-utc rt) tz-offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; given a 'two digit' number, find the year within 50 years +/-
(define (tm%natural-year n) ;; SRFI-19
  (let* ((current-year (:year (current-date)))
         (current-century (* (quotient current-year 100) 100)) )
    (cond
     ((>= n 100) n)
     ((<  n 0) n)
     ((<=  (- (+ current-century n) current-year) 50)
      (+ current-century n))
     (#t
      (+ (- current-century 100) n)))))

(define (date->julian-day date) ;; SRFI-19
  (let ((nanosecond (:nanosecond date))
        (second (:second date))
        (minute (:minute date))
        (hour (:hour date))
        (day (:day date))
        (month (:month date))
        (year (:year date))
        (offset (:zone-offset date)) )
    (+ (tm%encode-julian-day-number day month year)
       (- (/ 1 2))
       (+ (/ (/ (+ (* hour 60 60)
                   (* minute 60) second (/ nanosecond tm%nano)) tm%sid)
             (- offset))))))

(define (date->modified-julian-day date) ;; SRFI-19
  (- (date->julian-day date)
     (/ 4800001 2)))

(define (time-utc->julian-day time) ;; SRFI-19
  (if (not (time-of-type? time :time-utc))
      (tm%time-error 'time-utc->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (:second time) (/ (:nanosecond time) tm%nano))
        tm%sid)
     tm%tai-epoch-in-jd))

(define (time-utc->modified-julian-day time) ;; SRFI-19
  (- (time-utc->julian-day time)
     (/ 4800001 2)))

(define (time-tai->julian-day time) ;; SRFI-19
  (if (not (time-of-type? time :time-tai))
      (tm%time-error 'time-tai->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (- (:second time)
              (tm%leap-second-delta (:second time)))
           (/ (:nanosecond time) tm%nano))
        tm%sid)
     tm%tai-epoch-in-jd))

(define (time-tai->modified-julian-day time) ;; SRFI-19
  (- (time-tai->julian-day time)
     (/ 4800001 2)))

;; this is the same as time-tai->julian-day
(define (time-monotonic->julian-day time) ;; SRFI-19
  (if (not (time-of-type? time :time-monotonic))
      (tm%time-error 'time-monotonic->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (- (:second time)
              (tm%leap-second-delta (:second time)))
           (/ (:nanosecond time) tm%nano))
        tm%sid)
     tm%tai-epoch-in-jd))


(define (time-monotonic->modified-julian-day time) ;; SRFI-19
  (- (time-monotonic->julian-day time)
     (/ 4800001 2)))

(define (julian-day->time-utc jdn) ;; SRFI-19
  (let ( (nanosecs (* tm%nano tm%sid (- jdn tm%tai-epoch-in-jd))) )
    (make-time {:type       :time-utc
                :nanosecond (remainder nanosecs tm%nano)
                :second     (floor (/ nanosecs tm%nano))})))

(define (julian-day->time-tai jdn) ;; SRFI-19
  (time-utc->time-tai! (julian-day->time-utc jdn)))

(define (julian-day->time-monotonic jdn) ;; SRFI-19
  (time-utc->time-monotonic! (julian-day->time-utc jdn)))

(define (julian-day->date jdn :optional (tz-offset (tm%local-tz-offset))) ;; SRFI-19
  (time-utc->date (julian-day->time-utc jdn) tz-offset))

(define (modified-julian-day->date jdn :optional (tz-offset (tm%local-tz-offset))) ;; SRFI-19
  (julian-day->date (+ jdn (/ 4800001 2)) tz-offset))

(define (modified-julian-day->time-utc jdn) ;; SRFI-19
  (julian-day->time-utc (+ jdn (/ 4800001 2))))

(define (modified-julian-day->time-tai jdn) ;; SRFI-19
  (julian-day->time-tai (+ jdn (/ 4800001 2))))

(define (modified-julian-day->time-monotonic jdn) ;; SRFI-19
  (julian-day->time-monotonic (+ jdn (/ 4800001 2))))

(define (current-julian-day) ;; SRFI-19
  (time-utc->julian-day (current-time :time-utc)))

(define (current-modified-julian-day) ;; SRFI-19
  (time-utc->modified-julian-day (current-time :time-utc)))

;; returns a string rep. of number N, of minimum LENGTH,
;; padded with character PAD-WITH. If PAD-WITH if #f,
;; no padding is done, and it's as if number->string was used.
;; if string is longer than LENGTH, it's as if number->string was used.

(define (tm%padding n pad-with length) ;; SRFI-19
  (let* ( (str (number->string n))
	  (str-len (string-length str)) )
    (if (or (> str-len length)
            (not pad-with))
	str
	(let* ( (new-str (make-string length pad-with))
		(new-str-offset (- (string-length new-str)
				   str-len)) )
	  (do ((i 0 (+ i 1)))
            ((>= i (string-length str)))
            (string-set! new-str (+ new-str-offset i)
                         (string-ref str i)))
	  new-str))))

(define (tm%last-n-digits i n) ;; SRFI-19
  (inexact->exact (abs (remainder i (expt 10 n)))))

(define (tm%locale-abbr-weekday n)  ;; SRFI-19
  (->text (vector-ref tm%locale-abbr-weekday-vector n)))

(define (tm%locale-long-weekday n) ;; SRFI-19
  (->text (vector-ref tm%locale-long-weekday-vector n)))

(define (tm%locale-abbr-month n) ;; SRFI-19
  (->text (vector-ref tm%locale-abbr-month-vector n)))

(define (tm%locale-long-month n) ;; SRFI-19
  (->text (vector-ref tm%locale-long-month-vector n)))

(define (tm%vector-find needle haystack comparator) ;; SRFI-19
  (let ((len (length haystack)))
    (define (tm%vector-find-int index) ;; SRFI-19
      (cond
        ((>= index len) #f)
        ((comparator needle (vector-ref haystack index)) index)
        (#t (tm%vector-find-int (+ index 1)))))
    (tm%vector-find-int 0)))

(define (text=? a b)
  (equal? (->text a) (->text b)))

(define (tm%locale-abbr-weekday->index string) ;; SRFI-19
  (tm%vector-find string tm%locale-abbr-weekday-vector text=?))

(define (tm%locale-long-weekday->index string) ;; SRFI-19
  (tm%vector-find string tm%locale-long-weekday-vector text=?))

(define (tm%locale-abbr-month->index string) ;; SRFI-19
  (tm%vector-find string tm%locale-abbr-month-vector text=?))

(define (tm%locale-long-month->index string) ;; SRFI-19
  (tm%vector-find string tm%locale-long-month-vector text=?))

;; do nothing.
;; Your implementation might want to do something...
;;
(define (tm%locale-print-time-zone date port) ;; SRFI-19
  (values))

;; Again, locale specific.
(define (tm%locale-am/pm hr) ;; SRFI-19
  (->text
   (if (> hr 11) 'tm%locale-pm 'tm%locale-am)))

(define (tm%tz-printer offset port) ;; SRFI-19
  (cond
   ((= offset 0) (display "Z" port))
   ((negative? offset) (display "-" port))
   (#t (display "+" port)))
  (if (not (= offset 0))
      (let ( (hours   (abs (quotient offset (* 60 60))))
             (minutes (abs (quotient (remainder offset (* 60 60)) 60))) )
        (display (tm%padding hours #\0 2) port)
        (display (tm%padding minutes #\0 2) port))))

;; A table of output formatting directives.
;; the first time is the format char.
;; the second is a procedure that takes the date, a padding character
;; (which might be #f), and the output port.
;;
(define tm%directives ()) ;; SRFI-19

(define (set-date-output-directive-handler! char fn)
  (runtime-check char? char)
  (runtime-check procedure? fn)
  (push! (cons char fn) tm%directives))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (set-date-output-directive-handler! #\~ (lambda (date pad-with port)
                                            (display #\~ port)))

  (set-date-output-directive-handler! #\a (lambda (date pad-with port)
                                            (display (tm%locale-abbr-weekday (date-week-day date))
                                                     port)))

  (set-date-output-directive-handler! #\b (lambda (date pad-with port)
                                            (display (tm%locale-abbr-month (:month date))
                                                     port)))

  (set-date-output-directive-handler! #\B (lambda (date pad-with port)
                                            (display (tm%locale-long-month (:month date))
                                                     port)))

  (set-date-output-directive-handler! #\c (lambda (date pad-with port)
                                            (display (date->string date 'tm%locale-date-time-format) port)))

  (set-date-output-directive-handler! #\d (lambda (date pad-with port)
                                            (display (tm%padding (:day date)
                                                                 #\0 2)
                                                     port)))

  (set-date-output-directive-handler! #\D (lambda (date pad-with port)
                                            (display (date->string date 'tm%locale-short-date-format) port)))

  (set-date-output-directive-handler! #\e (lambda (date pad-with port)
                                            (display (tm%padding (:day date)
                                                                 #\space 2)
                                                     port)))

  (set-date-output-directive-handler! #\f (lambda (date pad-with port)
                                            (if (> (:nanosecond date)
                                                   tm%nano)
                                                (display (tm%padding (+ (:second date) 1) pad-with 2)  port)
                                                (display (tm%padding (:second date) pad-with 2) port))
                                            (let* ((ns (tm%fractional-part (/ (:nanosecond date)
                                                                              tm%nano 1.0)))
                                                   (le (string-length ns)))
                                              (if (> le 2)
                                                  (begin
                                                    (display 'tm%locale-number-separator port)
                                                    (display (substring ns 2 le) port))))))

  (set-date-output-directive-handler! #\h (lambda (date pad-with port)
                                            (display (date->string date "~b") port)))

  (set-date-output-directive-handler! #\H (lambda (date pad-with port)
                                            (display (tm%padding (:hour date)
                                                                 pad-with 2)
                                                     port)))

  (set-date-output-directive-handler! #\I (lambda (date pad-with port)
                                            (let ((hr (:hour date)))
                                              (if (> hr 12)
                                                  (display (tm%padding (- hr 12) pad-with 2) port)
                                                  (display (tm%padding hr pad-with 2) port)))))

  (set-date-output-directive-handler! #\j (lambda (date pad-with port)
                                            (display (tm%padding (date-year-day date)
                                                                 pad-with 3)
                                                     port)))

  (set-date-output-directive-handler! #\k (lambda (date pad-with port)
                                            (display (tm%padding (:hour date) #\0 2) port)))

  (set-date-output-directive-handler! #\l (lambda (date pad-with port)
                                            (let ((hr (if (> (:hour date) 12)
                                                          (- (:hour date) 12)
                                                          (:hour date))))
                                              (display (tm%padding hr #\space 2) port))))

  (set-date-output-directive-handler! #\m (lambda (date pad-with port)
                                            (display (tm%padding (:month date) pad-with 2) port)))

  (set-date-output-directive-handler! #\M (lambda (date pad-with port)
                                            (display (tm%padding (:minute date) pad-with 2) port)))

  (set-date-output-directive-handler! #\n (lambda (date pad-with port)
                                            (newline port)))

  (set-date-output-directive-handler! #\N (lambda (date pad-with port)
                                            (display (tm%padding (:nanosecond date) pad-with 9) port)))

  (set-date-output-directive-handler! #\p (lambda (date pad-with port)
                                            (display (tm%locale-am/pm (:hour date)) port)))

  (set-date-output-directive-handler! #\r (lambda (date pad-with port)
                                            (display (date->string date "~I:~M:~S ~p") port)))

  (set-date-output-directive-handler! #\s (lambda (date pad-with port)
                                            (display (:second (date->time-utc date)) port)))

  (set-date-output-directive-handler! #\S (lambda (date pad-with port)
                                            (if (> (:nanosecond date)
                                                   tm%nano)
                                                (display (tm%padding (+ (:second date) 1) pad-with 2) port)
                                                (display (tm%padding (:second date) pad-with 2) port))))

  (set-date-output-directive-handler! #\t (lambda (date pad-with port)
                                            (display (integer->char 9) port)))

  (set-date-output-directive-handler! #\T (lambda (date pad-with port)
                                            (display (date->string date "~H:~M:~S") port)))

  (set-date-output-directive-handler! #\U (lambda (date pad-with port)
                                            (if (> (tm%days-before-first-week date 0) 0)
                                                (display (tm%padding (+ (date-week-number date 0) 1)
                                                                     #\0 2) port)
                                                (display (tm%padding (date-week-number date 0)
                                                                     #\0 2) port))))

  (set-date-output-directive-handler! #\V (lambda (date pad-with port)
                                            (display (tm%padding (date-week-number date 1)
                                                                 #\0 2) port)))

  (set-date-output-directive-handler! #\w (lambda (date pad-with port)
                                            (display (date-week-day date) port)))

  (set-date-output-directive-handler! #\x (lambda (date pad-with port)
                                            (display (date->string date 'tm%locale-short-date-format) port)))

  (set-date-output-directive-handler! #\X (lambda (date pad-with port)
                                            (display (date->string date 'tm%locale-time-format) port)))

  (set-date-output-directive-handler! #\W (lambda (date pad-with port)
                                            (if (> (tm%days-before-first-week date 1) 0)
                                                (display (tm%padding (+ (date-week-number date 1) 1)
                                                                     #\0 2) port)
                                                (display (tm%padding (date-week-number date 1)
                                                                     #\0 2) port))))

  (set-date-output-directive-handler! #\y (lambda (date pad-with port)
                                            (display (tm%padding (tm%last-n-digits (:year date) 2) pad-with 2) port)))

  (set-date-output-directive-handler! #\Y (lambda (date pad-with port)
                                            (display (:year date) port)))

  (set-date-output-directive-handler! #\z (lambda (date pad-with port)
                                            (tm%tz-printer (:zone-offset date) port)))

  (set-date-output-directive-handler! #\Z (lambda (date pad-with port)
                                            (tm%locale-print-time-zone date port)))

  (set-date-output-directive-handler! #\1 (lambda (date pad-with port)
                                            (display (date->string date "~Y-~m-~d") port)))

  (set-date-output-directive-handler! #\2 (lambda (date pad-with port)
                                            (display (date->string date "~k:~M:~S~z") port)))

  (set-date-output-directive-handler! #\3 (lambda (date pad-with port)
                                            (display (date->string date "~k:~M:~S") port)))

  (set-date-output-directive-handler! #\4 (lambda (date pad-with port)
                                            (display (date->string date "~Y-~m-~dT~k:~M:~S~z") port)))

  (set-date-output-directive-handler! #\5 (lambda (date pad-with port)
                                            (display (date->string date "~Y-~m-~dT~k:~M:~S") port))))


(define (tm%get-formatter char) ;; SRFI-19
  (let ((associated (assoc char tm%directives)))
    (if associated (cdr associated) #f)))

(define (tm%date-printer date index format-string str-len port) ;; SRFI-19
  (if (>= index str-len)
      (values)
      (let ((current-char (string-ref format-string index)))
        (if (not (char=? current-char #\~))
            (begin
              (display current-char port)
              (tm%date-printer date (+ index 1) format-string str-len port))

            (if (= (+ index 1) str-len) ; bad format string.
                (tm%time-error 'tm%date-printer 'bad-date-format-string format-string)
                (let ((pad-char? (string-ref format-string (+ index 1))))
                  (cond
                   ((char=? pad-char? #\-)
                    (if (= (+ index 2) str-len) ; bad format string.
                        (tm%time-error 'tm%date-printer 'bad-date-format-string
                                       format-string)
                        (let ( (formatter (tm%get-formatter
                                           (string-ref format-string
                                                       (+ index 2)))) )
                          (if (not formatter)
                              (tm%time-error 'tm%date-printer 'bad-date-format-string
                                             format-string)
                              (begin
                                (formatter date #f port)
                                (tm%date-printer date (+ index 3)
                                                 format-string str-len port))))))

                   ((char=? pad-char? #\_)
                    (if (= (+ index 2) str-len) ; bad format string.
                        (tm%time-error 'tm%date-printer 'bad-date-format-string
                                       format-string)
                        (let ( (formatter (tm%get-formatter
                                           (string-ref format-string
                                                       (+ index 2)))) )
                          (if (not formatter)
                              (tm%time-error 'tm%date-printer 'bad-date-format-string
                                             format-string)
                              (begin
                                (formatter date #\space port)
                                (tm%date-printer date (+ index 3)
                                                  format-string str-len port))))))
                   (#t
                    (let ( (formatter (tm%get-formatter
                                       (string-ref format-string
                                                   (+ index 1)))) )
                      (if (not formatter)
                          (tm%time-error 'tm%date-printer 'bad-date-format-string
                                         format-string)
                          (begin
                            (formatter date #\0 port)
                            (tm%date-printer date (+ index 2)
                                             format-string str-len port))))))))))))


(define (date->string date :optional (format-string "~c")) ;; SRFI-19
  (let ((format-string (->text format-string)))
    (let ((str-port (open-output-string)))
      (tm%date-printer date 0 format-string (string-length format-string) str-port)
      (get-output-string str-port))))

(define (tm%char->int ch) ;; SRFI-19
  (cond
    ((char=? ch #\0) 0)
    ((char=? ch #\1) 1)
    ((char=? ch #\2) 2)
    ((char=? ch #\3) 3)
    ((char=? ch #\4) 4)
    ((char=? ch #\5) 5)
    ((char=? ch #\6) 6)
    ((char=? ch #\7) 7)
    ((char=? ch #\8) 8)
    ((char=? ch #\9) 9)
    (#t
     (tm%time-error 'string->date 'bad-date-template-string
                    (list "Non-integer character" ch )))))


(define (tm%integer-reader upto port) ;; SRFI-19
  "read an integer upto n characters long on port; upto -> #f if any length"
  (define (accum-int port accum nchars) ;; SRFI-19
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto )))
          accum
          (accum-int port (+ (* accum 10)
                             (tm%char->int (read-char port))) (+ nchars 1)))))
  (accum-int port 0 0))

(define (tm%make-integer-reader upto) ;; SRFI-19
  (lambda (port)
    (tm%integer-reader upto port)))



(define (tm%fractional-integer-reader upto port) ;; SRFI-19
  "read an fractional integer upto n characters long on port; upto -> #f if
   any length. The return value is normalized to upto decimal places. For
   example, if upto is 9 and the string read is '123', the return value is
   '123000000'."
  (define (accum-int port accum nchars) ;; SRFI-19
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto )))
          (* accum (expt 10 (- upto nchars)))
          (accum-int port (+ (* accum 10) (tm%char->int (read-char port))) (+ nchars 1)))))
  (accum-int port 0 0))

(define (tm%make-fractional-integer-reader upto) ;; SRFI-19
  (lambda (port)
    (tm%fractional-integer-reader upto port)))


(define (tm%integer-reader-exact n port) ;; SRFI-19
  "read *exactly* n characters and convert to integer; could be padded"
  (let ( (padding-ok #t) )
    (define (accum-int port accum nchars) ;; SRFI-19
      (let ((ch (peek-char port)))
	(cond
          ((>= nchars n) accum)
          ((eof-object? ch)
           (tm%time-error 'string->date 'bad-date-template-string
                          "Premature ending to integer read."))
          ((char-numeric? ch)
           (set! padding-ok #f)
           (accum-int port (+ (* accum 10) (tm%char->int (read-char
                                                          port)))
                      (+ nchars 1)))
          (padding-ok
           (read-char port) ; consume padding
           (accum-int port accum (+ nchars 1)))
          (#t ; padding where it shouldn't be
           (tm%time-error 'string->date 'bad-date-template-string
			  "Non-numeric characters in integer read.")))))
    (accum-int port 0 0)))


(define (tm%make-integer-exact-reader n) ;; SRFI-19
  (lambda (port)
    (tm%integer-reader-exact n port)))

(define (tm%zone-reader port)  ;; SRFI-19
  (let ( (offset 0)
	 (positive? #f) )
    (let ( (ch (read-char port)) )
      (if (eof-object? ch)
	  (tm%time-error 'string->date 'bad-date-template-string
			 (list "Invalid time zone +/-" ch)))
      (if (or (char=? ch #\Z) (char=? ch #\z))
          0
          (begin
            (cond
             ((char=? ch #\+) (set! positive? #t))
             ((char=? ch #\-) (set! positive? #f))
             (#t
              (tm%time-error 'string->date 'bad-date-template-string
                             (list "Invalid time zone +/-" ch))))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (tm%time-error 'string->date 'bad-date-template-string
                                 (list "Invalid time zone number" ch)))
              (set! offset (* (tm%char->int ch) 10 60 60)))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (tm%time-error 'string->date 'bad-date-template-string
                                 (list "Invalid time zone number" ch)))
              (set! offset (+ offset (* (tm%char->int ch) 60 60))))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (tm%time-error 'string->date 'bad-date-template-string
                                 (list "Invalid time zone number" ch)))
              (set! offset (+ offset (* (tm%char->int ch) 10 60))))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                  (tm%time-error 'string->date 'bad-date-template-string
                                 (list "Invalid time zone number" ch)))
              (set! offset (+ offset (* (tm%char->int ch) 60))))
            (if positive? offset (- offset)))))))

;; looking at a char, read the char string, run thru indexer, return index
(define (tm%locale-reader port indexer) ;; SRFI-19
  (let ( (string-port (open-output-string)) )
    (define (read-char-string) ;; SRFI-19
      (let ((ch (peek-char port)))
	(if (char-alphabetic? ch)
	    (begin (write-char (read-char port) string-port)
		   (read-char-string))
	    (get-output-string string-port))))
    (let* ( (str (read-char-string))
	    (index (indexer str)) )
      (if index index (tm%time-error 'string->date
				     'bad-date-template-string
				     (list "Invalid string for " indexer))))))

(define (tm%make-locale-reader indexer) ;; SRFI-19
  (lambda (port)
    (tm%locale-reader port indexer)))

(define (tm%make-char-id-reader char) ;; SRFI-19
  (lambda (port)
    (if (char=? char (read-char port))
	char
	(tm%time-error 'string->date
		       'bad-date-template-string
		       "Invalid character match."))))

(define (set-date-input-directive-handling! char valid-char?-fn read-val-fn integrate-val-fn)
  "Extend the set of date input directives, by adding a binding for the dispatch
   character specified by <char>. <valid-char?-fn> is a funtion taking a port that
   returns true as soon as the next character on the port is acceptable for input.
   <read-val-fn> is a function taking a port that reads a value. <integrate-val-fn>
   is a function that takes the value from <read-val-fn> and a date, and side effects
   the date to integrate the value."
  (push! (list char valid-char?-fn read-val-fn integrate-val-fn)
         tm%read-directives))

(define (tm%do-nothing val object)
  (values))

(define tm%read-directives  ()) ;; SRFI-19

(eval-when (:load-toplevel :compile-toplevel :execute)
  (set-date-input-directive-handling! #\~
                                     (lambda (ch) #t)
                                     (tm%make-char-id-reader #\~)
                                     tm%do-nothing)

  (set-date-input-directive-handling! #\a
                                     char-alphabetic?
                                     (tm%make-locale-reader tm%locale-abbr-weekday->index)
                                     tm%do-nothing)

  (set-date-input-directive-handling! #\A
                                     char-alphabetic?
                                     (tm%make-locale-reader tm%locale-long-weekday->index)
                                     tm%do-nothing)

  (set-date-input-directive-handling! #\b
                                     char-alphabetic?
                                     (tm%make-locale-reader tm%locale-abbr-month->index)
                                     (lambda (val object)
                                       (hash-set! object :month val)))

  (set-date-input-directive-handling! #\B
                                     char-alphabetic?
                                     (tm%make-locale-reader tm%locale-long-month->index)
                                     (lambda (val object)
                                       (hash-set! object :month val)))

  (set-date-input-directive-handling! #\d
                                     char-numeric?
                                     (tm%make-integer-reader 2)
                                     (lambda (val object)
                                       (hash-set! object :day val)))

  (set-date-input-directive-handling! #\e
                                     (lambda (ch) #t)
                                     (tm%make-integer-exact-reader 2)
                                     (lambda (val object)
                                       (hash-set! object :day val)))

  (set-date-input-directive-handling! #\h
                                     char-alphabetic?
                                     (tm%make-locale-reader tm%locale-abbr-month->index)
                                     (lambda (val object)
                                       (hash-set! object :month val)))

  (set-date-input-directive-handling! #\H
                                     char-numeric?
                                     (tm%make-integer-reader 2)
                                     (lambda (val object)
                                       (hash-set! object :hour val)))

  (set-date-input-directive-handling! #\k
                                     (lambda (ch) #t)
                                     (tm%make-integer-exact-reader 2)
                                     (lambda (val object)
                                       (hash-set! object :hour val)))

  (set-date-input-directive-handling! #\m
                                     char-numeric?
                                     (tm%make-integer-reader 2)
                                     (lambda (val object)
                                       (hash-set! object :month val)))

  (set-date-input-directive-handling! #\M
                                     char-numeric?
                                     (tm%make-integer-reader 2)
                                     (lambda (val object)
                                       (hash-set! object :minute val)))

  (set-date-input-directive-handling! #\N
                                     char-numeric?
                                     (tm%make-fractional-integer-reader 9)
                                     (lambda (val object)
                                       (hash-set! object :nanosecond val)))

  (set-date-input-directive-handling! #\S
                                     char-numeric?
                                     (tm%make-integer-reader 2)
                                     (lambda (val object)
                                       (hash-set! object :second val)))

  (set-date-input-directive-handling! #\y
                                     (lambda (ch) #t)
                                     (tm%make-integer-exact-reader 2)
                                     (lambda (val object)
                                       (hash-set! object :year (tm%natural-year val))))

  (set-date-input-directive-handling! #\Y
                                     char-numeric?
                                     (tm%make-integer-reader 4)
                                     (lambda (val object)
                                       (hash-set! object :year val)))

  (set-date-input-directive-handling! #\z
                                     (lambda (c)
                                       (or (char=? c #\Z)
                                           (char=? c #\z)
                                           (char=? c #\+)
                                           (char=? c #\-)))
                                     tm%zone-reader
                                     (lambda (val object)
                                       (hash-set! object :zone-offset val))))

(define (tm%string->date date index format-string str-len port template-string) ;; SRFI-19
  (define (skip-until port skipper) ;; SRFI-19
    (let ((ch (peek-char port)))
      (if (eof-object? ch)
          (tm%time-error 'string->date 'bad-date-format-string template-string)
          (if (not (skipper ch))
              (begin (read-char port) (skip-until port skipper))))))
  (if (>= index str-len)
      (begin
        (values))
      (let ( (current-char (string-ref format-string index)) )
        (if (not (char=? current-char #\~))
            (let ((port-char (read-char port)))
              (if (or (eof-object? port-char)
                      (not (char=? current-char port-char)))
                  (tm%time-error 'string->date 'bad-date-format-string template-string))
              (tm%string->date date (+ index 1) format-string str-len port template-string))
            ;; otherwise, it's an escape, we hope
            (if (> (+ index 1) str-len)
                (tm%time-error 'string->date 'bad-date-format-string template-string)
                (let* ( (format-char (string-ref format-string (+ index 1)))
                        (format-info (assoc format-char tm%read-directives)) )
                  (if (not format-info)
                      (tm%time-error 'string->date 'bad-date-format-string template-string)
                      (begin
                        (let ((skipper (cadr format-info))
                              (reader  (caddr format-info))
                              (actor   (cadddr format-info)))
                          (skip-until port skipper)
                          (let ((val (reader port)))
                            (if (eof-object? val)
                                (tm%time-error 'string->date 'bad-date-format-string template-string)
                                (actor val date)))
                          (tm%string->date date (+ index 2) format-string  str-len port template-string))))))))))

(define (string->date input-string template-string) ;; SRFI-19
  (let ((template-string (->text template-string)))
    (define (tm%date-ok? date)
      (and (:nanosecond date)
           (:second date)
           (:minute date)
           (:hour date)
           (:day date)
           (:month date)
           (:year date)
           (:zone-offset date)))
    (let ( (newdate (make-date {:nanosecond  0
                                :second      0
                                :minute      0
                                :hour        0
                                :day         #f
                                :month       #f
                                :year        #f
                                :zone-offset (tm%local-tz-offset)})) )
      (tm%string->date newdate
                       0
                       template-string
                       (string-length template-string)
                       (open-input-string input-string)
                       template-string)
      (if (tm%date-ok? newdate)
          newdate
          (tm%time-error 'string->date 'bad-date-format-string
                         (list "Incomplete date read. " newdate template-string))))))

(define-method (scheme::print-object (obj date) port machine-readable? shared-structure-map)
  ;; REVISIT: this doesn't print all available resolution. Is this a problem?
  ;; Strictily speaking it is, because that there are non-equal dates that
  ;; have print representations that imply they are equal.
  (write-strings port "#D\"" (date->string obj 'tm%iso-8601-date-time-format) "\""))

(define-method (scheme::print-object (obj time) port machine-readable? shared-structure-map)
  (write-strings port "#T")
  (if (= (:nanosecond obj) 0)
      (write (list (:type obj) (:second obj)) port)
      (write (list (:type obj) (:second obj) (:nanosecond obj)) port)))

(define (read-date port)
  (read-char port)
  (let* ((location (port-location port))
         (iso-8601-date (read port)))
    (unless (string? iso-8601-date)
      (read-error :reader-bad-date-syntax port location))
    (string->date iso-8601-date 'tm%iso-8601-date-time-format)))

(define (read-time port)
  (read-char port)
  (let* ((location (port-location port))
         (unparsed-time (read port)))
    (unless  (list? unparsed-time)
      (read-error :reader-bad-time-syntax port location))
    (case (length unparsed-time)
      ((3) (make-time {:type       (first unparsed-time)
                       :second     (second unparsed-time)
                       :nanosecond (third unparsed-time)}))
      ((2) (make-time {:type       (first unparsed-time)
                       :second     (second unparsed-time)
                       :nanosecond 0}))
      (#t
       (read-error :reader-bad-time-syntax port location)))))
