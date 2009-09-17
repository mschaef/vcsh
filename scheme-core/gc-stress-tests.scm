
(define-package "gc-stress-tests"
  (:uses "scheme")
  (:exports "gc-stress-test-1"
            "gc-stress-test-2"
            "gc-stress-test-3"
            "gc-stress-test-4"
            "gc-stress-test-5"
            ))

;; Currently, the interpreter itself is not re-entrant.
;(set! *after-gc* ())

;; (%lisp-heap-stress-thread type count sleep-duration)
;;
;; type - The type of stress thread to create:
;;    0 - Allocate <count> transient conses in a tight loop and terminate, sleeping
;;        <sleep-duration> ms per iteration. <count> of <0 requests an infinte loop.
;;    1 - Allocate <count> GC tripwires in recursively invoked stack frames,
;;        sleep <sleep-duration> ms, unwind the stack, and terminate.
;;    2 - Repeatedly invoke garbage collection and terminate.
;;    3 - Like type 1, but the thread retains references to all conses.
(define %lisp-heap-stress-thread #.(scheme::%subr-by-name "%lisp-heap-stress-thread"))

(define %gc-trip-wire #.(scheme::%subr-by-name "%gc-trip-wire"))
(define %arm-gc-trip-wires #.(scheme::%subr-by-name "%arm-gc-trip-wires"))

;; stress tests begin here

(define (gc-stress-test-1 :optional (iter-count #f) (thread-count 4))
  "Repeatedly create a batch of threads, letting them all terminate before the
   next iteration."
  (let loop ((ii 0))
    (unless (and iter-count (>= ii iter-count))
       (repeat thread-count
         (%lisp-heap-stress-thread 0 1 200))
       (sleep 1000)
       (loop (+ ii 1)))))

(define (gc-stress-test-2 :optional (thread-count 4) (cell-count -1))
  "Launch <thread-count> stress threads, each allocating <cell-count> conses before
   terminating. <cell-count> defaults to infinte runs."
  (repeat thread-count
     (%lisp-heap-stress-thread 0 cell-count 0)))

(define (gc-stress-test-3 :optional (thread-count 4) (stack-depth 100))
  "Launch <thread-count> stress threads that stack allocate GC tripwires,
   and check for trips on GC."
  (repeat thread-count
    (%lisp-heap-stress-thread 1 stack-depth 10000))
  (sleep 2000)
  (gc)
  (format (current-error-port) "\n\nARMING TRIP WIRES\n\n")
  (%arm-gc-trip-wires #t)
  (format (current-error-port) "\nTEST GC\n\n")
  (gc)
  (format (current-error-port) "\n\nDISARMING TRIP WIRES\n\n")
  (%arm-gc-trip-wires #f))

(define (gc-stress-test-4 :optional (thread-count 4) (iter-count -1))
  "Launch <thread-count> stress threads that repeatedly invoke the GC as quickly
   as possible."
  (repeat thread-count
    (%lisp-heap-stress-thread 2 iter-count)))

(define (gc-stress-test-5 :optional (thread-count 4) (iter-count -1))
  "Launch <thread-count> stress threads that repeatedly invoke the GC as quickly
   as possible, and <thread-count> stress threads that repatedly cons."
  (repeat thread-count
    (%lisp-heap-stress-thread 0 iter-count))
  (repeat thread-count
    (%lisp-heap-stress-thread 2 iter-count)))







