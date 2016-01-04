;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bench.scm
;;;
;;; A simple set of benchmarks to evaluate interpreter performance
;;;

(define-package "bench"
  (:uses "scheme" "csv")
  (:exports "account"
            "test-benchmarks"
            "bench"
            "fast-bench"))

;;;; Information on the current system

(define (benchmark-system-info)
  "Return a list describing the current build of scan and the hardware
it's running on."
  (define (without-domain-name sys-name) ;; REVISIT: Make part of stdlib?
    (aif (string-search "." sys-name)
         (substring sys-name 0 it)
         sys-name))
  (let ((si (system-info)))
    (list (without-domain-name (hash-ref si :system-name))
          (hash-ref si :platform-name)
          (hash-ref si :build-type))))

(define *benchmark-system-info* (benchmark-system-info))

;;;; The benchmark result database

(define *current-benchmark-sequence* #f)

(define (next-benchmark-sequence)
  (incr! *current-benchmark-sequence*)
  *current-benchmark-sequence*)

(define-structure benchmark-result
  (seq :default (next-benchmark-sequence))
  (system :default (benchmark-system-info))
  test-name
  timings
  (run-time :default (current-date)))

(define *benchmark-time-mode* :net)

(define (benchmark-result-cpu-time benchmark-result)
  (if (not (benchmark-result? benchmark-result))
      #f
      (let ((cpu-time (car (benchmark-result-timings benchmark-result)))
            (net-time (cadr (benchmark-result-timings benchmark-result))))
        (case *benchmark-time-mode*
          ((:cpu) cpu-time)
          ((:net) net-time)
          ((:gc) (- cpu-time net-time))
          (#t (error "Invalid *benchmark-time-mode*:~a " *benchmark-time-mode*))))))

(define (set-benchmark-time-mode! :optional (mode #f))
  (if mode
      (case mode
        ((:cpu :net :gc) (set! *benchmark-time-mode* mode))
        (#t (error "Bad benchmark time mode: ~a" mode)))
      (format #t "Please enter one of :cpu, :net, or :gc. Current mode is ~a\n" *benchmark-time-mode*))
  (values))

(define *last-benchmark-result-set* '())

(define *reference-benchmark-result-sets* (make-hash))

(define *benchmark-results-filename* "benchmark_results.scm")

(define (save-benchmark-results)
  "Saves the current set of benchmark results to disk."
  (with-port of (open-file *benchmark-results-filename* :mode :write)
    (display ";;;; Saved benchmark results - DO NOT ALTER!!!\n" of)
    (display ";;;; \n" of)
    (display ";;;; \n" of)
    (display "\n" of)
    (let ((results (qsort (append-map #L(hash-ref *reference-benchmark-result-sets* _ ())
                                      (hash-keys *reference-benchmark-result-sets*))
                          <
                          benchmark-result-seq)))
      (dolist (result results)
        (write result of)
        (newline of)))
    (display "; end benchmark results\n" of)))

(define (load-benchmark-results)
  "Loads the current set of benchmark results from disk."
  (catch-all
   (format (current-debug-port) "; Loading benchmark results\n")
   (time
    (with-port ip (open-file *benchmark-results-filename*)
      (hash-clear! *reference-benchmark-result-sets*)
      (let loop ((max-seq -1))
        (let ((result (read ip)))
          (cond ((eof-object? result)
                 (set! *current-benchmark-sequence* max-seq)
                 max-seq)
                ((benchmark-result? result)
                 (hash-push! *reference-benchmark-result-sets*
                             (benchmark-result-system result)
                             result)
                 (loop (max max-seq (benchmark-result-seq result))))
                (#t
                 (error "Bad benchmark result: ~s" result)))))))))


(define (promote-benchmark-results)
  "Makes the current set of benchmark results the reference for the
   current system."
  (dolist (results *last-benchmark-result-set*)
    (hash-push! *reference-benchmark-result-sets* *benchmark-system-info* results))
  (save-benchmark-results))

;;;; Execution time estimator

(define *test-benchmark-mode* #f)

(define *estimate-min-test-duration* 5)
(define *benchmark-result-bar-length* 40)
(define *benchmark-result-name-length* 25)

(define (estimate-execution-time closure)
  "Estimate the time in milliseconds it takes to execute the (parameterless)
   closure, returning a two element list consisting of the total cpu time, and the time
   net of garbage collection. The estimate is obtained by repeatedly calling the closure
   until at least *estimate-min-test-duration*  seconds of clock time pass. The
   repeat count is computed by starting at 1 and successively doubling until
   the time threshold is crossed."
  (define (execution-time count closure)
    (format #t "~a." count)
    (flush-port (current-output-port))
    (gc)
    (let ((result (scheme::%time-apply0 #L0(repeat count (apply closure)))))
      (let ((cpu-time (vector-ref result 1))
            (net-time (- (vector-ref result 1) (vector-ref result 2))))
        (list cpu-time
              (* 1000 (/ cpu-time count))
              (* 1000 (/ net-time count))))))
  (let loop ((count 1))
    (let ((result (execution-time count closure)))
      (if (or *test-benchmark-mode*
              (> (car result) *estimate-min-test-duration*))
          (cdr result)
          (loop (* count 2))))))

;;;; The benchmark database

(define *benchmarks* (make-hash))

(define (all-benchmark-names) (hash-keys *benchmarks*))

(define (benchmark-function benchname)
  (hash-ref *benchmarks* benchname #L0(error "Undefined benchmark: ~a" benchname)))

(define benchmark-time-sym (gensym))

(defmacro (defbench benchname . code)
  `(hash-set! *benchmarks* ',benchname (lambda ()
                                         (let ((,benchmark-time-sym #f))
                                           ,@code
                                           ,benchmark-time-sym))))


(defmacro (account . code)
  `(set! ,benchmark-time-sym (estimate-execution-time (lambda () ,@code))))

;;;; Benchmark result reporting

(define (benchmark-result-named? result name)
  (eq? (benchmark-result-test-name result) name))

(define (find-test-result test-name results)
  (find #L(benchmark-result-named? _ test-name) results))

(define (most-current-benchmarks-for-result-set results)
  "Given a list of benchmark results <results>, return a list composed
   only of the most current results for each test."
  (map #L(find-test-result _ results)
       (list->set (map benchmark-result-test-name results))))

(define (reference-result-set :optional (system *benchmark-system-info*))
  "Returns the best reference results for <system>. <system> defaults to
   the current system."
  (most-current-benchmarks-for-result-set
   (aif (hash-ref *reference-benchmark-result-sets* system)
     it
     ())))

(define (select-benchmark-result :optional (set-type "result"))
  "Allow the user to pick from a list of available benchmark result sets,
   returning the set."
  (catch 'none-chosen
    (let ((choice (repl-choose (cons :last-run (hash-keys *reference-benchmark-result-sets*))
			       (format #f "Pick a ~a set" set-type))))
      (most-current-benchmarks-for-result-set
       (if (eq? choice :last-run)
	   *last-benchmark-result-set*
	   (hash-ref *reference-benchmark-result-sets* choice ()))))))

(define *benchmark-scale-factor* 2.0)

(define (display-benchmark-result name actual nominal)

  (define (bar value max-value)
    (let ((value (inexact->exact (* (/ value max-value) *benchmark-result-bar-length*)))
          (nominal (inexact->exact  (/  *benchmark-result-bar-length* *benchmark-scale-factor*)))
          (max-value *benchmark-result-bar-length*))
      (let ((limited-value (min value max-value)))
        (string-set! (format #f "[~a~a~a"
                             (make-string limited-value #\-)
                             (make-string (- max-value limited-value) #\space)
                             (if (> value max-value) ">> " "]  "))
                     nominal #\+))))

  (display (pad-to-width name *benchmark-result-name-length*))
  (dynamic-let ((*flonum-print-precision* 3))
    (if nominal
        (let ((scaled-value (/ actual nominal)))
          (format #t "~a~a (~a ms.)\n"
                  (bar scaled-value *benchmark-scale-factor*)
                  scaled-value
                  actual))
        (format #t "-- No Reference Data -- (~a ms.)\n" actual))))

(define (display-benchmark-results results :optional (reference (reference-result-set)))
  (if (null? reference)
      (format #t "\n\nNo Reference results for system: ~a " *benchmark-system-info*)
      (dynamic-let ((*info* #f))
        (format #t "\n\nBenchmark results (shorter bar is better, compared to ~a):" (benchmark-result-system (car reference)))
        (format #t "\nBenchmark time mode = ~a\n" *benchmark-time-mode*)
        (dolist (result (qsort results string< #L(symbol-name (benchmark-result-test-name _))))
          (display-benchmark-result (benchmark-result-test-name result)
                                    (benchmark-result-cpu-time result)
                                    (benchmark-result-cpu-time (find-test-result (benchmark-result-test-name result)
                                                                                 reference)))))))

;;;; The benchmark runner

(defmacro (bench-repeat n . code)
  `(repeat (if *test-benchmark-mode* 1 ,n)
     ,@code))

(define (test-benchmarks . tests)
  "Run through all benchmarks as quickly as possible to check for runtime
   errors."
  (dynamic-let ((*test-benchmark-mode* #t))
    (apply bench tests)))

(define (bench :optional (tests (all-benchmark-names)))
  (let ((count 0))
    (define (run-named-benchmark bench-name)
      (incr! count)
      (format #t "\n[~a/~a] ~a: " count (length tests) bench-name)
      (make-benchmark-result :test-name bench-name
                             :timings ((benchmark-function bench-name))))

    (set! *last-benchmark-result-set* (map run-named-benchmark (qsort tests string< symbol-name)))
    
    (display-benchmark-results *last-benchmark-result-set*)
    (format #t "\nEvaluate (promote-benchmark-results) to make these results the standard for ~s" *benchmark-system-info*)
    (format #t "\nEvaluate (compare-benchmark-results) to benchmark results\n\n")
    (values)))

(define (compare-benchmark-results)
  (display-benchmark-results (select-benchmark-result)
                             (select-benchmark-result "reference")))

(push! '(:bench bench "Runs the benchmark suite")
       scheme::*repl-abbreviations*)

(push! '(:pbr promote-benchmark-results "Promotes the current benchmark results to 'official' status")
       scheme::*repl-abbreviations*)

(push! '(:cbr compare-benchmark-results "Compares two sets of benchmark results.")
       scheme::*repl-abbreviations*)

(push! '(:sbtm set-benchmark-time-mode! "Sets the benchmark time mode.")
       scheme::*repl-abbreviations*)

;;;;; Bootup

(load "standard-benchmarks.scm")

(load-benchmark-results)
