;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bench.scm
;;;
;;; A simple set of benchmarks to evaluate interpreter performance
;;;

(define-package "bench"
  (:uses "scheme" "csv")
  (:exports "test-benchmarks"
            "bench"
            "hash-bench"
            "fast-bench"))

;; To prevent timing inconsistancies caused by run-time heap
;; expansion, expand the heap ahead of time.

(enlarge-heap 64)

;; Turn the stack limit off.
;(scheme::%set-stack-limit #f)

(define-structure benchmark-result
  seq
  system
  test-name
  timings
  (run-time :default #f))

(define (benchmark-result-cpu-time benchmark-result)
  (if (not (benchmark-result? benchmark-result))
      #f
      (car (benchmark-result-timings benchmark-result))))

(define *benchmarks* '())

(define benchmark-time-sym (gensym))

(defmacro (account . code)
  `(begin
     (gc)
     (set! ,benchmark-time-sym (estimate-execution-time (lambda () ,@code)))))

(export! 'account)

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
    (gc)
    (format #t "~a." count)
    (flush-port (current-output-port))
    (let ((result (scheme::%time-apply0 (lambda () (repeat count (apply closure))))))
      (let ((cpu-time (vector-ref result 1))
	    (net-time (- (vector-ref result 1)
			 (vector-ref result 2))))
	(list cpu-time
	      (* 1000 (/ cpu-time count))
	      (* 1000 (/ net-time count))))))
  (define (iter count)
    (let ((result (execution-time count closure)))
      (if (> (car result) *estimate-min-test-duration* )
	  (cdr result)
	  (iter (* count 2)))))
  (iter 1))

(defmacro (defbench benchname . code)
  `(begin
     (unless (member ',benchname *benchmarks*)
       (push! ',benchname *benchmarks*))
     (define  ,benchname (lambda ()
                           (let ((,benchmark-time-sym #f))
                             ,@code
                             ,benchmark-time-sym)))))


(define *last-benchmark-result-set* '())

(define *reference-benchmark-result-sets* (make-hash :equal))

(define (benchmark-system-info)
  "Return a list describing the current build of scan and the hardware it's
   running on."
  (let ((si (system-info)))
    (list (hash-ref si :system-name)
          (hash-ref si :platform-name)
          (hash-ref si :build-type))))

(define (add-reference-result result)
  "Adds benchmark result <result> to the current reference results, listed
   under the current system type."
  (hash-push! *reference-benchmark-result-sets* (benchmark-system-info) result))


(define (benchmark-result-named? result name)
  (eq? (benchmark-result-test-name result) name))

(define (find-test-result test-name results)
  (find #L(benchmark-result-named? _ test-name) results))

(define (most-current-benchmarks-for-result-set results)
  "Given a list of benchmark results <results>, return a list composed
   only of the most current results for each test."
  (map #L(find-test-result _ results)
       (list->set (map benchmark-result-test-name results))))

(define (reference-result-set :optional (system (benchmark-system-info)))
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


(define *benchmark-results-filename* "benchmark_results.scm")



(push! '(:bench bench "Runs the benchmark suite")
       scheme::*repl-abbreviations*)

(push! '(:pbr promote-benchmark-results
              "Promotes the current benchmark results to 'official' status")
       scheme::*repl-abbreviations*)

(push! '(:cbr compare-benchmark-results
             "Compares two sets of benchmark results.")
       scheme::*repl-abbreviations*)


(define (save-benchmark-results)
  "Saves the current set of benchmark results to disk."
  (with-port of (open-output-file *benchmark-results-filename*)
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

(define *current-benchmark-sequence* #f)

(define (next-benchmark-sequence)
  (incr! *current-benchmark-sequence*)
  *current-benchmark-sequence*)

(define (load-benchmark-results)
  "Loads the current set of benchmark results from disk."
  (catch-all
   (with-port ip (open-input-file *benchmark-results-filename*)
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
                (error "Bad benchmark result: ~s" result))))))))

(load-benchmark-results)

(define (promote-benchmark-results)
  "Makes the current set of benchmark results the reference for the
   current system."
  (dolist (results *last-benchmark-result-set*)
    (hash-push! *reference-benchmark-result-sets* (benchmark-system-info) results))
  (save-benchmark-results))

(define (display-benchmark-results results :optional (reference (reference-result-set)))
  (dynamic-let ((*info* #f))
    (gc)
    (format #t "\nBenchmark results (shorter bar is better):\n")
    (dolist (result (qsort results
			   (lambda (s1 s2) (string< (symbol-name s1) (symbol-name s2)))
			   benchmark-result-test-name))
      (display-benchmark-result (benchmark-result-test-name result)
                                (benchmark-result-cpu-time result)
                                (benchmark-result-cpu-time (find-test-result (benchmark-result-test-name result)
                                                                             reference))))))


(define (bench . tests)
  (define (run-named-benchmark bench-name)
    (dynamic-let ((*info* #f))
      (gc)
      (format #t "\n~a: " bench-name)
      ((symbol-value bench-name))))

  (define (sort-benchmark-names names)
    (qsort names #L2(< (strcmp _0 _1) 0) symbol-name))

  (let* ((tests (if (null? tests) *benchmarks* tests))
         (results (map (lambda (test)
                           (make-benchmark-result :test-name test
                                                  :seq (next-benchmark-sequence)
                                                  :timings  (run-named-benchmark test)
                                                  :system (benchmark-system-info)
                                                  :run-time (current-date)))
                       (sort-benchmark-names tests))))

    (set! *last-benchmark-result-set* results)

    (display-benchmark-results results)
    (format #t "\nEvaluate (promote-benchmark-results) to make these results the standard for ~s\n"
            (benchmark-system-info))
    (format #t "\nEvaluate (compare-benchmark-results) to benchmark results\n")
    (values)))

(define (compare-benchmark-results)
  (let ((from (select-benchmark-result))
	(reference (select-benchmark-result "reference")))
    (display-benchmark-results from reference)))


(define (list-from-by b inc elems)
  (define (list-from-by-1 b elems accum)
    (if (<= elems 0)
	accum
	(list-from-by-1 (+ b inc) (- elems 1) (cons b accum))))
  (list-from-by-1 b elems '()))


(define *repeat-only-once* #f)

(defmacro (bench-repeat n . code)
  `(repeat (if *repeat-only-once* 1 ,n)
     ,@code))

(defbench exec-loop-repeat
  (account
   (bench-repeat 100000 1)))

(defbench exec-loop-tail
  (account
   (let ((loop #f))
     (set! loop (lambda (c)
		  (if (> c 0) (loop (- c 1)))))
     (loop 100000))))

(defbench exec-let
  (account
   (let ((x 0) (y 1))
     )))

(defbench exec-choose-case
  (account
   (for-each (lambda (x)
	       (case x
		 ((1) 'x)
		 ((2) 'y)
		 ((3) 'z)
		 ((4) 'a)
		 (#t 'foo)))
	     '(1 2 3 4 5 6 7 8))))

(defbench exec-choose-cond
  (account
   (for-each (lambda (x)
	       (cond
		((eq? x 1) 'x)
		((eq? x 2) 'y)
		((eq? x 3) 'z)
		((eq? x 4) 'a)
		(#t 'foo)))
	     '(1 2 3 4 5 6 7 8))))

(defbench calc-dot-product
  (let ((l1 (list-from-by 1.0 2.0 100))
	(l2 (list-from-by 2.0 2.0 100)))
  (account
   (let ((rsum 0.0))
     (for-each (lambda (x y)
		 (incr! rsum (* x y)))
	       l1 l2)))))

(defbench calc-savage
  (account
   (asin (sin (acos (cos (atan (tan 1))))))))


(defbench heap-10cons
  (account
   (cons 0 0)
   (cons 0 0)
   (cons 0 0)
   (cons 0 0)
   (cons 0 0)
   (cons 0 0)
   (cons 0 0)
   (cons 0 0)
   (cons 0 0)
   (cons 0 0)))


(defbench heap-010ele-vectors
  (account
   (make-vector (+ 1 (random 10)))))

(defbench heap-100ele-vectors
  (account
   (make-vector (+ 10 (random 100)))))

(defbench exec-var-lookup-deep
  (let ((x 0))
    (let ((dummy #f))
      (let ((dummy #f))
	(let ((dummy #f))
	  (let ((dummy #f))
	    (let ((dummy #f))
	      (let ((dummy #f))
		(let ((dummy #f))
		  (let ((dummy #f))
		    (let ((dummy #f))
		      (let ((dummy #f))
			(let ((dummy #f))
			  (let ((dummy #f))
			    (let ((dummy #f))
			      (let ((dummy #f))
				(let ((dummy #f))
				  (let ((dummy #f))
				    (account
				     x)))))))))))))))))))

(defbench exec-var-lookup-shallow
  (let ((x 0))
    (account
     x)))

(define *hash-test-syms*
              '(bitwise-xor for-each2 c unless b letrec
		define %%%typeref-constructable? make-tree log10 rational? eqv? a
		open-input-file member *flonum-print-precision* for-each1 cadr
		fast-save cddadr string-trim-right char=?
		bitwise-arithmatic-shift-right foo %%%type-slot-names list-cell
		runtime filter char-alphabetic? write-to-string open-input-string
		savage string-trim for-each use-package! current-debug-port
		input-port? is-a? in-package! describe benchname open-null-port
		fourth nth hash-ref string? %%%slot-get list-let l2 butlast
		%%%register-structure-type symbol-package string-upcase l1
		set-current-output-port remote macroexpand apply *benchmarks*
		bench-name third close-input-file truncate bitwise-rotate-right return
		close-port list errobj *pi* cadadr define-structure vector-copy asin
		cond-bench rsum output-port? cdadr abs char>=? null? gc binary-port?
		tree-set! caaadr %%%slot-set! qsort set-current-error-port qq-expand
		current-output-port gc-test var-lookup-deep results char>? real?
		string-lessp symbol? cells cdddar exp10 symbol->string fold-right
		map tan realtime get char-ci>? load exit-repl elems
		bitwise-rotate-left dynamic-let list-set! import! car flush-port delq
		open-debug-port char-ci>=? char? nil append last string-append trunc
		unwind-protect system catch environment odd? make-package! string-set!
		set! strcmp random char-downcase while case-bench open-output-string
		char<=? caddar list-from-by-1 equal? substring-equal? > newline =
		char->integer open-null-input <= < string->symbol error length atom?
		cdaaar string-length hash-key >= alist-delete assv append! begin sym
		env-lookup read repeat-loop time ass export! gc-status catch-all
		char-ci<=? char-ci=? string->number accum dot-product reverse
		list->vector sqrt type-of vector->list ceiling take nconc c-bytes
		string-downcase pop! nreverse let-bench awhen string-ref
		var-lookup-shallow exact->inexact assq prog1 cos
		string-trim-right-questions if incr! list-all-packages number->string
		block code symbol-value find-tail %%%typeref-construct strspn second
		loop / let* append-map string->keyword and keyword? set-random-seed
		cond vector? - assoc port-location allocate-heap + lambda push! some?
		character->string * list-ref vector-fill! map2 not cadar cddaar
		unquote last-pair map1 %%%typeref-type benchmark-time-sym max
		list-all-unique-symbols display-to-string symbol-name exact? char-ci<?
		%%%get-current-frames set-cdr! strcspn close-output-file
		use-package-and-export! unexport! throw acos char-upper-case?
		char-whitespace? char-ready? package? exp load-from-string
		inexact->display-string integer? substring read-from-string tail-loop
		cdaddr cadaar write-char string-trim-left floor read-char
		bitwise-shift-right %%%make-typeref symbol-bound? procedure? awhile
		atan debug-write write cons delete it result %time rest negative?
		hash-set! repeat even? do-symbols eof-object? eq? gensym
		do-external-symbols *package* quote peek-char append-map! zero?
		char-numeric? bitwise-and sin list->structure port-translate-mode
		char-upcase memq alist-copy memv name ceil subset quotient cdadar
		heap-fsck-big %%%do-types fast-print dummy exists? is-a-strict?
		gc-info setprop aif perf-report z cddddr cdr vector caar vector-ref
		defbench y caaar caadar cddar cdar cadddr x syms fast-load bitwise-or
		list-all-symbols find-symbol open-output-file account
		current-error-port caddr log unread-char apropos do-bench heap-fsck
		remainder bitwise-not alist-cons list-copy display every?
		heap-fsck-mini find expt list-from-by let set-log-level!
		string->uninterned-symbol drop positive? bitwise-shift-left case
		vector-set! fast-read char<? set-symbol-value! make-vector round eval
		min pair? fold the-environment get-output-string %%%define when
		putprop number? set-current-debug-port set-port-translate-mode! first
		boolean? string-search current-input-port s1 caaaar cdaar cdaadr
		integer->char s2 vector-length unintern! inexact->exact type-supertype
		c-blocks inc inexact? let-internal null-list? complex? find-package
		set-car! tree-ref defmacro set-current-input-port caaddr caadr cddr
		cdddr or modulo char-lower-case?))


;; (defbench csv-file-reader
;;   (account (read-csv-file "big_csv_file.csv")))

(defbench s-expression-file-reader
  (account (begin (with-port p (open-input-file "big_csv_file.sxp") (read p)) '())))

(defbench hash-set!-seq-numbers
  (let ((htable (make-hash :equal)))
    (account
     (dotimes (ii 100000)
       (hash-set! htable ii ii)))))

(defbench hash-ref-seq-numbers
  (let ((htable (make-hash :equal)))
    (dotimes (ii 100000)
      (hash-set! htable ii ii))
    (account
     (dotimes (ii 100000)
       (hash-ref htable ii)))))

(defbench hash-set!-seq-numbers/eq
  (let ((htable (make-hash :eq)))
    (account
     (dotimes (ii 100000)
       (hash-set! htable ii ii)))))

(defbench hash-ref-seq-numbers/eq
  (let ((htable (make-hash :eq)))
    (dotimes (ii 100000)
      (hash-set! htable ii ii))
    (account
     (dotimes (ii 100000)
       (hash-ref htable ii)))))

(defbench hash-set!-eq
  (let ((htable (make-hash :eq)))
    (account
     (for-each (lambda (sym) (hash-set! htable sym 'foo))
	       *hash-test-syms*))))

(defbench hash-ref-eq
  (let ((htable (make-hash :eq)))
    (for-each (lambda (sym) (hash-set! htable sym 'foo))
	      *hash-test-syms*)
    (account
     (for-each (lambda (sym) (hash-ref htable sym))
	       *hash-test-syms*))))

(defbench hash-set!-equal
  (let ((htable (make-hash :equal)))
    (account
     (for-each (lambda (sym) (hash-set! htable sym 'foo))
	       *hash-test-syms*))))

(defbench hash-ref-equal
  (let ((htable (make-hash :equal)))
    (for-each (lambda (sym) (hash-set! htable sym 'foo))
	      *hash-test-syms*)
    (account
     (for-each (lambda (sym) (hash-ref htable sym))
	       *hash-test-syms*))))

(defbench funcall-inline
  (account
   ((lambda()
      ))))

(defbench funcall-inline-args
  (account
   ((lambda( a b c d e f g )
      ) 1 2 3 4 5 6 7 )))

(defbench funcall-local
  (account
   (let ((fn-1 (lambda () )))
     (fn-1))))

(defbench funcall-local-args
  (account
   (let ((fn-1 (lambda (a b c d e f g) )))
     (fn-1 1 2 3 4 5 6 7))))

(define (fn-2) '())

(defbench funcall-global
  (account
   (fn-2)))

(define (fn-2-args a b c d e f g) '())

(defbench funcall-global-args
  (account
   (fn-2-args 1 2 3 4 5 6 7)))

(define (fn-optional a :optional b c) ())

(defbench funcall-optional-0-opts
  (account (fn-optional 1)))

(defbench funcall-optional-1-opts
  (account (fn-optional 1 2)))

(defbench funcall-optional-2-opts
  (account (fn-optional 1 2 3)))

(define (fn-keyword a :keyword k-1 k-2) ())

(defbench funcall-keyword-0-keys
  (account (fn-keyword 1)))

(defbench funcall-keyword-1-keys
  (account (fn-keyword 1 :k-1 2)))

(defbench funcall-keyword-2-keys
  (account (fn-keyword 1 :k-1 2 :k-2 3)))


(defbench lambda-eval
  (account
   (lambda () )))

(defbench output-symbol
  (let ((p (open-null-port)))
    (account
     (display *hash-test-syms* p))))

(defbench output-fixnum
  (let ((p (open-null-port))
	(fixnums (list-from-by 0 1 1000)))
    (account
     (display fixnums p))))

(defbench output-flonum
  (let ((p (open-null-port))
	(fixnums (list-from-by 0.0 1.0 1000)))
    (account
     (display fixnums p))))

(defbench output-string
  (let ((p (open-output-string)))
    (account
     (display *hash-test-syms* p))))

(define (nested-lists depth)
  (if (= depth 0)
      (list 'a 'b 'c)
      (list (nested-lists (- depth 1))
	    (nested-lists (- depth 1))
	    (nested-lists (- depth 1)))))

(defbench heap-fsck-lots-of-cells
  (account
   (nested-lists 10)))

(defbench output-nested-lists
  (let ((p (open-null-port))
	(l (nested-lists 11)))
    (account
     (display l p))))

(defbench binary-integer-io
  (let ((test-filename (temporary-file-name "sct")))
    (account
     (with-port p (open-output-file test-filename :binary)
       (bench-repeat 1048576
	 (write-binary-fixnum 12345 4 #f p)))
     (with-port p (open-input-file test-filename :binary)
       (bench-repeat 1048576
	 (read-binary-fixnum 4 #f p))))
    (delete-file test-filename)))

(defbench signal-processing
  (handler-bind ((test-signal (lambda args #f)))
    (handler-bind ((test-signal (lambda args #f)))
      (handler-bind ((test-signal (lambda args #f)))
	(handler-bind ((test-signal (lambda args #f)))
	  (account
	   (bench-repeat 10000
	     (signal 'test-signal))))))))

(defbench handler-establishment
  (account
   (bench-repeat 10000
     (handler-bind ((test-signal (lambda args #f)))
       (handler-bind ((test-signal (lambda args #f)))
	 (handler-bind ((test-signal (lambda args #f)))
	   (handler-bind ((test-signal (lambda args #f)))
	     #f)))))))

(define-generic-function (generic/+ x y)
  (error "Error: this should never be called in the benchmark"))

(define-method (generic/+ (x fixnum) (y fixnum))
  (+ x y))

(defbench generic-function-call
  (account (bench-repeat 5000
	     (generic/+ 2 3))))

(define-generic-function (generic/inheritance x)
  :base-handler)

(define-method (generic/inheritance (x number))
  (call-next-method))

(define-method (generic/inheritance (x fixnum))
  (call-next-method))

(defbench inherited-generic-function-call
  (account (bench-repeat 2000
	     (generic/inheritance 2))))

(define (hash-performance list-size iterations)
  (let ((xs (list-from-by 0 1 list-size)))
    (gc)
    (vector-ref (scheme::%time (bench-repeat iterations (hash-key xs))) 1)))

(define (cross xs ys)
  (append-map (lambda (x)
		(map (lambda (y) (list x y)) ys)) xs))

(define (table xs)
  (for-each (lambda (x)
	      (write x)
	      (newline))
	    xs))

;(define (t)
;  (table (map (lambda (params)
;		   (let ((size (car params))
;			 (iters (cadr params)))
;		     (list size iters (hash-performance size iters))))
;		 (cross
;		  (list-from-by 0     100   10)       ;size
;		  (list-from-by 10000 10000 10)))))   ;iters

(defbench list-cross-product
  (let ((xs (list-from-by 0 1 100)))
    (account
     (cross xs xs))))




(define (make-point x y z)
  (let ((ht (make-hash)))
    (hash-set! ht 'x x)
    (hash-set! ht 'y y)
    (hash-set! ht 'z z)
    ht))

(define (point-list i)
  (map (lambda (x) (make-point (random x)
				  (random x)
				  (random x)))
	  (list-from-by 1 1 i)))

(define (hash-field-sum xs f)
  (let ((sum 0.0))
    (for-each (lambda (x)
		(incr! sum (hash-ref x f)))
	      xs)
    sum))

(define (is-prime? x)
  (let ((last-factor (inexact->exact (sqrt x)))
	(factor 2)
	(prime-now? #t))
    (while (and prime-now? (<= factor last-factor))
	   (set! prime-now? (not (= 0(modulo x factor))))
	   (incr! factor))
    prime-now?))

(defbench calc-1000-primes
  (account
   (filter is-prime? (iseq 1 1000))))


(define *glob-test-strings*
  '("to-c-source.exe" "to-c-source.o" "TAGS" "fasl-dump"
    "fasl-dump.exe" "fasl-dump.o" "assert.h" "vcsh.exe" "vcsh.o" "scc"
    "a.scf" "scanlib.a" "fasl-utils.cpp" "s-core.o" "TO*DO" "s-core.cpp"
    "s-core.scf" "to-c-source" "scanlib.o" "scansh0.exe" "vcsh" "scansh0"
    "scansh0.o" "scan-vm.a" "string.o" "reader.o" "oblist.o" "number.o"
    "to-c-source.cpp" "mt19937.o" "fasl-dump.cpp" "memory.o" "main.o"
    "macro.o" "hash-table.o" "fasl-utils.o" "fasl-loader.o" "evaluator.o"
    "des-cport-test.i" "diagnostics.o" "sys.h" "list.o" "scan-types.h"
    "error.o" "fasl-loader.cpp" "vcsh.cpp" "scanlib.cpp" "mt19937.cpp"
    "oblist.cpp" "Makefile" "array.o" "scan.h" "error.cpp" "number.cpp"
    "fasl-file.h" "system.cpp" "main.cpp" "des-port.cpp" "array.cpp"
    "hash-table.cpp" "list.cpp" "memory.cpp" "diagnostics.cpp" "io.cpp"
    "string.cpp" "scan.rc" "reader.cpp" "evaluator.cpp" "mt19937.h"
    "macro.cpp" "scansh0.cpp" "io.o" "util-types.cpp" "util-types.o"
    "os.o" "util-tchar.h" "os.cpp" "sys.cpp" "sys.o" "format" "entries"
    "util-types.cpp.svn-base" "util-tchar.h.svn-base" "os.cpp.svn-base"
    "sys.cpp.svn-base" "test-guidelines.txt"
    "test-path-string-to-list.scm" "test-equalp-simple-list-vector.scm"
    "test-flush-whitespace-string-input.scm" "test-list-to-filename.scm"
    "test-filename-extension.scm" "test-filename-path.scm"
    "test-filename-basename.scm" "test-parse-glob-pattern.scm"
    "test-complex-rect-equalp.scm" "test-listp-dotted-listp.scm"
    "test-generic-function-return-value.scm"
    "test-generic-function-inheritance-arity-2.scm"
    "test-generic-function-inheritance-arity-1.scm"
    "test-nan-infinitep.scm" "test-generic-function-arity-4.scm"
    "test-generic-function-arity-2.scm"
    "test-generic-function-arity-1.scm" "test-ieee754-bits.scm"
    "test-key-list-to-hash.scm" "test-string-quasiquote.scm"
    "test-vector-filln.scm" "test-begin-2.scm" "test-booleanp.scm"
    "test-letrec-lambda.scm" "test-define-internal.scm"
    "test-for-each-n.scm" "test-for-each-2.scm" "test-for-each-1.scm"
    "test-dolist.scm" "test-map-n.scm" "test-map-2.scm" "test-map-1.scm"
    "test-equalp-simple-hash.scm" "test-EQUALp-shared-list.scm"
    "test-is-file-basenamep.scm" "test-canonicalize-filename.scm"
    "test-class-graph.scm" "test-EQUALp-simple-list-vector.scm"
    "test-EQUALp-simple-hash.scm" "test-is-directory-filenamep.scm"
    "test-EQUALp-atom.scm" "test-fast-io-shared-structure.scm"
    "test-define-external.scm" "test-fast-io-hashes.scm"
    "test-EQUALp-shared-hash.scm" "test-fast-io-lists-and-vectors.scm"
    "test-fast-io-circular-structure.scm" "test-fast-io-atoms.scm"
    "test-fast-io-symbols.scm" "test-take-whilen.scm" "test-list-ref.scm"
    "test-binary-integer-io.scm" "test-error-handler.scm"
    "test-handler-bind.scm" "test-error.scm" "test-dynamic-let.scm"
    "test-format.scm" "test-list-let.scm"
    "test-input-port-port-locations.scm"
    "test-input-port-translate-mode.scm"
    "test-fast-io-strings-and-characters.scm" "test-read-line.scm"
    "test-break.scm" "test-span.scm" "test-drop-while.scm" "test-drop.scm"
    "test-throw-catch.scm" "test-take-while.scm" "test-take.scm"
    "test-mapping.scm" "test-pair-fold-right.scm" "test-fold-right.scm"
    "test-pair-fold.scm" "test-fold.scm" "test-quasiquote.scm"
    "test-string-port-translate-test.scm"
    "test-string-first-character-substring.scm"
    "test-quasiquote-reader.scm" "test-multiple-read-from-string.scm"
    "test-integer-to-char.scm" "test-string-round-trip.scm"
    "test-string-drop-right.scm" "test-string-take-right.scm"
    "test-string-drop.scm" "test-string-search-from-right.scm"
    "test-string-take.scm" "test-string-fold.scm" "test-string-search.scm"
    "test-filename-to-list.scm" "test-is-directory-basenamep.scm"
    "test-string-comparison-ci.scm"
    "test-string-upcase-string-downcase.scm" "test-vectorp.scm"
    "test-taken.scm" "test-string-length-append.scm" "test-string-ref.scm"
    "test-proto-objects-reader.scm" "test-substring.scm"
    "test-unwind-protect-coverage.scm" "test-string-equal.scm"
    "test-do.scm" "test-symbol-printing-package-names.scm"
    "test-proto-objects.scm" "test-apply-coverage.scm"
    "test-macro-expansion.scm" "test-hash-copy.scm"
    "test-hash-table-complex-key.scm" "test-hash-type.scm"
    "test-eq-hash-table.scm" "test-hash-ref-star.scm"
    "test-character-to-string.scm" "test-list-setn.scm"
    "test-hash-setn.scm" "test-type-of-from-reader.scm"
    "test-hash-ref.scm" "test-equalp-atom.scm"
    "test-complex-rect-accessors.scm" "test-matches-globp.scm"
    "test-read-complex.scm" "test-number-divide.scm"
    "test-number-bitwise.scm" "test-number-add.scm"
    "test-numeric-simple-comparison.scm" "test-numeric-equality.scm"
    "test-number-type-predicates.scm"
    "test-numeric-complex-comparison.scm" "test-number-exactness.scm"
    "test-vector-read-write-round-trip.scm" "test-spann.scm"
    "test-string-setn.scm" "test-number-io.scm" "test-vector-resize.scm"
    "test-vector-copy.scm" "test-vector-setn.scm" "test-list->vector.scm"
    "test-anyp.scm" "test-number-multiply.scm" "test-string-trim.scm"
    "test-vector-ref.scm" "test-vector.scm" "test-vector-to-list.scm"
    "test-character-read-write.scm" "test-string-comparison-cs.scm"
    "test-let.scm" "test-popn.scm" "test-everyp.scm" "test-or.scm"
    "test-and.scm" "test-charp.scm" "#benchmark_results.scm#"
    "test-linear-execution.scm" "test-misc.scm" "test-breakn.scm"
    "test-letstar.scm" "test-stringp.scm" "run-tests.scm"
    "test-make-vector.scm" "test-begin-1.scm" "3" "2" "1" "format"
    "entries" "3.svn-base" "2.svn-base" "1.svn-base" "6" "format"
    "entries" "6.svn-base" "5" "format" "entries" "5.svn-base" "4"
    "format" "entries" "4.svn-base" "format" "entries"
    "test-path-string-to-list.scm.svn-base" "test-vectorp.scm.svn-base"
    "test-vector.scm.svn-base" "test-vector-to-list.scm.svn-base"
    "test-vector-setn.scm.svn-base"
    "test-vector-read-write-round-trip.scm.svn-base"
    "test-vector-resize.scm.svn-base" "test-vector-filln.scm.svn-base"
    "test-vector-copy.scm.svn-base"
    "test-type-of-from-reader.scm.svn-base"
    "test-unwind-protect-coverage.scm.svn-base"
    "test-throw-catch.scm.svn-base" "test-guidelines.txt.svn-base"
    "test-taken.scm.svn-base" "test-take.scm.svn-base"
    "test-vector-ref.scm.svn-base"
    "test-symbol-printing-package-names.scm.svn-base"
    "test-take-while.scm.svn-base" "test-substring.scm.svn-base"
    "test-stringp.scm.svn-base" "test-take-whilen.scm.svn-base"
    "test-string-trim.scm.svn-base" "test-string-take.scm.svn-base"
    "test-string-take-right.scm.svn-base" "test-string-setn.scm.svn-base"
    "test-string-search.scm.svn-base"
    "test-string-search-from-right.scm.svn-base"
    "test-string-round-trip.scm.svn-base" "test-string-ref.scm.svn-base"
    "test-string-quasiquote.scm.svn-base"
    "test-string-port-translate-test.scm.svn-base"
    "test-string-length-append.scm.svn-base"
    "test-string-first-character-substring.scm.svn-base"
    "test-string-fold.scm.svn-base" "test-string-equal.scm.svn-base"
    "test-string-drop.scm.svn-base" "test-string-drop-right.scm.svn-base"
    "test-string-comparison-cs.scm.svn-base"
    "test-string-comparison-ci.scm.svn-base" "test-spann.scm.svn-base"
    "test-span.scm.svn-base" "test-read-line.scm.svn-base"
    "test-read-complex.scm.svn-base" "test-quasiquote.scm.svn-base"
    "test-quasiquote-reader.scm.svn-base"
    "test-proto-objects.scm.svn-base"
    "test-proto-objects-reader.scm.svn-base" "test-popn.scm.svn-base"
    "test-parse-glob-pattern.scm.svn-base" "test-pair-fold.scm.svn-base"
    "test-pair-fold-right.scm.svn-base"
    "test-numeric-simple-comparison.scm.svn-base"
    "test-numeric-equality.scm.svn-base"
    "test-numeric-complex-comparison.scm.svn-base"
    "test-number-type-predicates.scm.svn-base"
    "test-number-multiply.scm.svn-base" "test-number-io.scm.svn-base"
    "test-number-exactness.scm.svn-base" "test-number-divide.scm.svn-base"
    "test-number-bitwise.scm.svn-base" "test-number-add.scm.svn-base"
    "test-nan-infinitep.scm.svn-base"
    "test-multiple-read-from-string.scm.svn-base" "test-misc.scm.svn-base"
    "test-matches-globp.scm.svn-base" "test-mapping.scm.svn-base"
    "test-map-n.scm.svn-base" "test-map-2.scm.svn-base"
    "test-map-1.scm.svn-base" "test-make-vector.scm.svn-base"
    "test-macro-expansion.scm.svn-base"
    "test-listp-dotted-listp.scm.svn-base"
    "test-list-to-filename.scm.svn-base" "test-list-setn.scm.svn-base"
    "test-list-ref.scm.svn-base" "test-list-let.scm.svn-base"
    "test-list->vector.scm.svn-base" "test-linear-execution.scm.svn-base"
    "test-letstar.scm.svn-base" "test-letrec-lambda.scm.svn-base"
    "test-let.scm.svn-base" "test-key-list-to-hash.scm.svn-base"
    "test-is-file-basenamep.scm.svn-base"
    "test-is-directory-filenamep.scm.svn-base"
    "test-is-directory-basenamep.scm.svn-base"
    "test-integer-to-char.scm.svn-base"
    "test-input-port-translate-mode.scm.svn-base"
    "test-input-port-port-locations.scm.svn-base"
    "test-string-upcase-string-downcase.scm.svn-base"
    "test-hash-type.scm.svn-base" "test-ieee754-bits.scm.svn-base"
    "test-hash-setn.scm.svn-base" "test-hash-ref.scm.svn-base"
    "test-hash-ref-star.scm.svn-base" "test-hash-copy.scm.svn-base"
    "test-generic-function-inheritance-arity-2.scm.svn-base"
    "test-generic-function-inheritance-arity-1.scm.svn-base"
    "test-or.scm.svn-base" "test-handler-bind.scm.svn-base"
    "test-generic-function-arity-4.scm.svn-base"
    "test-generic-function-arity-2.scm.svn-base"
    "test-generic-function-arity-1.scm.svn-base"
    "test-generic-function-return-value.scm.svn-base"
    "test-format.scm.svn-base" "test-for-each-n.scm.svn-base"
    "test-for-each-2.scm.svn-base" "test-for-each-1.scm.svn-base"
    "test-fold.scm.svn-base"
    "test-flush-whitespace-string-input.scm.svn-base"
    "test-fold-right.scm.svn-base" "test-filename-to-list.scm.svn-base"
    "test-filename-path.scm.svn-base"
    "test-filename-extension.scm.svn-base"
    "test-filename-basename.scm.svn-base"
    "test-hash-table-complex-key.scm.svn-base"
    "test-fast-io-shared-structure.scm.svn-base"
    "test-fast-io-lists-and-vectors.scm.svn-base"
    "test-fast-io-strings-and-characters.scm.svn-base"
    "test-fast-io-circular-structure.scm.svn-base"
    "test-fast-io-hashes.scm.svn-base" "test-fast-io-atoms.scm.svn-base"
    "test-everyp.scm.svn-base" "test-error.scm.svn-base"
    "test-error-handler.scm.svn-base" "test-fast-io-symbols.scm.svn-base"
    "test-equalp-simple-hash.scm.svn-base" "test-equalp-atom.scm.svn-base"
    "test-eq-hash-table.scm.svn-base" "test-dynamic-let.scm.svn-base"
    "test-drop.scm.svn-base" "test-drop-while.scm.svn-base"
    "test-dolist.scm.svn-base" "test-define-internal.scm.svn-base"
    "test-define-external.scm.svn-base"
    "test-complex-rect-equalp.scm.svn-base"
    "test-complex-rect-accessors.scm.svn-base"
    "test-equalp-simple-list-vector.scm.svn-base"
    "test-character-to-string.scm.svn-base"
    "test-character-read-write.scm.svn-base"
    "test-canonicalize-filename.scm.svn-base" "test-charp.scm.svn-base"
    "test-breakn.scm.svn-base" "test-break.scm.svn-base"
    "test-booleanp.scm.svn-base" "test-class-graph.scm.svn-base"
    "test-begin-2.scm.svn-base" "test-apply-coverage.scm.svn-base"
    "test-binary-integer-io.scm.svn-base"
    "test-EQUALp-simple-list-vector.scm.svn-base" "test-anyp.scm.svn-base"
    "test-EQUALp-simple-hash.scm.svn-base"
    "test-EQUALp-shared-list.scm.svn-base" "test-and.scm.svn-base"
    "test-EQUALp-atom.scm.svn-base" "test-EQUALp-shared-hash.scm.svn-base"
    "test-do.scm.svn-base" "run-tests.scm.svn-base"
    "test-begin-1.scm.svn-base" "util-types.cpp" "util-types.o" "os.o"
    "os.cpp" "sys.o" "sys.cpp" "util-tchar.h" "format" "entries"
    "util-tchar.h.svn-base" "util-types.cpp.svn-base" "sys.cpp.svn-base"
    "os.cpp.svn-base" "big_csv_file.csv" "big_csv_file.sxp" "bench.scm~"
    "bench.scm" "benchmark_results.scm" "gabriel-scm.tar.gz" "format"
    "entries" "gabriel-scm.tar.gz.svn-base" "gabriel-scm.tar.gz.svn-base"
    "format" "entries" "big_csv_file.sxp.svn-base"
    "big_csv_file.csv.svn-base" "benchmark_results.scm.svn-base"
    "bench.scm.svn-base" "unit-test.scm~" "unit-test.scm" "s-core.scm~"
    "s-core.scm" "fasl-write.scm" "fasl-compiler0.scm" "fasl-compiler.scm"
    "format" "entries" "unit-test.scm.svn-base" "s-core.scm.svn-base"
    "fasl-write.scm.svn-base" "fasl-compiler0.scm.svn-base"
    "fasl-compiler.scm.svn-base" "README.txt" "fasl-write.sco"
    "fasl-compiler.sco" "fasl-compiler0.sco" "s-core.sco"
    "invoke-repl.sco" "scc0" "Makefile" "format" "entries"
    "fasl-compiler.sco.svn-base" "fasl-write.sco.svn-base"
    "fasl-compiler0.sco.svn-base" "scc0.svn-base" "s-core.sco.svn-base"
    "fasl-compiler.sco.svn-base" "invoke-repl.sco.svn-base"
    "fasl-write.sco.svn-base" "fasl-compiler0.sco.svn-base"
    "scc0.svn-base" "Makefile.svn-base" "README.txt.svn-base"
    "s-core.sco.svn-base" "format" "entries" "scc.svn-base"
    "scan-types.h.svn-base" "sys.h.svn-base" "assert.h.svn-base"
    "TO*DO.svn-base" "scc.svn-base" "des-cport-test.i.svn-base"
    "hash-table.cpp.svn-base" "fasl-loader.cpp.svn-base"
    "des-port.cpp.svn-base" "vcsh.cpp.svn-base" "scanlib.cpp.svn-base"
    "mt19937.cpp.svn-base" "oblist.cpp.svn-base" "Makefile.svn-base"
    "to-c-source.cpp.svn-base" "scan.h.svn-base" "error.cpp.svn-base"
    "number.cpp.svn-base" "fasl-dump.cpp.svn-base" "system.cpp.svn-base"
    "main.cpp.svn-base" "fasl-utils.cpp.svn-base" "array.cpp.svn-base"
    "fasl-file.h.svn-base" "list.cpp.svn-base" "memory.cpp.svn-base"
    "diagnostics.cpp.svn-base" "io.cpp.svn-base" "string.cpp.svn-base"
    "scan.rc.svn-base" "reader.cpp.svn-base" "evaluator.cpp.svn-base"
    "mt19937.h.svn-base" "macro.cpp.svn-base" "scansh0.cpp.svn-base"))

(defbench globbing
  (account
   (filter-by-glob *glob-test-strings* "*.cpp")
   (filter-by-glob *glob-test-strings* "test*.*")
   (filter-by-glob *glob-test-strings* "*.*")))

(defbench qsorting
  (account
   (qsort *glob-test-strings* string<)))

(defbench fast-queue
  (account
   (let ((q (scheme::%make-q)))
     (bench-repeat 100000
             (scheme::%q-enqueue! 1 q))
     (scheme::%q-items q))))

(defbench slow-queue
  (account
   (let ((q (make-queue)))
     (bench-repeat 100000
             (q-enqueue! 1 q))
     (q-items q))))

(defbench fibonacci
  (define (fib x)
    (if (< x 2)
        1
        (+ (fib (- x 1)) (fib (- x 2)))))
  (account
   (fib 21)))

(define-structure benchmark-structure
  f1 f2 f3)

(defbench structure/make-0-args
  (account
   (bench-repeat 100000
           (make-benchmark-structure))))

(defbench structure/make-1-args
  (account
   (bench-repeat 100000
           (make-benchmark-structure :f1 1))))

(defbench structure/make-3-args
  (account
   (bench-repeat 100000
           (make-benchmark-structure :f1 1 :f2 2 :f3 3))))


(defbench structure/ref
  (let ((structures (map #L(make-benchmark-structure) (iseq 1 100000))))
    (account
     (dolist (structure structures)
       (benchmark-structure-f1 structure)
       (benchmark-structure-f2 structure)
       (benchmark-structure-f3 structure)))))

(defbench structure/set!
  (let ((structures (map #L(make-benchmark-structure) (iseq 1 100000))))
    (account
     (dolist (structure structures)
       (set-benchmark-structure-f1! structure :foo)
       (set-benchmark-structure-f2! structure :bar)
       (set-benchmark-structure-f3! structure :baz)))))

(defbench structure/read
  (account
   (bench-repeat 10000
           (read-from-string "#S(benchmark-structure :f1 1 :f2 2 :f3 3)"))))

(defbench structure/write
  (let ((op (open-null-port)))
    (account
     (bench-repeat 100000
             (write #S(benchmark-structure :f1 1 :f2 2 :f3 3) op)))))

(defbench structure/fasl-write
  (let ((test-filename (temporary-file-name "sct")))
    (with-fasl-file os test-filename
      (account
       (bench-repeat 65536
         (fasl-write (make-benchmark-structure :f1 1 :f2 2 :f3 3) os))))
    (delete-file test-filename)))

(defbench structure/fasl-read
  (let ((test-filename (temporary-file-name "sct")))
    (with-fasl-file os test-filename
      (bench-repeat 65536
        (fasl-write (make-benchmark-structure :f1 1 :f2 2 :f3 3) os)))
    (account (fasl-load test-filename))
    (delete-file test-filename)))

(define (make-complex-structure-vector size)
  (let ((db (make-vector size)))
    (dotimes (ii size)
      (vector-set! db ii (make-benchmark-structure)))
    (dotimes (ii size)
      (set-benchmark-structure-f1! (vector-ref db ii) (vector-ref db (random size)))
      (set-benchmark-structure-f2! (vector-ref db ii) (vector-ref db (random size)))
      (set-benchmark-structure-f3! (vector-ref db ii) (vector-ref db (random size))))
    db))

(defbench structure/fasl-roundtrip-complex-sharing
  (let ((structures (make-complex-structure-vector 2000))
        (fn (temporary-file-name "scf")))
    (account
     (with-fasl-file of fn
       (fasl-write structures of))
     (with-port if (open-input-file fn :binary)
       (fasl-load if)))
    (delete-file fn)))

(define-proto benchmark-proto
  'f1 0
  'f2 0
  'f3 0)

(defbench proto/make-0-args
  (account
   (bench-repeat 100000
           (make-instance 'benchmark-proto))))

(defbench proto/make-1-args
  (account
   (bench-repeat 100000
           (make-instance 'benchmark-proto 'f1 1))))

(defbench proto/make-3-args
  (account
   (bench-repeat 100000
           (make-instance 'benchmark-proto 'f1 1 'f2 2 'f3 3))))

(defbench proto/ref
  (let ((protos (map #L(make-instance 'benchmark-proto) (iseq 1 100000))))
    (account
     (dolist (proto protos)
       (slot-ref proto 'f1)
       (slot-ref proto 'f2)
       (slot-ref proto 'f3)))))


(defbench proto/set!
  (let ((protos (map #L(make-instance 'benchmark-proto) (iseq 1 100000))))
    (account
     (dolist (proto protos)
       (slot-set! proto 'f1 :foo)
       (slot-set! proto 'f2 :bar)
       (slot-set! proto 'f3 :baz)))))


(defbench proto/read
  (account
   (bench-repeat 100000
           (read-from-string "#I(benchmark-proto f1 1 f2 2 f3 3)"))))

(defbench proto/write
  (let ((op (open-null-port)))
    (account
     (bench-repeat 100000
             (write  #I(benchmark-proto f1 1 f2 2 f3 3) op)))))


(defbench formatted-io/simple
  (let ((np (open-null-port)))
    (account
     (bench-repeat 10000
             (format np "Hello World")))))


(defbench formatted-io/printer
  (let ((np (open-null-port)))
    (account
     (bench-repeat 10000
             (format np "~a~s~a" 'foo "bar" "baz")))))

(defbench printer/basic
  (let ((np (open-null-port)))
    (account
     (bench-repeat 10000
             (write '('foo "bar" :keyword 1 1.0 1.0-2.0i
                           (1 2 3 . 4)
                           #(1 2 3 #(4 5 6)))
                    np)))))

(defbench opening-input-string
  (let ((text (make-string 1024 "01234567")))
    (account
      (bench-repeat 4096 (open-input-string text)))))

(defbench read-char-from-string
  (let ((text (make-string 32768 "*")))
    (account
     (let ((ip (open-input-string text)))
       (bench-repeat 32768 (read-char ip))))))

(defbench write-char-to-string
  (account
   (let ((op (open-output-string)))
     (bench-repeat 131072
       (write-char #\* op)))))

(defbench write-strings-to-string
  (account
   (let ((op (open-output-string)))
     (bench-repeat 32768
       (write-strings op "****")))))

(defbench read/boolean
  (let ((text (string-append "(" (make-string 8192 "#f #t ") ")")))
    (account
     (bench-repeat 16
             (read-from-string text)))))

(defbench read/character
  (let ((text (string-append "(" (make-string 4096 "#\\l #\\i #\\s #\\p ") ")")))
    (account
     (bench-repeat 16
             (read-from-string text)))))


(defbench read/string
  (let ((text (string-append "(" (make-string 256 " \"hello world, this is a test\" ") ")")))
    (account
     (bench-repeat 256
             (read-from-string text)))))

(defbench read/float
  (let ((text (string-append "(" (make-string 8192 " 3.14159 265.234 ") ")")))
    (account
     (bench-repeat 16
             (read-from-string text)))))

(defbench read/complex
  (let ((text (string-append "(" (make-string 8192 " 3.14159+265.234i ") ")")))
    (account
     (bench-repeat 16
             (read-from-string text)))))

(defbench read/symbol
  (let ((text (string-append "(" (make-string 2000 " hello :world this is a test ") ")")))
    (account
     (bench-repeat 16
             (read-from-string text)))))


(defbench read/vector
  (let ((text (string-append "(" (make-string 2000 " #(1) ") ")")))
    (account
     (bench-repeat 64
             (read-from-string text)))))

(define (test-benchmarks)
  "Run through all benchmarks as quickly as possible to check for runtime
   errors."
  (dynamic-let ((*estimate-min-test-duration* 0.001)
                (*repeat-only-once* #t))
    (bench)))

(define (fast-bench)
  "Generate some quick and dirty benchmarks for the purpose of testing the
   benchmark suite. The results from these tests shouldn't be trusted
   for actual benchmarking purposes."
  (dynamic-let ((*estimate-min-test-duration* 0.2))
    (bench 'funcall-inline
	   'funcall-inline-args
	   'funcall-local
	   'funcall-local-args
           'mandelbrot-cplx
           'qsorting
           'fast-queue
           'slow-queue
           'fibonacci)))

(define (hash-bench)
  (bench 'hash-set!-seq-numbers
         'hash-ref-seq-numbers
         'hash-set!-eq
         'hash-ref-eq
         'hash-set!-equal
         'hash-ref-equal))

(for-each load (directory "gabriel*.scm"))
(load "mandelbrot.scm")



