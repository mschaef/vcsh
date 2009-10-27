;;;; unit-test.scm
;;;; August 10th, 2007
;;
;;
;; A set of facilities for implementing unit test suites.

(define-package "unit-test"
  (:uses "scheme")
  (:exports "define-test"
            "all-tests"
            "test"
            "test-case"
            "test-case/execution-order"
            "checkpoint"
            "non-local-escape?"
            "read-error?"
            "runtime-error?"
            "can-fast-io-round-trip?"
            "can-read/write-round-trip?"
            "fast-io-round-trip"
            "write-to-string"
            "type-of-tests"
            "load-tests"
            "values-eq?"
            "values-equal?"
            "dtu-lambda-list"))

;; TODO: unit tests over ranged x (ie: this should apply from all x in [0,255]
;; TODO: test-case that shows expected/actual values (should probably special case based on nested form)

;;;; Unit test setup

;; Enlarge the heap for performance

(dynamic-let ((*info* #f))
  (format #t "Expanding heap...\n")
  (enlarge-heap 50))

;; A few Configuration options

(define *show-check-conditions* #f) ;; REVISIT: flag configurable
(define *show-test-messages* #f)
(define *force-gc-on-check* #f)
(define *show-failed-test-forms* #f)

;;;; Unit test definitions.

(define *test-cases* (make-hash :eq))
(define *running-test-case* #f)


(define *check-count* 0)
(define *total-check-count* 0)
(define *check-fail-count* 0)
(define *check-fails* '())
(define *check-escape* (gensym "check-escape"))
(define *test-escape* (gensym "test-escape"))


(define (add-test! test-name test-fn)
  "Extend the unit test dictionary to include a test named
   <test-name> implemented by the function <test-fn>."
  (check symbol? test-name)
  (check closure? test-fn)
  (when (hash-has? *test-cases* test-name)
    (format #t "; WARNING: Redefining test case ~s" test-name))
  (hash-set! *test-cases* test-name test-fn)
  (values))

(define-structure test-failure
  name
  source-location
  form
  (cause :default #f))

(define (report-failure test-failure)
  (check test-failure? test-failure)
  (incr! *check-fail-count*)
  (push! test-failure *check-fails*))

(define (test-failed test-name location cause)
  (report-failure (make-test-failure :name            test-name
                                     :source-location location
                                     :form            :TOPLEVEL-OF-TEST
                                     :cause           cause)))

(defmacro (define-test test-name . code)
  "Define a test named <test-name> implemented with the code in <code>."
  (let ((loc (form-source-location (car code))))
    `(add-test! ',test-name
                (lambda ()
                  (catch *test-escape*
                         (handler-bind
                        ((runtime-error (lambda args
                                          (test-failed ',test-name ',loc (cons 'runtime-error args))
                                          (throw *test-escape* #f)))
                         (unhandled-abort (lambda args
                                            (test-failed ',test-name ',loc (cons 'unhandled-abort args))
                                            (throw *test-escape* #f)))
                         (uncaught-throw (lambda args
                                           (test-failed ',test-name ',loc (cons 'uncaught-throw args))
                                           (throw *test-escape* #f))))
                      (dynamic-let ((*running-test-case* ',test-name))
                        ,@code)))))))

(define (all-tests)
  "Returns a list of all currently defined tests, sorted in alphabetical
   order by test name."
  (qsort (hash-keys *test-cases*)
         (lambda (x y) (> 0 (strcmp x y)))
         symbol-name))

;;;; Unit test execution

(define (execute-test test-name)
  (dynamic-let ((*error* *show-test-messages*)
                (*info* *show-test-messages*))
    (set! *check-count* 0)
    (set! *check-fail-count* 0)
    (format #t "; ~a..." test-name)
    ((hash-ref *test-cases* test-name))
    (indent 50)
    (format #t " ~a check~a." *check-count* (if (> *check-count* 1) "s" ""))
    (when (> *check-fail-count* 0) (format #t " (~a FAILED!!!)" *check-fail-count*))
    (newline)
    ))

(define (test-location-string form-loc)
  (if form-loc
      (format #f "~a:~a:~a" (car form-loc) (cadr form-loc) (cddr form-loc))
      (format #f "?:?:?")))

(define (load-tests :optional (load-directory #f) (filename-template "test*.scm"))
  "Loads all unit test files from the specified <load-directory>. The load directory
   defaults to the current directory. <filename-template> can optionally be specified
   to determine which files are considered unit tests."
 (dynamic-let ((*info* #t)) ;; REVISIT: Make this configurable?
   (for-each load (directory (if load-directory
                                 (make-filename load-directory filename-template)
                                 filename-template)))))

(define (test . specific-tests)
  (catch 'abort-tests
    (handler-bind ((uncaught-throw
                    (lambda (tag retval)
                      (format #t "Uncaught throw during evaluation: ~a\n" tag)
                      (throw 'abort-tests (list retval))))
                   (unhandled-abort
                    (lambda (condition args)
                      (format #t "Unhandled abort during evaluation: ~a\n\nargs=~a\n\n" condition args)
                      (throw 'abort-tests (list condition))))
                   (runtime-error
                    (lambda  args
                      (format #t "Unhandled runtime error during evaluation:\n\nargs=~a\n\n" args)
                      (dynamic-let ((*error* #t))
                        (apply handle-runtime-error args))))
                   (user-break
                    (lambda ()
                      (info "***USER BREAK***")
                      (throw 'abort-tests (list 'user-break)))))
      (catch 'error-escape
        (set! *total-check-count* 0)
        (set! *check-fails* '())
        (newline)
        (dolist (test-name (if (null? specific-tests) (all-tests) specific-tests))
          (execute-test test-name))
        (unless (null? *check-fails*)
          (format #t "--------------------------------\n")
          (dolist (failure (reverse *check-fails*))
                  (format #t "~a: failure in ~s\n"
                          (test-location-string (test-failure-source-location failure))
                          (test-failure-name failure))
                  (when *show-failed-test-forms*
                        (format #t " >  ~s\n" (test-failure-form failure)))
                  (awhen (test-failure-cause failure)
                         (format #t " >  caused by: ~s\n" it)))
          (format #t "\n\n~a Failure(s)!\n" (length *check-fails*))
          (format #t "--------------------------------\n"))
        (format #t "\n~a total checks run.\n" *total-check-count*)
        (null? *check-fails*)))))

;;;; Condition checking

(define (check-condition condition-passed? condition-form source-location)
  (define (condition-failed :optional (cause #f))
    (report-failure (make-test-failure :name            *running-test-case*
                                       :source-location source-location
                                       :form            condition-form
                                       :cause           cause)))
  (incr! *check-count*)
  (incr! *total-check-count*)
  (when *force-gc-on-check* (gc))
  (when *show-check-conditions*
    (format #t "Checking Condition: ~s\n" condition-form))
  (catch *check-escape*
    (unless (handler-bind ((runtime-error
                            (lambda args
                              (condition-failed (cons 'runtime-error args))
                              (throw *check-escape* #f)))
                           (unhandled-abort
                            (lambda args
                              (condition-failed (cons 'unhandled-abort args))
                              (throw *check-escape* #f)))
                           (uncaught-throw
                            (lambda args
                              (condition-failed (cons 'uncaught-throw args))
                              (throw *check-escape* #f))))
              (condition-passed?))
      (condition-failed))))

(defmacro (test-case condition)
  `(check-condition (lambda () ,condition) ',condition ',(form-source-location condition)))


;;;; Execution order checking.

(define *current-execution-order* '())

(defmacro (test-case/execution-order desired-order . code)
  (let ((desired-order (if (exact? desired-order) (iota desired-order 1 1) desired-order)))
    (with-gensyms (return-value-sym)
      `(dynamic-let ((*current-execution-order* '()))
         (let ((,return-value-sym (begin ,@code)))
           (let ((actual-order (reverse *current-execution-order*)))
             (test-case (equal? actual-order ',desired-order)))
           ,return-value-sym)))))

(define (checkpoint point-name . optional-return-value)
  (let ((point-name (if point-name point-name :does-not-execute)))
    (push! point-name *current-execution-order*)
    (if (null? optional-return-value)
        '()
        (car optional-return-value))))


;;;; Test utility functions and macros


(defmacro (non-local-escape? . code)
  "Returns a boolean indicating if <code> attempts a non-local-escape."
  (let ((escapes-sym (gensym)))
    `(let ((,escapes-sym #t))
       (catch-all
        ,@code
        (set! ,escapes-sym #f))
       ,escapes-sym)))

(define *runtime-error-escape* (gensym "error-escape"))

(define (throws-runtime-error? fn)
  "Returns #t if <fn> throws a runtime error, #f otherwise."
  (catch *runtime-error-escape*
    (handler-bind ((runtime-error (lambda arg
                                    (throw *runtime-error-escape*
                                           #t))))
      (fn)
      #f)))

(defmacro (runtime-error? . code)
  "Returns #t if the execution of <code> throws a runtime error, #f otherwise."
  `(throws-runtime-error? (lambda () ,@code)))

(define *read-error-escape* (gensym "error-escape"))


(define (throws-read-error? fn)
  "Returns #t if <fn> throws a read error, #f otherwise."
  (catch *read-error-escape*
    (handler-bind ((read-error (lambda arg
                                 (throw *read-error-escape*
                                        #t))))
      (fn)
      #f)))

(defmacro (read-error? . code)
  "Returns #t if the execution of <code> throws a read error, #f otherwise."
  `(throws-read-error? (lambda () ,@code)))

(define (read/write-round-trip obj)
  "Returns the result of writing <object> to a string and reading it back in.
   (Theoretically, this should be the same thing for printable objects.)"
  (let ((op (open-output-string))
        (new-obj (gensym)))
    (set-port-translate-mode! op #f)
    (write obj op)
    (catch-all
     (read-from-string (get-output-string op)))))

(define (can-read/write-round-trip? obj)
  "Given an object, determine if it can be written, and read back in,
   resulting in an equivalent object."
  (let ((obj2 (read/write-round-trip obj)))
    (equal? obj obj2)))

(define (fast-io-round-trip object :optional (dump-fasl-file? #f))
  "Returns the result of fast writing <object> to a binary file and
   reading it back in. (Theoretically, this should be the same thing.)"
  (let ((test-filename (temporary-file-name "sct")))
    (with-port p (open-output-file test-filename :binary)
      (with-fasl-stream s p
                        (fasl-write object s)))
    (let ((object ()))
      (when dump-fasl-file?
        (system (format #f ".\\fasl-dump ~a" test-filename)))
      (with-port p (open-input-file test-filename :binary)
        (set! object (fast-read p)))
      (delete-file test-filename)
      object)))

(define (can-fast-io-round-trip? obj)
  (let ((obj2 (fast-io-round-trip obj)))
    (EQUAL? obj obj2)))


(define (write-to-string x)
  (let ((o (open-output-string)))
    (set-port-translate-mode! o #f)
    (write x o)
    (get-output-string o)))

(defmacro (type-of-tests form type)
  `(begin
     (test-case (eq? (type-of ,form)               ,type))
     (test-case (is-a?        ,form                ,type))))

(define (shallow-list=? xs ys :optional (test eq?))
  (let loop ((xs xs) (ys ys))
    (cond ((and (null? xs) (null? ys))
           #t)
          ((and (pair? xs) (pair? ys))
           (and (test (car xs) (car ys))
                (loop (cdr xs) (cdr ys))))
          (#t
           (test xs ys)))))

(define (values-eq? actual-values . expecteds)
  (let ((values (values-bind actual-values xs xs)))
    (shallow-list=? values expecteds eq?)))

(define (values-equal? actual-values . expecteds)
  (let ((values (values-bind actual-values xs xs)))
    (shallow-list=? values expecteds equal?)))

(define (dtu-lambda-list procedure)
  (values-bind (procedure-lambda-list procedure) (real-ll source-ll)
    (if source-ll source-ll real-ll)))



(define (fiort x)
  (let ((xrt (fast-io-round-trip x)))
    (newline)
    (write x)
    (newline)
    (write xrt)
    (newline)
    xrt))
