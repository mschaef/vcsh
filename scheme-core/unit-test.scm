
;;;; unit-test.scm -- 
;;;;
;;;; A set of facilities for implementing unit test suites.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define-package "unit-test"
  (:uses "scheme")
  (:exports "define-test"
            "all-tests"
            "test"
            "test-case"
            "test-case/execution-order"
            "checkpoint"
            "checkpoint-order-of"
            "non-local-escape?"
            "read-error?"
            "runtime-error?"
            "can-fast-io-round-trip?"
            "can-read/write-round-trip?"
            "fast-io-round-trip"
            "load-tests"
            "values-eq?"
            "values-equal?"
            "dtu-lambda-list"))

;; REVISIT: unit tests over ranged x (ie: this should apply from all x in [0,255]
;; REVISIT: test-case that shows expected/actual values (should probably special case based on nested form)

;;;; Unit test setup

;; A few Configuration options

(define *show-check-conditions* #f) ;; REVISIT: flag configurable
(define *show-test-messages* #t)
(define *show-failed-test-forms* #f)
(define *show-failed-test-causes* #f)

;;;; Test output

(define (message . args)
  "Write a test output message using format-style arguments."
  (apply format #t args))

;;;; Error Trap

(define (call-with-unexpected-escape-handler fn handler)
  (let* ((escape-name (gensym "unexpected-escape")))
    (catch escape-name
      (handler-bind ((runtime-error
                      (lambda args
                        (apply handler :runtime-error args)
                        (throw escape-name)))
                     (unhandled-abort
                      (lambda args
                        (apply handler :unhandled-abort args)
                        (throw escape-name)))
                     (uncaught-throw
                      (lambda args
                        (apply handler :uncaught-throw args)
                        (throw escape-name))))
        (fn)))))

(defmacro (with-unexpected-escape-handler handler . code)
  `(call-with-unexpected-escape-handler (lambda () ,@code) ,handler))

;;;; Unit test definitions.

(define *test-cases* (make-hash))
(define *running-test-case* #f)

(define *check-results* '())
(define *check-escape* (gensym "check-escape"))
(define *test-escape* (gensym "test-escape"))

(define-structure test-case
  name
  source-location
  runner)

(define (add-test! test-name source-location runner)
  "Extend the unit test dictionary to include a test named
   <test-name> implemented by the function <test-fn>."
  (check symbol? test-name)
  (check closure? runner)
  (when (hash-has? *test-cases* test-name)
    (warning "Redefining test case ~s" test-name))
  (hash-set! *test-cases* test-name (make-test-case :name            test-name
                                                    :source-location source-location
                                                    :runner          runner))
  (values))

(define (all-tests)
  "Returns a list of all currently defined tests, sorted in alphabetical
   order by test name."
  (qsort (hash-keys *test-cases*) string<-ci symbol-name))

(define (test-by-name test-name)
  (hash-ref *test-cases* test-name #f))

;;;; Test result reporting

(define-structure test-result
  (test-case :default *running-test-case*)
  (condition-form :default #f)
  (source-location :default #f)
  outcome
  (cause :default #f))

(define (failure-result? test-result)
  (not (eq? :check-succeeded (test-result-outcome test-result))))

(define (report-test-result test-result)
  (check test-result? test-result)
  (push! test-result *check-results*))

(define (test-failed cause)
  (report-test-result
   (make-test-result :outcome         :toplevel-test-failure
                     :cause           cause)))

;;;; Condition checking

(define (check-condition condition-passed? condition-form source-location)
  (define (condition-passed)
    (report-test-result
     (make-test-result :source-location source-location
                       :condition-form  condition-form
                       :outcome         :check-succeeded)))

  (define (condition-failed failure-type . cause)
    (report-test-result
     (make-test-result :source-location source-location
                       :condition-form  condition-form
                       :outcome         failure-type
                       :cause           (car cause)))
    (throw *check-escape* #f))
  (when *show-check-conditions*
    (message "Checking Condition: ~s\n" condition-form))
  (catch *check-escape*
    (with-unexpected-escape-handler condition-failed
       (if (condition-passed?)
           (condition-passed)
           (condition-failed :test-failed #f)))))

(defmacro (test-case condition)
  `(check-condition (lambda () ,condition) ',condition ',(form-source-location condition)))

;;;; Unit test execution

(define (run-test test-case)
  (catch *test-escape*
      (dynamic-let ((*running-test-case* test-case)
                    (*check-results* ()))
        (with-unexpected-escape-handler (lambda args
                                          (test-failed args)
                                          (throw *test-escape* *check-results*))
           ((test-case-runner test-case)))
        *check-results*)))

(define (execute-test test-case)
  (dynamic-let ((*error* *show-test-messages*)
                (*info* *show-test-messages*))
    (message "; ~a..." (test-case-name test-case))
    (let* ((check-results (run-test test-case))
           (result-count (length check-results))
           (failure-count (length (filter failure-result? check-results))))
      (message "~50T~a check~a.~a\n"
              result-count
              (if (> result-count 1) "s" "")
              (if (> failure-count 0) (format #f " (~a FAILED!)" failure-count) ""))
      check-results)))

;;;; Test definition

(defmacro (define-test test-name . code)
  "Define a test named <test-name> implemented with the code in <code>."
  (let ((loc (form-source-location (car code))))
    `(add-test! ',test-name ',loc (lambda () ,@code))))

;;;; Test Runner

(define (load-tests :optional (load-directory #f) (filename-template "test*.scm"))
  "Loads all unit test files from the specified <load-directory>. The load directory
   defaults to the current directory. <filename-template> can optionally be specified
   to determine which files are considered unit tests."
 (dynamic-let ((*info* #t))
   (for-each load (directory (if load-directory
                                 (make-filename load-directory filename-template)
                                 filename-template)))))

(define (test-result-location-string result)
  (define (location-string location)
    (format #f "~a:~a:~a" (car location) (cadr location) (cddr location)))
  (aif (test-result-source-location result)
       (location-string it)
       (aif (test-case-source-location (test-result-test-case result))
            (location-string it)
            "?:?:?")))

(define (show-check-fails check-results)
  (let ((failure-results (filter failure-result? check-results)))
    (unless (null? failure-results)
      (message "--------------------------------\n")
      (dolist (failure-result (reverse failure-results))
        (message "~a: FAIL (~a) in ~s\n"
                 (test-result-location-string failure-result)
                 (test-result-outcome failure-result)
                 (test-case-name (test-result-test-case failure-result)))
        (when *show-failed-test-forms*
          (message " form >  ~s\n" (test-result-condition-form failure-result)))
        (when *show-failed-test-causes*
          (message " cause >  ~s\n" (test-result-cause failure-result)))
        (when (eq? (test-result-outcome failure-result) :runtime-error)
          (message " >  caused by: ~I\n"
                  (hash-ref (test-result-cause failure-result) :message)
                  (hash-ref (test-result-cause failure-result) :args))))
      (message "\n\n~a Failure(s)!\n" (length failure-results))
      (message "--------------------------------\n"))))

(define (test :optional (tests-to-run (all-tests)))
  (catch 'abort-tests
    (handler-bind ((user-break
                    (lambda ()
                      (info "***USER BREAK***")
                      (throw 'abort-tests (list 'user-break)))))
      (catch 'error-escape
        (newline)
        (let ((test-results (append-map (lambda (test-name)
                                          (execute-test (test-by-name test-name)))
                                        tests-to-run)))
          (show-check-fails (filter failure-result? test-results))
          (message "\n~a total checks run.\n" (length test-results))
        (null? (filter failure-result? test-results)))))))

;;;; Execution order checking.

(define *current-execution-order* #f)

(define (checkpoint-order fn)
  (dynamic-let ((*current-execution-order* '()))
    (fn)
    (reverse *current-execution-order*)))

(defmacro (checkpoint-order-of . code)
  `(checkpoint-order
    (lambda () ,@code)))

(define (checkpoint point-name :optional return-value)
  (unless *current-execution-order*
    (error "Cannot make a checkpoint outside of a checkpoint block."))
  (push! point-name *current-execution-order*)
  return-value)

;;;; Error validation

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

;;;; Utility functions

(define (read/write-round-trip obj)
  "Returns the result of writing <object> to a string and reading it back in.
   (Theoretically, this should be the same thing for printable objects.)"
  (catch-all
   (read-from-string (write-to-string obj))))

(define (can-read/write-round-trip? obj)
  "Given an object, determine if it can be written, and read back in,
   resulting in an equivalent object."
  (let ((obj2 (read/write-round-trip obj)))
    (equal? obj obj2)))

(define (fast-io-round-trip object :optional (dump-fasl-file? #f))
  "Returns the result of fast writing <object> to a binary file and
   reading it back in. (Theoretically, this should be the same thing.)"
  (let ((test-filename (temporary-file-name "sct")))
    (with-port p (open-file test-filename :mode :write :encoding :binary)
      (with-fasl-stream s p
                        (fasl-write s object)))
    (let ((object ()))
      (when dump-fasl-file?
        (system (format #f "../vm/fasl-dump ~a" test-filename)))
      (with-port p (open-file test-filename :encoding :binary)
        (set! object (fast-read (make-fasl-reader p))))
      (delete-file test-filename)
      object)))

(define (can-fast-io-round-trip? obj)
  (let ((obj2 (fast-io-round-trip obj)))
    (EQUAL? obj obj2)))

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
  (let ((values (mvbind xs actual-values xs)))
    (shallow-list=? values expecteds eq?)))

(define (values-equal? actual-values . expecteds)
  (let ((values (mvbind xs actual-values xs)))
    (shallow-list=? values expecteds equal?)))

(define (dtu-lambda-list procedure)
  (mvbind (real-ll source-ll) (procedure-lambda-list procedure)
    (if source-ll source-ll real-ll)))

