
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
(define *force-gc-on-check* #f)
(define *show-failed-test-forms* #f)
(define *show-failed-test-causes* #f)

;;;; Unit test definitions.

(define *test-cases* (make-identity-hash))
(define *running-test-case* #f)

(define *check-results* '())
(define *check-escape* (gensym "check-escape"))
(define *test-escape* (gensym "test-escape"))

(define-structure test-case
  name
  location
  runner)

(define (add-test! test-name location runner)
  "Extend the unit test dictionary to include a test named
   <test-name> implemented by the function <test-fn>."
  (check symbol? test-name)
  (check closure? runner)
  (when (hash-has? *test-cases* test-name)
    (format #t "; WARNING: Redefining test case ~s" test-name))
  (hash-set! *test-cases* test-name (make-test-case :name     test-name
                                                    :location location
                                                    :runner   runner))
  (values))

(define (all-tests)
  "Returns a list of all currently defined tests, sorted in alphabetical
   order by test name."
  (qsort (hash-keys *test-cases*) string<-ci symbol-name))

(define (test-by-name test-name)
  (hash-ref *test-cases* test-name #f))

;;;; Test result reporting

(define-structure test-result
  name
  source-location
  form
  (type :default :test-failed)
  (cause :default #f))

(define (failure-result? test-result)
  (not (eq? :passed (test-result-cause test-result))))

(define (report-test-result test-result)
  (check test-result? test-result)
  (push! test-result *check-results*))

(define (test-passed test-name location)
  (report-test-result (make-test-result :name            test-name
                                        :source-location location
                                        :form            :TOPLEVEL-OF-TEST
                                        :cause           :passed)))

(define (test-failed test-name location cause)
  (report-test-result (make-test-result :name            test-name
                                        :source-location location
                                        :form            :TOPLEVEL-OF-TEST
                                        :cause           cause)))

;;;; Condition checking

(define (check-condition condition-passed? condition-form source-location)
  (define (condition-failed failure-type :optional (cause #f))
    (report-test-result (make-test-result :name            *running-test-case*
                                          :source-location source-location
                                          :form            condition-form
                                          :type            failure-type
                                          :cause           cause))
    (throw *check-escape* #f))
  (when *force-gc-on-check* (gc))
  (when *show-check-conditions*
    (format #t "Checking Condition: ~s\n" condition-form))
  (catch *check-escape*
    (if (handler-bind ((runtime-error
                        (lambda (error-info) (condition-failed :runtime-error error-info)))
                       (unhandled-abort
                        (lambda args (condition-failed :unhandled-abort args)))
                       (uncaught-throw
                        (lambda args (condition-failed :uncaught-throw args))))
          (condition-passed?))
        (test-passed *running-test-case* source-location)
        (condition-failed :test-failed))))

(defmacro (test-case condition)
  `(check-condition (lambda () ,condition) ',condition ',(form-source-location condition)))

;;;; Unit test execution

(define (run-test test-case)
  (define (fail reason args)
    (test-failed (test-case-name test-case) (test-case-location test-case)
                 (cons reason args))
    (throw *test-escape* #f))
  (catch *test-escape*
    (handler-bind ((runtime-error   (lambda args (fail 'runtime-error   args)))
                   (unhandled-abort (lambda args (fail 'unhandled-abort args)))
                   (uncaught-throw  (lambda args (fail 'uncaught-throw  args))))
      (dynamic-let ((*running-test-case* (test-case-name test-case))
                    (*check-results* ()))
        ((test-case-runner test-case))
        *check-results*))))

(define (execute-test test-case)
  (dynamic-let ((*error* *show-test-messages*)
                (*info* *show-test-messages*))
    (format #t "; ~a..." (test-case-name test-case))
    (let* ((check-results (run-test test-case))
           (result-count (length check-results))
           (failure-count (length (filter failure-result? check-results))))
      (indent 50)
      (format #t " ~a check~a.~a\n"
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
 (dynamic-let ((*info* #t)) ;; REVISIT: Make this configurable?
   (for-each load (directory (if load-directory
                                 (make-filename load-directory filename-template)
                                 filename-template)))))

(define (test-location-string form-loc)
  (if form-loc
      (format #f "~a:~a:~a" (car form-loc) (cadr form-loc) (cddr form-loc))
      "?:?:?"))

(define (show-check-fails check-results)
  (let ((failures (filter failure-result? check-results)))
    (unless (null? failures)
      (format #t "--------------------------------\n")
      (dolist (failure (reverse failures))
        (format #t "~a: failure in ~s\n"
                (test-location-string (test-result-source-location failure))
                (test-result-name failure))
        (when *show-failed-test-forms*
          (format #t " form >  ~s\n" (test-result-form failure)))
        (when *show-failed-test-causes*
          (format #t " cause >  ~s\n" (test-result-cause failure)))
        (when (eq? (test-result-type failure) :runtime-error)
          (format #t " >  caused by: ~I\n"
                  (hash-ref (test-result-cause failure) :message)
                  (hash-ref (test-result-cause failure) :args))))
      (format #t "\n\n~a Failure(s)!\n" (length failures))
      (format #t "--------------------------------\n"))))

(define (test :optional (tests-to-run (all-tests)))
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
                    (lambda (error-info)
                      (format #t "; Unhandled runtime error during test evaluation.\n")
                      (dynamic-let ((*error* #t))
                        (show-runtime-error error-info))))
                   (user-break
                    (lambda ()
                      (info "***USER BREAK***")
                      (throw 'abort-tests (list 'user-break)))))
      (catch 'error-escape
        (newline)
        (let ((test-results (append-map (lambda (test-name)
                                          (execute-test (test-by-name test-name)))
                                        tests-to-run)))
          (show-check-fails (filter failure-result? test-results))
          (format #t "\n~a total checks run.\n" (length test-results))
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

