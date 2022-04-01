
;;;; unit-test.scm -- 
;;;;
;;;; A set of facilities for implementing unit test suites.
;;;;
;;;; (C) Copyright 2001-2015 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define-package "unit-test"
  (:uses "scheme")
  (:exports "define-test"
            "all-tests"
            "run-tests"
            "check"
            "check-for"))

;; REVISIT: check-for (multiple checks over a set of bindings of a given var)
;; REVISIT: check that shows expected/actual values

;;;; Unit test setup

;; A few Configuration options

(define *show-check-conditions* (environment-variable/boolean "UNIT_TEST_SHOW_CHECK_CONDITIONS" #f)) 
(define *show-test-errors* (environment-variable/boolean "UNIT_TEST_SHOW_ERRORS" #t))
(define *show-failed-test-forms* (environment-variable/boolean "UNIT_TEST_SHOW_FAILED_FORMS" #f))
(define *show-failed-test-causes* (environment-variable/boolean "UNIT_TEST_SHOW_TEST_CAUSES" #f))

;;;; Test output

(define *test-output-port* (current-output-port))

(define (message . args)
  "Write a test output message using format-style arguments."
  (apply format *test-output-port* args)
  (flush-port *test-output-port*))

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

(define (add-test! test-name source-location runner)
  "Extend the unit test dictionary to include a test named
   <test-name> implemented by the function <test-fn>."
  (when (hash-has? *test-cases* test-name)
    (warning "Redefining test case ~s" test-name))
  (hash-set! *test-cases* test-name {:name test-name
                                     :source-location source-location
                                     :runner runner})
  (values))

(define (all-tests)
  "Returns a list of all currently defined tests, sorted in alphabetical
   order by test name."
  (qsort (hash-keys *test-cases*) string< full-symbol-name))

(define (test-by-name test-name)
  (hash-ref *test-cases* test-name #f))

;;;; Test result reporting

(define (failure-result? test-result)
  (not (eq? :check-succeeded (:outcome test-result))))

(define (report-test-result! test-result)
  (push! test-result *check-results*))

(define (test-failed cause)
  (report-test-result! {:test-case *running-test-case*
                        :condition-form #f
                        :source-location #f
                        :outcome :toplevel-test-failure
                        :cause cause}))

;;;; Condition checking

(define (check-condition condition-passed? condition-form source-location)
  (define (condition-passed)
    (report-test-result! {:test-case *running-test-case*
                          :condition-form  condition-form
                          :source-location source-location
                          :outcome         :check-succeeded
                          :cause #f}))

  (define (condition-failed failure-type . cause)
    (report-test-result! {:test-case *running-test-case*
                          :source-location source-location
                          :condition-form  condition-form
                          :outcome         failure-type
                          :cause           (car cause)})
    (throw *check-escape* #f))
  (when *show-check-conditions*
    (message "Checking Condition: ~s\n" condition-form))
  (catch *check-escape*
    (with-unexpected-escape-handler condition-failed
      (if (condition-passed?)
          (condition-passed)
          (condition-failed :test-failed #f)))))

(defmacro (check condition)
  `(check-condition (lambda () ,condition) ',condition ',(form-source-location condition)))

(defmacro (check-for binding-form condition)
  `(dolist ,binding-form (check ,condition)))

;;;; Unit test execution

(define (run-test test-case)
  (catch *test-escape*
    (dynamic-let ((*running-test-case* test-case)
                  (*check-results* ()))
      (with-unexpected-escape-handler (lambda args
                                        (test-failed args)
                                        (throw *test-escape* *check-results*))
        ((:runner test-case)))
      *check-results*)))

(define (execute-test test-case)
  (dynamic-let ((*error* *show-test-errors*)
                (*info* *show-test-errors*))
    (message "; ~a..." (:name test-case))
    (let* ((check-results (run-test test-case))
           (result-count (length check-results))
           (failure-count (length (filter failure-result? check-results))))
      (message "~72T~a check~a.~a\n"
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
  (aif (:source-location result)
       (location-string it)
       (aif (:source-location (:test-case result))
            (location-string it)
            "?:?:?")))

(define (show-check-fails check-results)
  (let ((failure-results (filter failure-result? check-results)))
    (unless (null? failure-results)
      (message "--------------------------------\n")
      (dolist (failure-result (reverse failure-results))
        (message "~a: FAIL (~a) in ~s\n"
                 (:location-string failure-result)
                 (:outcome failure-result)
                 (:name (:test-case failure-result)))
        (when *show-failed-test-forms*
          (message " form >  ~s\n" (:condition-form failure-result)))
        (when *show-failed-test-causes*
          (message " cause >  ~s\n" (:cause failure-result)))
        (when (eq? (:outcome failure-result) :runtime-error)
          (message " >  caused by: ~I\n"
                  (hash-ref (:cause failure-result) :message)
                  (hash-ref (:cause failure-result) :args))))
      (message "\n\n~a Failure(s)!\n" (length failure-results))
      (message "--------------------------------\n"))))

(define (run-tests :optional (tests-to-run (all-tests)))
  (catch 'abort-tests
    (handler-bind ((user-break
                    (lambda ()
                      (info "***USER BREAK***")
                      (throw 'abort-tests (list 'user-break)))))
      (catch 'error-escape
        (newline)
        (let ((test-results (append-map execute-test
                                        (map test-by-name tests-to-run))))
          (show-check-fails (filter failure-result? test-results))
          (message "\n~a total checks run.\n" (length test-results))
        (null? (filter failure-result? test-results)))))))

