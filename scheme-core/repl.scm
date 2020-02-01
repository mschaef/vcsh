
;;;; repl.scm --
;;;;
;;;; The master read-eval-print-loop
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;;;; The REPL history facility

(define *1)
(define *2)
(define *3)

(define (extend-repl-history! value)
  "Adds <value> to the REPL history."
  (set! *3 *2)
  (set! *2 *1)
  (set! *1 value))

(define (repl-choose choices :optional (prompt "which one?") (simple? #t))
  "Allow a user at the REPL to select from the list of <choices>. <prompt>
   is printed to prompt the user. <simple?> selects simple mode. In simple
   mode, the user is presented with the printed version of each choice.
   If simple mode is not selected, then each choice must be a cons cell
   of the format ( <choice-displayed-object> . <choice-selected-object>).
   If the user sat on the pot and didn't choose anything, none-chosen is
   thrown with () as the return value."
  (runtime-check list? choices)
  (newline)
  (let ((l (length choices)))
    (let display-next ((choices choices) (i 0))
      (unless (null? choices)
        (if simple?
            (format #t "~a> ~s\n" i (car choices))
            (format #t "~a> ~a\n" i (caar choices)))
        (display-next (cdr choices) (+ i 1))))
    (let try-again ()
      (format #t "~a (n for none)> " prompt)
      (let ((choice (read)))
        (cond ((and (exact? choice) (>= choice 0) (< choice l))
               (let ((choice-value (if simple?
                                       (list-ref choices choice)
                                       (cdr (list-ref choices choice)))))
                 (format #t " chosen: ~a\n" (if simple?
                                                choice-value
                                                (car (list-ref choices choice))))
                 choice-value))
              ((member choice '(n no none #f nothing :n :no :none :nothing))
               (throw 'none-chosen ()))
              (#t
               (format (current-error-port)
                       "Please enter a number in the range [~a, ~a].\n" 0 (- l 1))
               (try-again)))))))

(define (quoted-abbrev? abbrev)
  (eq? (third abbrev) :quote))

(define (show-repl-abbreviations abbrevs)
  (dynamic-let ((*info* #f))
    (format (current-error-port) "~&Current REPL Abbreviations:\n")
    (format (current-error-port) "~&------------------------------------------------\n")
    (dolist (abbrev (qsort abbrevs string< #L(string-downcase (symbol-name (first _)))))
      (format (current-error-port) "~&~2T~s~10T~a~a\n"
              (first abbrev) (second abbrev) (if (quoted-abbrev? abbrev)
                                                 "'" "")))))

(define *repl-abbreviations* ())
(define *repl-abbreviations-enabled* #t)

(define (extend-repl-abbreviations! abbreviation fn quoted?)
  (runtime-check keyword? abbreviation)
  (when (assoc abbreviation *repl-abbreviations*)
    (info "Duplicate REPL abbreviation: ~s" abbreviation))
  (push! `(,abbreviation ,fn ,@(if quoted? '(:quote) ()))
         *repl-abbreviations*))

(defmacro (define-repl-abbreviation abbreviation fn)
  (let ((fn (if (pair? fn) (second fn) fn))
        (quoted? (and (pair? fn) (eq? 'quote (first fn)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (extend-repl-abbreviations! ',abbreviation ',fn ,quoted?))))

(define-repl-abbreviation :crh clear-repl-history!)
(define-repl-abbreviation :x exit-repl)
(define-repl-abbreviation :X exit)
(define-repl-abbreviation :top toplevel)

(define (call-with-default-read-error-handling fn)
  "Calls <fn> with default read error handling. Default read
   error handling is to print an error message to the current
   error port and throw 'read-failed."
  (handler-bind ((read-error (lambda (error-type port location)
                               (format (current-error-port)
                                       "; Reader Error: ~a @ (~a,~a)\n" error-type
                                       (car location)
                                       (cdr location))
                               (throw 'read-failed ())))
                 (runtime-error (lambda (error-info)
                                  (format (current-error-port) "; Error while reading:\n")
                                  (show-runtime-error error-info)
                                  (throw 'read-failed ()))))
    (fn)))

(defmacro (with-default-read-error-handling . code)
  "Runs <code> with default read error handling. Default read
   error handling is to print an error message to the current
   error port and throw 'read-failed."
  `(call-with-default-read-error-handling (lambda () ,@code)))


;;; The main read-eval-print loop itself, including several ways to
;;; end it.

(define *repl-level* 0)

(define (repl-prompt repl-level)
  (dynamic-let ((*print-addresses* #f))
    (if (= repl-level 1)
        (format #t "~a> " (package-name *package*))
        (format #t "~a:~a> " (package-name *package*) repl-level))))

(define (read-abbreviated-list :optional (ip (current-input-port)))
  "Read an abbreviated list from <ip>. An abbreviated list is a list
   that closes on the first #\\newline that's read outside of a recursive
   call to (read). It is so named because it is the basis for the syntax
   used for REPL abbreviations."
  (let loop ((xs ()))
    (let next-char ()
      (case (peek-char ip)
        ((#\;)
         (read-text-until-character ip #\newline)
         (next-char))
        ((#\tab #\cr #\space)
         (read-char ip)
         (next-char))
        ((#\newline)
         (read-char ip)
         (reverse xs))
        (#t
         (loop (cons (read ip) xs)))))))

(define (call-with-repl-error-handling context-name fn)
  "Evaluates <form> in environment <env>, suppressing errors and returning
  return values as a list, rather than as multiple values."
  (catch 'repl-abort-evaluation
    (handler-bind ((uncaught-throw ;; REVISIT: Refactor this into an external fn
                    (lambda (tag retval)
                      ;; REVISIT: Correct this port
                      (format #t "Uncaught throw during ~a: ~a\n" context-name tag)
                      (throw 'repl-abort-evaluation (list retval))))
                   (unhandled-abort
                    (lambda (condition args)
                      (format #t "Unhandled abort during ~a: ~a\n\nargs=~a\n\n" context-name condition args)
                      (throw 'repl-abort-evaluation (list condition))))
                   (user-break
                    (lambda ()
                      (info "***USER BREAK***")
                      (throw 'repl-abort-evaluation (list 'user-break)))))
      (catch 'error-escape
        (fn)))))

(defmacro (with-repl-error-handling context-name . code)
  "Evaluates <form> in environment <env>, suppressing errors and returning
  return values as a list, rather than as multiple values."
  `(call-with-repl-error-handling ,context-name
                                  (lambda () ,@code)))

(define (repl-read abbrevs)
  "Reads a form from the current input port, honoring REPL
   abbreviations contained in <abbrevs>."
  (define (repl-abbreviated-form args)
    (define (quote-all xs)
      (map (lambda (x) `',x) xs))
    (aif (assoc (car args) abbrevs)
         (cons (cadr it) (if (quoted-abbrev? it) (quote-all (cdr args)) (cdr args)))
         (begin
           (info "Unknown REPL abbreviation: ~a\n" (car args))
           (show-repl-abbreviations abbrevs)
           (newline)
           (throw 'read-failed))))
  (flush-port (current-error-port))
  (flush-port (current-output-port))
  (let ((first-char (flush-whitespace)))
    (with-repl-error-handling "pre-read-hook"
     (invoke-hook '*repl-pre-read-hook*))
    (let ((input-form (with-default-read-error-handling (read))))
      (if (and (keyword? input-form) *repl-abbreviations-enabled*)
          (repl-abbreviated-form (cons input-form
                                       (with-default-read-error-handling (read-abbreviated-list))))
          input-form))))

(define (repl-eval form :optional (env ()))
  "Evaluates <form> in environment <env>, suppressing errors and returning
  return values as a list, rather than as multiple values."
  (with-repl-error-handling "evaluation"
      (mvbind results (time (eval `(begin-user-stack ,form) env))
        results)))

(define *repl-pre-read-hook* ())
(define *repl-pre-print-value-hook* ())
(define *repl-post-hook* ())

(define *last-repl-error* #f)

(define (repl-print . values)
  "Prints each of <values> in a nicely formatted, REPL-like, way, updating
   the REPL history."
  (dolist (value values)
    (catch 'repl-do-not-print
      (with-repl-error-handling "pre-print-value-hook"
       (invoke-hook '*repl-pre-print-value-hook* value))
      (handler-bind ((runtime-error (lambda (error-info)
                                      (set! *last-repl-error* error-info)
                                      (format (current-error-port) "; REPL Print Error: ~I\n"
                                              (hash-ref error-info :message)
                                              (hash-ref error-info :args))
                                      (throw 'repl-do-not-print))))
        (extend-repl-history! value)
        (dynamic-let ((*print-readably* #f))
          (format #t "; *1 = ~s\n" value))))))

(define (exit :optional (retval 0))
  "Forcibly shuts down the interpreter, via a dynamic escape, causing the
   interpreter to return <retval> as a return value."
  (throw '%end-it-all retval))

(define (repl :optional (env ()))
  "Enters a read-eval-print-loop, which will print a  prompt, read a form,
   evaluate the input in environment <env>, and finally print the results.
   The loop ends when the input port reaches its end or the form evaluated
   throws an escape that escapes out of the loop.  The read-eval-print-loop
   establishes a dynamic escape named repl-escape."
  (dynamic-let ((*repl-level* (+ 1 *repl-level*)))
    (catch 'repl-escape
      (let loop ()
        (catch 'read-failed
          (repl-prompt *repl-level*)
          (let ((form (repl-read *repl-abbreviations*)))
            (when (eof-object? form)
              (exit 0))
            (apply repl-print (repl-eval form env))
            (with-repl-error-handling "post-hook"
             (invoke-hook '*repl-post-hook*)))) ;; REVISIT: error guard
        (loop)))))

(define (toplevel-repl)
  "Enters a toplevel Lisp REPL. A toplevel Lisp REPL is distinguished
by the fact that it has unique handling for the 'repl-toplevel dynamic
escape used to return to the toplevel REPL."
  (catch 'repl-quit
    (let loop ()
      (catch 'repl-toplevel
        (repl))
      (format #t "; Restarting toplevel REPL\n")
      (loop))))

(define (exit-repl)
  "Exits the most current read-eval-print-loop."
  (throw 'repl-escape)
  (values))

(define (toplevel)
  "Exits all read-eval-print-loops, and invokes a new toplevel loop."
  (throw 'repl-toplevel)
  (values))


(add-hook-function! '*repl-post-hook* 'do-idle-processing)
