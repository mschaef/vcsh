
;;;; repl.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; The master read-eval-print-loop

;;;; The REPL history facility

(define *repl-history* #())

(define *repl-recent-history-length* 5)
(define *repl-recent-history-indices* ())

(define (extend-repl-history! value)
  "Adds <value> to the REPL history, returning the value's history
   index. If a value is new to the history, it is added to the end
   of the history, otherwise, the value in the history is reused."
  (let ((index (aif (vector-index #L(eq? _ value) *repl-history*)
                    it
                    (let ((new-index (length *repl-history*)))
                      (set! *repl-history* (vector-resize *repl-history* (+ new-index 1)))
                      (vector-set! *repl-history* new-index value)
                      new-index))))
    (set! *repl-recent-history-indices*
          (take-up-to (cons index *repl-recent-history-indices*)
                       *repl-recent-history-length*))
    index))

(define (repl-history-value index)
  "Returns the history value at index <index>."
  (cond ((not (number? index))
         (error "Bad REPL history index, expected a number.~s" index))
        ((>= index (length *repl-history*))
         (error "History index ~s not available." index))
        ((> (- index) (length *repl-recent-history-indices*))
         (error "Recent history index ~s not available." index))
        ((< index 0)
         (repl-history-value (nth *repl-recent-history-indices* (- (- index) 1))))
        (#t
         (vector-ref *repl-history* index))))

(define (clear-repl-history! . xs)
  "Clears the REPL history, and returns each of <xs> as a return
   value."
  (set! *repl-history* #())
  (apply values xs))


(define (read-history-form port)
  (read-char port)
  `(repl-history-value ,(read port)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-property! 'repl-history-value 'pretty-print-syntax "##")
  (set-char-syntax! *readsharp-syntax* #\# 'read-history-form))

(define (repl-choose choices :optional (prompt "which one?") (simple? #t))
  "Allow a user at the REPL to select from the list of <choices>. <prompt>
   is printed to prompt the user. <simple?> selects simple mode. In simple
   mode, the user is presented with the printed version of each choice.
   If simple mode is not selected, then each choice must be a cons cell
   of the format ( <choice-displayed-object> . <choice-selected-object>).
   If the user sat on the pot and didn't choose anything, none-chosen is
   thrown with () as the return value."
  (check list? choices)
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

(push! '(:crh clear-repl-history!) *repl-abbreviations*)
(push! '(:x exit-repl) *repl-abbreviations*)
(push! '(:X exit) *repl-abbreviations*)
(push! '(:top toplevel) *repl-abbreviations*)

(define-text
  :read-error-bad-symbol-syntax "Bad symbol syntax"
  :read-error-package-not-found "Package not found"
  :read-error-symbol-not-found-in-package "Symbol not found in package"
  :read-error-symbol-private-to-package "Symbol private to package"
  :reader-bad-base-instance "Bad base instance"
  :reader-bad-character-code "Bad character code"
  :reader-bad-date-syntax "Bad date syntax"
  :reader-bad-dotted-list "Bad dotted list"
  :reader-bad-escape "Bad escape"
  :reader-bad-inexact-number-syntax "Bad inexact number syntad"
  :reader-bad-message-send "Bad message send"
  :reader-bad-number-syntax-syntax "Bad number syntax"
  :reader-bad-readsharp-syntax "Bad readsharp syntax"
  :reader-bad-slot-reference "Bad slot reference"
  :reader-bad-time-syntax "Bad time syntax"
  :reader-eos-in-list "End of input while reading list"
  :reader-eos-in-string "End of input while reading string"
  :reader-unexpected-close "Unexpected close"
  :reader-unknown-syntax "Unknown syntax"
  )

(define (read-error error-type port location . args)
  "Signals a read error."
  (abort 'read-error error-type port location args))

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
                 (runtime-error (lambda (message args)
                                  (format (current-error-port) "; Error while reading:\n")
                                  (format (current-error-port) "; Error: ~I\n" message args)
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

(defmacro (with-repl-error-handling context-name . code) ;; TODO: split into fn/macro
  "Evaluates <form> in environment <env>, suppressing errors and returning
  return values as a list, rather than as multiple values."
  `(catch 'repl-abort-evaluation
     (handler-bind ((uncaught-throw ;; TODO: Refactor this into an external fn
                     (lambda (tag retval)
                       ;; TODO: Correct this port
                       (format #t "Uncaught throw during ~a: ~a\n" ,context-name tag)
                       (throw 'repl-abort-evaluation (list retval))))
                    (unhandled-abort
                     (lambda (condition args)
                       (format #t "Unhandled abort during ~a: ~a\n\nargs=~a\n\n" ,context-name condition args)
                       (throw 'repl-abort-evaluation (list condition))))
                    (user-break
                     (lambda ()
                       (info "***USER BREAK***")
                       (throw 'repl-abort-evaluation (list 'user-break)))))
       (catch 'error-escape
         ,@code))))

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
   (mvbind results (time (eval `(scheme::%mark-stack 'system-stack-boundary ,form) env))
     results)))

(define *repl-pre-read-hook* ())
(define *repl-pre-print-value-hook* ())
(define *repl-post-hook* ())

(define (repl-print . values)
  "Prints each of <values> in a nicely formatted, REPL-like, way, updating
   the REPL history."
  (dolist (value values)
    (catch 'repl-do-not-print
      (with-repl-error-handling "pre-print-value-hook"
       (invoke-hook '*repl-pre-print-value-hook* value))
      (handler-bind ((runtime-error (lambda (message args)
                                      (format (current-error-port) "; REPL Print Error: ~I" message args)
                                      (throw 'repl-do-not-print))))
        (let ((history-index (extend-repl-history! value)))
          (dynamic-let ((*print-readably* #f))
            (format #t "; ##~a = ~s\n" history-index value)))))))

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
              (throw 'repl-escape))
            (apply repl-print (repl-eval form env))
            (with-repl-error-handling "post-hook"
             (invoke-hook '*repl-post-hook*)))) ;; TODO: error guard
        (loop)))))

(define (toplevel-repl)
  "Enters a toplevel Lisp REPL. A toplevel Lisp REPL is distinguished
   by the fact that it has unique handling for the 'repl-quit and
   'repl-toplevel dynamic escapes used to quit the system and return to the
   toplevel REPL."
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

(define (exit :optional (retval 0))
  "Forcibly shuts down the interpreter, via a dynamic escape, causing the
   interpreter to return <retval> as a return value."
  (throw '%end-it-all retval))


(add-hook-function! '*repl-post-hook* 'do-idle-processing)