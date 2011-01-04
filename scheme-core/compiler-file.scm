;;;; compiler-file.scm
;;;
;;; The file compiler. A compiler that uses the standard expression
;;; compiler to compile files either ahead of time or at runtime.

(define *cross-compile* #f)
(define *initial-package* "user")
(define *disable-load-unit-boundaries* #f)

;;; FASL file generaiton

(define (emit-action form output-fasl-stream genv)
  (trace-message *show-actions* "==> EMIT-ACTION: ~s\n" form)
  (fasl-write-op system::FASL_OP_LOADER_APPLY0 (list (compile-form form #t)) output-fasl-stream))

;;; The file reader

(define *compiler-reader* read)
(define *compiler-location-map* #f)

(define (compile-read-error message port port-location)
  (signal 'compile-read-error message port port-location))

(define (port-location-string port :optional (port-location ()))
  (if (null? port-location)
      (format #f "~a:...: " (port-name port))
      (format #f "~a:~a:~a: " (port-name port) (car port-location) (cdr port-location))))

(define (form-location-string form)
  (aif (and (hash? *compiler-location-map*)
            (hash-ref *compiler-location-map* form #f))
       (port-location-string (car it) (cdr it))
       "...."))

(define *package-var* '*package*)

(define (compiler-read port genv)
  (let ((loc (begin
               (flush-whitespace port #t)
               (port-location port))))
    (handler-bind ((read-error (lambda (message port loc)
                                 (compile-read-error message port loc))))
      (dynamic-let ((*location-mapping* *compiler-location-map*)
                    (*package* (symbol-value *package-var* ()))
                    (scheme::*reader-genv* genv))
        (trace-message *show-actions* "* READ in ~s genv=~@\n" *package* genv)
        (*compiler-reader* port #f))))) ; REVISIT #. eval/read forms are not evaluated in genv

;;; The main loop

(define (form-list-reader forms)
  "Makes a reader for the list of forms <forms>. A reader is a closure
   that returns a form on each call and an eof-object when there are no
   more forms to be read."
  (lambda ()
    (if (null? forms)
        (scheme::%make-eof)
        (let ((form (car forms)))
          (set! forms (cdr forms))
          form))))

(define process-toplevel-forms) ; forward

(define (process-toplevel-eval-when form load-time-eval? compile-time-eval? *toplevel-forms* genv)
  (mvbind (situations forms) (parse-eval-when form)
    (let ((load-time-eval? (and load-time-eval? (member :load-toplevel situations)))
          (compile-time-eval? (or (member :compile-toplevel situations)
                                  (and compile-time-eval?
                                       (member :execute-toplevel situations)))))
      (when (or load-time-eval? compile-time-eval?)
        (process-toplevel-forms (form-list-reader forms) load-time-eval? compile-time-eval? *toplevel-forms* genv)))))

(define *files-currently-compiling* ())

(define (currently-compiling-file? filename)
  (if (any? #L(filename-string=? _ filename) *files-currently-compiling*)
      filename
      #f))

(define compile-file/simple)

(define (process-toplevel-include form output-fasl-stream genv)
  (unless (and (list? form) (length=2? form) (string? (second form)))
    (compile-error #f "Invalid include form: ~s" form))
  (let ((file-spec (second form)))
    (define (file-spec-files)
      (if (wild-glob-pattern? file-spec) ;; TODO: Always glob
          (directory file-spec)
          (list file-spec)))
    (dolist (filename (file-spec-files))
      (call-with-compiler-tracing *show-actions* '("BEGIN-INCLUDE" "END-INCLUDE")
                                  (lambda (filename)
                                    (when (currently-compiling-file? filename)
                                      (compile-fatal-error #f "Recursive include of ~s while compiling ~s"
                                                           filename *files-currently-compiling*))
                                    (compile-file/simple filename output-fasl-stream genv))
        filename))))


(define (evaluated-object? obj)
  "Returns true if <obj> is an object that has specific handling in the scheme
   evaluator. If <obj> evaluates to itself, returns #f."
  (or (scheme::fast-op? obj)
      (symbol? obj)
      (pair? obj)))

(define (process-toplevel-define form output-fasl-stream genv)
  (let* ((var (second form))
         (val-thunk (toplevel-form->thunk (third form)))
         (val (scheme::maybe-call-with-global-environment val-thunk genv)))

    (trace-message *show-actions* "==> DEFINE: ~s := ~s\n" var val)
    (trace-message *verbose* "; defining ~a\n" var)

    ;; error checking here???
    (scheme::%define-global var val genv)

    (fasl-write-op system::FASL_OP_LOADER_DEFINEA0 (list var val-thunk) output-fasl-stream)))

(define (emit-action form output-fasl-stream genv)
  (trace-message *show-actions* "==> EMIT-ACTION: ~s\n" form)
  (fasl-write-op system::FASL_OP_LOADER_APPLY0 (list (toplevel-form->thunk form)) output-fasl-stream))

(define process-toplevel-form)
(define process-%%begin-load-unit-boundaries)

(define (process-toplevel-form form load-time-eval? compile-time-eval? output-fasl-stream genv)
  (trace-message *show-actions* "* PROCESS-TOPLEVEL-FORM~a~a: ~s\n"
                 (if load-time-eval? " [load-time]" "")
                 (if compile-time-eval? " [compile-time]" "")
                 form)
  ;; Forms that aren't lists evaluate to themselves and can be ignored
  (when (pair? form)
    (case (car form)
      ((%%begin-load-unit-boundaries)
       (process-%%begin-load-unit-boundaries form output-fasl-stream))
      ((scheme::%define)
       (process-toplevel-define form output-fasl-stream genv))
      ((begin)
       (process-toplevel-forms (form-list-reader (cdr form)) load-time-eval? compile-time-eval? output-fasl-stream genv))
      ((include)
       (process-toplevel-include form output-fasl-stream genv))
      ((eval-when)
       (process-toplevel-eval-when form load-time-eval? compile-time-eval? output-fasl-stream genv))
      (#t
       (mvbind (expanded? expanded-form) (maybe-expand-user-macro form genv #t)
         (cond (expanded?
                (process-toplevel-form expanded-form load-time-eval? compile-time-eval? output-fasl-stream genv))
               (#t
                (when compile-time-eval?
                  (compiler-evaluate form))
                (when load-time-eval?
                  (emit-action form output-fasl-stream genv)))))))))

(define (process-toplevel-forms reader load-time-eval? compile-time-eval? output-fasl-stream genv)
  (let loop ((next-form (reader)))
    (unless (eof-object? next-form)
      (process-toplevel-form next-form load-time-eval? compile-time-eval? output-fasl-stream genv)
      (loop (reader)))))

(define (compile-port-forms ip output-fasl-stream genv)
  (process-toplevel-forms (lambda () (compiler-read ip genv)) #t #f output-fasl-stream genv))

;;; Error reporting

(define (end-compile-abnormally return-code output-fasl-stream)
  (abort-fasl-writes output-fasl-stream)
  (throw 'end-compile-now return-code))

(define (compiler-message context-desc message-type message message-args)
  "Display a compiler message to the compiler error port. <context-desc> is a description
   of the context for the message. <message-type> is the type of message to be written.
   <message> is a format string with the message text and <message-args> is a list of
   arguments to the message format string."
  (format *compiler-error-port* "~a ~a - ~I\n" context-desc message-type message message-args))

(define (compiler-message/form context-form message-type message message-args)
  "Display a compiler message to the compiler error port. <context-form> is a form
   that provides context for the message, if appropriate. <message-type> is the
   type of message to be written. <message> is a format string with the message text
   and <message-args> is a list of arguments to the message format string."
  (compiler-message (form-location-string context-form) message-type message message-args))

;;; Compilation setup

;; TODO: dynamic-let in a specific global environment

;; These two functions are very unusual... they are directly written into
;; compiled FASL output as arguments to APPLY0 and APPLYN and are used
;; to protect *package* at load unit boundaries. *-GET-PACKAGE is literally
;; the first intepreted code run by the VM on image startup, so it has to load
;; and run with virtually no facilities from the image. (No subrs!
;; No compiler package!) The net of this is that they're defined in the 
;; system package so that the lambda name for the functions can be loaded
;; at startup time. Also... you can do virtually nothing in these functions
;; since they have to run in such a minimal environment.
;;
;; Like I said... these are unusual.
(define (system::LOAD-TIME-GET-PACKAGE) *package*)
(define (system::LOAD-TIME-SET-PACKAGE! package) (set! *package* package))

(define (begin-load-unit filename output-fasl-stream)
  (unless  *disable-load-unit-boundaries*
    (fasl-write-op system::FASL_OP_BEGIN_LOAD_UNIT (list filename) output-fasl-stream)
    (fasl-write-op system::FASL_OP_LOADER_APPLY0 (list system::LOAD-TIME-GET-PACKAGE) output-fasl-stream)
    (fasl-write-op system::FASL_OP_LOADER_PUSH () output-fasl-stream)))

(define (end-load-unit filename output-fasl-stream)
  (unless *disable-load-unit-boundaries*
    (fasl-write-op system::FASL_OP_LOADER_APPLYN (list system::LOAD-TIME-SET-PACKAGE! 1) output-fasl-stream)
    (fasl-write-op system::FASL_OP_END_LOAD_UNIT (list filename) output-fasl-stream)))

(define (process-%%begin-load-unit-boundaries form output-fasl-stream)
  (dbind (fn filename) form
    (unless *disable-load-unit-boundaries*
      (error "Cannot begin load unit boundaries unless they have already been disabled on the command line."))
    (unless (string? filename)
      (error "Bad filename for beginning load unit boundaries: ~s" filename))
    (trace-message #t "; Beginning load unit boundaries with filename: ~a\n" filename)
    (set! *disable-load-unit-boundaries* #f)
    (begin-load-unit filename output-fasl-stream)))

(define (compile-file/simple filename output-fasl-stream genv)
  ;; REVISIT: Logic to restore *package* after compiling a file. Ideally, this should
  ;; match the behavior of scheme::call-as-loader, although it is unclear how this
  ;; relates to the way we do cross-compilation.
  (let ((original-package (symbol-value *package-var* ())))
    (dynamic-let ((*files-currently-compiling* (cons filename *files-currently-compiling*)))
      (trace-message #t "; Compiling file: ~a\n" filename)
      (with-port input-port (open-input-file filename)
        (begin-load-unit filename output-fasl-stream)
        (compile-port-forms input-port output-fasl-stream genv)
        (end-load-unit filename output-fasl-stream)))
    (set-symbol-value! *package-var* original-package ())))

(define (compile-file/checked filename output-fasl-stream genv)
  (let ((compile-error-count 0))
    (handler-bind ((compile-read-error
                    (lambda (message port port-location)
                      (compiler-message (port-location-string port port-location) :read-error message ())
                      (end-compile-abnormally 1 output-fasl-stream)))
                   (compile-error
                    (lambda (context-form fatal? message details)
                      (compiler-message/form context-form :error message details)
                      (incr! compile-error-count)
                      (when fatal?
                        (end-compile-abnormally 1 output-fasl-stream))))
                   (compile-warning
                    (lambda (context-form message args)
                      (compiler-message/form context-form :warning message args))))
      (compile-file/simple filename output-fasl-stream genv)
      compile-error-count)))

(define (compile-files filenames output-file-name :optional (genv #f))
  (with-fasl-file output-fasl-stream output-file-name
    (let next-file ((filenames filenames) (error-count 0))
      (cond ((not (null? filenames))
             (next-file (cdr filenames)
                        (+ error-count (compile-file/checked (car filenames) output-fasl-stream genv))))
            ((> error-count 0)
             (format *compiler-error-port* "; ~a error(s) detected while compiling.\n" error-count)
             (end-compile-abnormally 2 output-fasl-stream))
            (#t
             ())))))

(define (input-file-name->output-file-name input-filename)
  (let retry ((input-filename input-filename))
    (cond ((list? input-filename)
           (if (length=1? input-filename)
               (retry (first input-filename))
               "a.scf"))
          ((string? input-filename)
           (string-append (filename-no-extension input-filename) ".scf"))
          (#t
           (error "Invalid input filename: ~s" input-filename)))))

;; This is an example of how another form of cross compilation might work

(define (compile-file filename :optional (output-file-name #f))
  (let ((output-file-name (cond ((string? output-file-name) output-file-name)
                                ((not output-file-name) (input-file-name->output-file-name filename))
                                (#t (error "Invalid output filename: ~s" filename))))
        (filenames (if (list? filename) filename (list filename))))
    (set! *compiler-location-map* (make-hash :eq))
    (catch 'end-compile-now
      (handler-bind ((runtime-error
                      (if *debug*
                          handle-runtime-error
                          (lambda (message args)
                            ;;(show-runtime-error message args)
                            (format *compiler-error-port*
                                    "\n\n\nINTERNAL COMPILER ERROR!, message=~s\n\targs=~s\n"
                                    message args)
                            (throw 'end-compile-now 127)))))


        (let* ((compiler-genv (scheme::%current-global-environment))
               (target-genv compiler-genv))

          (trace-message *verbose* "; global-genv=~@, target-genv=~@\n"
                         compiler-genv target-genv)

          (compile-files filenames output-file-name target-genv))

        (format *compiler-output-port* "; Compile completed successfully.\n"))
      0)))
