
;;;; compiler-file.scm --
;;;;
;;;; The file compiler. A compiler that uses the standard expression
;;;; compiler to compile files either ahead of time or at runtime.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define *initial-package* "user")
(define *disable-load-unit-boundaries* #f)

;;; The file reader

(define *compiler-reader* read)

(define (compile-read-error message port port-location)
  (signal 'compile-read-error message port port-location))

(define (port-location-string port :optional (port-location ()))
  (if (null? port-location)
      (format #f "~a:...: " (port-name port))
      (format #f "~a:~a:~a: " (port-name port) (car port-location) (cdr port-location))))

(define (form-location-string form)
  (aif (and (hash? *location-mapping*)
            (hash-ref *location-mapping* form #f))
       (port-location-string (car it) (cdr it))
       "...."))

(define *package-var* '*package*)

(define (compiler-read port)
  (let ((loc (begin
               (flush-whitespace port #t)
               (port-location port))))
    (handler-bind ((read-error (lambda (message port loc)
                                 (compile-read-error message port loc))))
      (dynamic-let ((*package* (symbol-value *package-var* ())))
        (trace-message *show-actions* "* READ in ~s\n" *package*)
        (*compiler-reader* port)))))

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


(define *files-currently-compiling* ())

(define (currently-compiling-file? filename)
  (if (any? #L(filename-string=? _ filename) *files-currently-compiling*)
      filename
      #f))

(define compile-file/simple)

;;;; Toplevel form handlers

(define *toplevel-form-handlers* #h(:eq))

(define (toplevel-file-form-symbols)
  (hash-keys *toplevel-form-handlers*))

(defmacro (define-toplevel-form pattern . code) ;; TODO: share with the (very similar) code in compiler-meaning.
  (check pair? pattern)
  (check symbol? (car pattern))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (hash-push! *toplevel-form-handlers* ',(car pattern)
                 (cons (lambda (form)
                         (and (pair? form)
                              (dbind-matches? ,(cdr pattern) (cdr form))))
                       (lambda (form load-time-eval? compile-time-eval? output-fasl-stream)
                         (dbind ,(cdr pattern) (cdr form)
                           ,@code))))))

(forward process-toplevel-forms)

(define-toplevel-form (eval-when situations . forms)
  (unless (and (list? situations)
               (every? #L(member _ '(:compile-toplevel :load-toplevel :execute)) situations))
    (compile-error form "Bad situations list, situations must be :compile-toplevel, :load-toplevel, or :execute."))
  (let ((load-time-eval? (and load-time-eval? (member :load-toplevel situations)))
        (compile-time-eval? (or (member :compile-toplevel situations)
                                (and compile-time-eval?
                                     (member :execute-toplevel situations)))))
    (when (or load-time-eval?
              compile-time-eval?)
      (process-toplevel-forms (form-list-reader forms) load-time-eval? compile-time-eval? output-fasl-stream))))

(define (compile-toplevel-definition symbol value-form output-fasl-stream)
  (let* ((value-thunk (compile value-form))
         (value (value-thunk)))

    (trace-message *show-actions* "==> DEFINE: ~s := ~s\n" symbol value)
    (trace-message *verbose* "; defining ~a\n" symbol)

    ;; error checking here???
    (scheme::%define-global symbol value)

    (fasl-write-op output-fasl-stream system::FASL_OP_LOADER_DEFINEA0
                   symbol value-thunk)))

(define begin-load-unit-boundaries)

;;;; Toplevel special forms

(define-toplevel-form (include file-spec)
  (define (file-spec-files)
    (if (wild-glob-pattern? file-spec) ;; REVISIT: Always glob
        (directory file-spec)
        (list file-spec)))
  (dolist (filename (file-spec-files))
    (call-with-compiler-tracing *show-actions* '("BEGIN-INCLUDE" "END-INCLUDE")
                                (lambda (filename)
                                  (when (currently-compiling-file? filename)
                                    (compile-fatal-error #f "Recursive include of ~s while compiling ~s"
                                                         filename *files-currently-compiling*))
                                  (compile-file/simple filename output-fasl-stream))
      filename)))

(define-toplevel-form (scheme::%define symbol value-form)
  (compile-toplevel-definition symbol value-form output-fasl-stream))

(define-toplevel-form (scheme::%define symbol)
  (compile-toplevel-definition symbol () output-fasl-stream))

(define-toplevel-form (%%begin-load-unit-boundaries filename)
  (begin-load-unit-boundaries filename output-fasl-stream))

(define-toplevel-form (begin . forms)
  (process-toplevel-forms (form-list-reader forms)
                          load-time-eval? compile-time-eval? output-fasl-stream))

;;;; The toplevel form main loop

(define (emit-action form output-fasl-stream)
  (trace-message *show-actions* "==> EMIT-ACTION: ~s\n" form)
  (fasl-write-op output-fasl-stream system::FASL_OP_LOADER_APPLY0 (compile form)))

(define (process-toplevel-form form load-time-eval? compile-time-eval? output-fasl-stream)
  (trace-message *show-actions* "* PROCESS-TOPLEVEL-FORM~a~a: ~s\n"
                 (if load-time-eval? " [load-time]" "")
                 (if compile-time-eval? " [compile-time]" "")
                 form)
  ;; Forms that aren't lists evaluate to themselves and can be ignored
  (when (pair? form)
    (aif (hash-ref *toplevel-form-handlers* (car form))
      (aif (find #L((car _) form) it)
        ((cdr it) form load-time-eval? compile-time-eval? output-fasl-stream)
        (error "Invalid syntax for toplevel form ~a: ~s" (car form) form))
      (mvbind (expanded? expanded-form) (maybe-expand-user-macro form #t)
        (cond (expanded?
               (process-toplevel-form expanded-form load-time-eval? compile-time-eval? output-fasl-stream))
              (#t
               (when compile-time-eval?
                 (compiler-evaluate form))
               (when load-time-eval?
                 (emit-action form output-fasl-stream))))))))

(define (process-toplevel-forms reader load-time-eval? compile-time-eval? output-fasl-stream)
  (let loop ((next-form (reader)))
    (unless (eof-object? next-form)
      (process-toplevel-form next-form load-time-eval? compile-time-eval? output-fasl-stream)
      (loop (reader)))))

(define (compile-port-forms ip output-fasl-stream)
  (process-toplevel-forms (lambda () (compiler-read ip)) #t #f output-fasl-stream))

;;; Error reporting

(define (end-compile-abnormally return-code)
  (abort 'end-compile-now return-code))

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
    (fasl-write-op output-fasl-stream system::FASL_OP_BEGIN_LOAD_UNIT filename)
    (fasl-write-op output-fasl-stream system::FASL_OP_LOADER_APPLY0 system::LOAD-TIME-GET-PACKAGE)
    (fasl-write-op output-fasl-stream system::FASL_OP_LOADER_PUSH)))

(define (end-load-unit filename output-fasl-stream)
  (unless *disable-load-unit-boundaries*
    (fasl-write-op output-fasl-stream system::FASL_OP_LOADER_APPLYN system::LOAD-TIME-SET-PACKAGE! 1)
    (fasl-write-op output-fasl-stream system::FASL_OP_END_LOAD_UNIT filename)))

(define (begin-load-unit-boundaries filename output-fasl-stream)
  (unless *disable-load-unit-boundaries*
    (error "Cannot begin load unit boundaries unless they have already been disabled on the command line."))
  (unless (string? filename)
    (error "Bad filename for beginning load unit boundaries: ~s" filename))
  (trace-message #t "; Beginning load unit boundaries with filename: ~a\n" filename)
  (set! *disable-load-unit-boundaries* #f)
  (begin-load-unit filename output-fasl-stream))

(define (compile-file/simple filename output-fasl-stream)
  ;; REVISIT: Logic to restore *package* after compiling a file. Ideally, this should
  ;; match the behavior of scheme::call-as-loader, although it is unclear how this
  ;; relates to the way we do cross-compilation.
  (let ((original-package (symbol-value *package-var* ())))
    (dynamic-let ((*files-currently-compiling* (cons filename *files-currently-compiling*)))
      (trace-message #t "; Compiling file: ~a\n" filename)
      (with-port input-port (open-input-file filename)
        (begin-load-unit filename output-fasl-stream)
        (compile-port-forms input-port output-fasl-stream)
        (end-load-unit filename output-fasl-stream)))
    (set-symbol-value! *package-var* original-package ())))

(define (compile-file/checked filename output-fasl-stream)
  (let ((compile-error-count 0))
    (handler-bind ((compile-read-error
                    (lambda (message port port-location)
                      (compiler-message (port-location-string port port-location) :read-error message ())
                      (end-compile-abnormally 1)))
                   (compile-error
                    (lambda (context-form fatal? message details)
                      (compiler-message/form context-form :error message details)
                      (incr! compile-error-count)
                      (when fatal?
                        (end-compile-abnormally 1))))
                   (compile-warning
                    (lambda (context-form message args)
                      (compiler-message/form context-form :warning message args))))
      (compile-file/simple filename output-fasl-stream)
      compile-error-count)))

(define (do-compile-files filenames output-filename)
  (with-fasl-file output-fasl-stream output-filename

      (handler-bind ((end-compile-now
                      (lambda (return-code)
                        (abort-fasl-writes output-fasl-stream))))

        (let next-file ((filenames filenames) (error-count 0))
          (cond ((not (null? filenames))
                 (next-file (cdr filenames)
                            (+ error-count (compile-file/checked (car filenames) output-fasl-stream))))
                ((> error-count 0)
                 (format *compiler-error-port* "; ~a error(s) detected while compiling.\n" error-count)
                 (end-compile-abnormally 2))
                (#t
                 ()))))))

(define (find-default-output-filename input-filenames)
  (if (length=1? input-filenames)
      (let ((input-filename (first input-filenames)))
        (unless (string? input-filename)
          (error "Invalid input filename: ~s" input-filename))
        (string-append (filename-no-extension input-filename) ".scf"))
      "a.scf"))

(define (setup-initial-package!)
  (aif (and *initial-package*
            (find-package *initial-package*))
       (scheme::%define-global compiler::*package-var* it)
       (begin
         (trace-message #t "Initial package not found: ~s" *initial-package*)
         (end-compile-abnormally 127))))

(define (compiler-runtime-error-handler error-info)
  (if *debug*
      (handle-runtime-error error-info)
      (begin
        (trace-message #t "; Runtime error during compile:\n;   ~I\n"
                       (hash-ref error-info :message)
                       (hash-ref error-info :arg))
        (end-compile-abnormally 127))))

(define (parse-compiler-filename-arguments input-filename output-filename)
  (let* ((input-filenames (->list input-filename))
         (output-filename (aif output-filename
                               it
                               (find-default-output-filename input-filenames))))
    (unless (string? output-filename)
      (error "Invalid output filename: ~s" output-filename))
    (values input-filenames
            output-filename)))

(define (compile-file input-filename :optional (output-filename #f))
  (mvbind (input-filenames output-filename)
      (parse-compiler-filename-arguments input-filename output-filename)
    (catch 'end-compile-now
      (handler-bind ((end-compile-now
                      (lambda (return-code)
                        (when (file-exists? output-filename)
                          (delete-file output-filename))
                        (throw 'end-compile-now return-code))))
        (handler-bind ((runtime-error compiler-runtime-error-handler))
          (setup-initial-package!)
          (dynamic-let ((*location-mapping* (make-hash :eq)))
            (do-compile-files input-filenames
                              output-filename))
          (trace-message #t "; Compile completed successfully.\n"))
        0))))

