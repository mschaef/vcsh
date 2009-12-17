;;;; main.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;;
;;;; Scheme interpreter initialization file. This is where the interpreter
;;;; is actually booted up.


;;; Command line argument parser

(define *command-argument-bindings* (make-hash :equal))
(define *file-argument-handler* #f)
(define *program-launch-name* #f)

(define (safe-args)
  "Return the value of args safely... return () is *args* is unbound.  This
   happens during scansh0 startup."
  (if (symbol-bound? '*args*)
      *args*
      '()))

(define (bad-command-argument-value parameter-name message value)
  (abort 'invalid-command-argument parameter-name message value))

(define (process-arguments first-arg-is-launch-name?)
  "Process the command line arguments, using current argument bindings"

  (define (split-arg-list arg-list)
    "Split an argument string list into an argument binding a-list of
     ( <option-name> . <option-value> ) pairs. Arguments starting with
     #\\/ or #\\- are considered option arguments and are split at #\\:
     or #\\=, with the option value following. All non-option arguments
     are lumped into a list, keyed by the symbol 'filenames."
    (define (split-option-arg arg)
      (values-bind (split-string-once arg #\:) (var val)
         (if val
             (cons var val)
             (values-bind (split-string-once arg #\=) (var val)
                (cons var val)))))

    (map (lambda (arg)
           (cond ((equal? arg "--") #f)
                 ((string-begins-with? arg "--")
                  (split-option-arg (substring arg 2)))
                 ((string-begins-with? arg "-" "/")
                  (split-option-arg (substring arg 1)))
                 (#t
                  (cons :filename arg))))
         arg-list))

  (define (process-argument arg-binding)
    (catch 'end-current-argument
      (let ((arg-name (car arg-binding)))
        (handler-bind ((invalid-command-argument
                       (lambda (parameter-name message value)
                         (format (current-error-port)
                                 "Bad value ~s for parameter ~s of command argument ~s.\n Details: ~a\n"
                                 value parameter-name arg-name message)
                         (throw 'end-current-argument))))
          (if (eq? arg-name :filename)
              (if *file-argument-handler*
                  (*file-argument-handler* (cdr arg-binding))
                  (throw 'invalid-argument "Can't handle file arguments"))
              (aif (hash-ref *command-argument-bindings* arg-name)
                   (catch 'invalid-argument
                     (it (cdr arg-binding)))
                   (format (current-error-port)
                           "Unknown command line argument: ~a\n" (car arg-binding))))))))

  (let ((args (safe-args)))
    (when first-arg-is-launch-name?
      (set! *program-launch-name* (car args)))
    (for-each process-argument (split-arg-list (if first-arg-is-launch-name?
                                                   (cdr args)
                                                   args)))))

(define (extend-command-argument-bindings! name/names proc)
  (let ((names (if (atom? name/names) (list name/names) name/names)))
    (dolist (name names)
      (hash-set! *command-argument-bindings* name  proc))))


(define (set-file-argument-handler! proc)
  (set! *file-argument-handler* proc))

(defmacro (define-command-argument l-list . code)
  (define (parse-command-argument-lambda-list original)
    (let retry ((l-list original))
      (cond ((string? l-list)
             (retry`((,l-list))))
            ((not (pair? l-list))
             (error "Invalid command argument lambda list: ~s" original))
            ((and (list? l-list) (every? string? l-list))
             (retry `(,l-list)))
            ((string? (car l-list)) (retry `((,(car l-list)) ,@(cdr l-list))))
            (#t
             (let ((names (car l-list))
                   (args (cdr l-list)))
               (unless (and (list? names) (every? string? names))
                 (error "Invalid command argument name in lambda-list: ~s." original))
               (unless (valid-lambda-list? args)
                 (error "Invalid arguments ~s in command argument lambda-list: ~s." args original))
               (values (car l-list) (cdr l-list)))))))
  (values-bind (parse-command-argument-lambda-list l-list) (names args)
     (when (> (length args) 1)
       (error "Command arguments taking more than one argument are currently unsupported: ~s" names)) ; TODO: Fix this
     `(extend-command-argument-bindings! ',names (lambda ,args ,@code))))

(defmacro (define-file-argument-handling . code)
  `(set-file-argument-handler!  (lambda (arg) ,@code)))

(define fasl-load %fasl-load)

(define *location-mapping* #f)

(define *loader-location-map* (make-hash :eq))

(define (form-source-location form)
  "Return the original location of <form> in the source code that has
   been loaded by the interpreter. If <form> is of unknown origin,
   returns <#f. The location is returned as as a tuple of
   the form (<filename> <line> . <col>)."
  (if *loader-location-map*
       (aif (hash-ref *loader-location-map* form #f)
            (cons (port-name (car it)) (cdr it))
            #f)
       #f))

(define (%text-load source)
  "Load and evaluate forms from <source>, which must be either a text mode
   input port or the name of a file that will be opened with such."
  (cond ((string? source)
         (call-with-input-file source %text-load))
        ((and (input-port? source) (not (binary-port? source)))
         (while (not (port-at-end? source))
           (eval (dynamic-let ((*location-mapping* *loader-location-map*))
                   (read source #f *location-mapping*)))))
        (#t
         (error "Bad filename or port in load: ~s" source))))

;; There are three kinds of load hooks:
;;
;; 1) *pre-load-hook*
;;
;;     Invoked immediately before starting to load a file
;;
;; 2) *finalize-load-hook*/*post-load-hook*
;;
;;    Both are invoked immediately after successfully loading
;;    a file. The difference is that finalize-load hook functions
;;    are restricted in scope to the currently loading file. They
;;    must be installed during a file load to be run only once: at
;;    the end of that particular file's load.


(define *pre-load-hook* ())
(define *post-load-hook* ())

(define (do-load load-fn source)
  "Load the input <source> using the loader function <load-fn>. <source> must
   be a suitable argument for <load-fn>."
  (dynamic-let ((*package* *package*)
                (*finalize-load-hook* ()))
    (catch 'end-load
      (begin-1
       (invoke-hook '*pre-load-hook* source)
       (load-fn source)
       (invoke-hook '*finalize-load-hook* source)
       (invoke-hook '*post-load-hook* source)))))

(define (load-file filename)
  "Loads the file specified by <filename> and returns <filename>. Signals
   an error if the file does not exist or signals an error while being loaded.
   If the file extension is scf, this attempts to fast load the file, otherwise
   it's loaded with the text loader."
  (info "Loading from ~a, *package*=~a" filename *package*)
  (dynamic-let ((*current-load-file* filename))
    (let ((file-type (string-downcase (filename-extension filename))))
      (cond ((equal? file-type "scf")
             (do-load %fasl-load filename))
            (#t
             (do-load %text-load filename))))
    filename))

(define (load-internal filename)
  "Loads the internal file <filename>. Signals an error if the file does
   not exist or signals an error while being loaded."
  (aif (find-internal-file filename)
       (let ((port (clone-c-data-port it)))
         (info "Loading Internal File: ~a" filename)
         (if (binary-port? port)
             (do-load %fasl-load port)
             (do-load %text-load port)))
       (error "Internal file not found: ~a\n" filename)))

(define (load-if-exists filename)
  "Loads <filename>, only if it exists as specfied. Returns <filename> if
   the load is successful, #f if the file does not exist."
  (if (file-exists? filename)
      (load-file filename)
      #f))

(define *path-prefix* ())
(define *path-suffix* ())

(define (load-path)
  "Returns the current load search path in list form. The load path is
   defined in the environment variable SCANPATH. There are also two
   global variables, *path-prefix* and *path-suffix* that can be lists
   of path entries to prepend and append, respectively, to the SCANPATH."
  (append *path-prefix*
          (aif (environment-variable "SCANPATH")
               (path-string->list it)
               ())
          *path-suffix*))

(define (which-file filename)
  "Searches for <filename> in the current SCANPATH. Returns either a fully
   qualified filename to the file if it exists of #f if not."
  (let try-next-dir ((dirs (load-path)))
    (if (null? dirs)
        #f
        (let ((full-filename (make-filename (car dirs) filename)))
          (if (file-exists? full-filename)
              full-filename
              (try-next-dir (cdr dirs)))))))

(define (file-to-load filename)
  "Determines the file to load based on the filename <filename>. This looks for
   the file in the current director, and if it cannot be found there, searches
   SCANPATH."
  (if (file-exists? filename)
      filename
      (which-file filename)))

(define (load filename)
  "Loads <filename>, searching for it in current directory and then in
   the directories in the (load-path)."
  (cond ((input-port? filename)
         (do-load %text-load filename))
        ((symbol? filename)
         (load (symbol-name filename)))
        ((not (string? filename))
         (error "Invalid argument: ~a" filename))
        (#t
         (aif (file-to-load filename)
              (load-file it)
              (error "File not found, after searching path: ~a" filename)))))

(define (include file-spec)
  "Loads <file-spec>, searching for it in current directory and then in
   the directories in the (load-path). This is equivalent to load, except that
   the compiler treats included files as if they were in the including
   source file. The file specification may also be specified as a wildcard, in which
   case all matching files are loaded in arbitrary order."
  (define (file-spec-files)
    (if (wild-glob-pattern? file-spec)
        (directory file-spec)
        (list file-spec)))
  (dolist (filename (file-spec-files))
    (load filename)))

(define *silent* #f)
(define *no-startup-files* #f)
(define *no-repl* #f)

(define-command-argument ("no-repl")
  "Don't launch a REPL."
  (set! *no-repl* #t))

(define-command-argument ("no-startup-files")
  "Don't process startup files."
  (set! *no-startup-files* #t))

(define-command-argument ("silent")
  "Don't print any status or info messages, by default."
  (set! *info* #f)
  (set! *silent* #t))

(define (show-command-help)
  "Shows command line help listing all current command line option bindings."
  (define (show-option-handler-lambda-list option-handler)
    (values-bind (procedure-lambda-list option-handler) (rt-l-list source-l-list)
      (cond ((eq? source-l-list #f) "--- Unknown Argument List ---") ;; Should only happen when the lambda-list procedure prop is stripped off
            ((null? source-l-list) ())
            (#t (format #t "=<~a>" (symbol-name (car source-l-list)))))))

  (define (all-distinct-option-bindings)
    "Return a list of all distinct option bindings. The car of each list
     node is a list of the names bound to the procedure in the cdr of
     each list node. The lists are sorted into alphabetical order."
    (let ((option-handler-bindings (make-hash :eq)))
      (dohash (arg-name arg-binding *command-argument-bindings*)
              (hash-push! option-handler-bindings arg-binding arg-name))
      (qsort (map #L(cons (qsort (rest _) string<) (first _))
                  (hash->a-list option-handler-bindings))
             string< caar)))

  (define (show-binding-names names)
    (fold (lambda (name need-comma?)
            (format #t "~a~a~a" (if need-comma? ", " "") (if (> (length name) 1) "--" "-") name)
            #t)
          #f names))

  (dynamic-let ((*info* #f))
    (format #t "Usage: ~a [OPTIONS]~a\n" *program-launch-name* (if *file-argument-handler* " [FILE]..." ""))
    (dolist (binding (all-distinct-option-bindings))
      (format #t "  ")
      (show-binding-names (car binding))
      (let ((option-handler (cdr binding)))
        (show-option-handler-lambda-list option-handler)
        (awhen (documentation option-handler)
          (newline)
          (dolist (line (break-lines it *default-width*))
            (format #t "    ~a\n" line)))
        (newline)))))


(define-command-argument ("?" "help")
  "Display this help message."
  (show-command-help))

(define-command-argument ("c" "compile")
  "Run in compiler mode. (Request help with this flag specified for more
   help on compiler-specific options.)"
  (load-internal "fasl-compiler-run")) ; REVISIT: loading this without fasl-compiler throws a malformed package list error

(define-file-argument-handling
  (handler-bind ((runtime-error
                  (lambda (message args)
                    (format (current-error-port) "; Error while reading file: ~s\n" arg)
                    (handle-runtime-error message args))))

  (load-file arg)))

(define-command-argument ("load-file" filename)
  "Loads the specified file into the environment at the time the command line
   argument is processed.  If this option is specified  multiple times, all files
   are loaded in the order specified on the command line."
  (load-file filename)) ; REVISIT: the fasl-compiler version of this option has validity checks... necessary here too?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GC policy

;; Without a re-entrant interpreter, it is not safe to set an
;; *after-gc* hook in the presence of multiple threads.
;;
;; (define (*after-gc* free-cells default-heap-size)
;;   "Default garbage collection policy is to add a heap if we
;;    run a garbage collection cycle and end up with less than
;;    a quarter-heap's worth of free cells.  If this is true,
;;    we're not getting enough value out of each GC run, and
;;    need more breathing room."
;;   (when (< free-cells (/ default-heap-size 4))
;;     (enlarge-heap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We need the compiler around


(define (process-startup-files)
  "Loads vcsh startup files. This loads any of the following in this
   order: .vcsh_init and vcsh.init in the user's home directory, and
   .vcsh_init and vcsh.init in the current directory."
  (awhen (home-directory)
    (load-if-exists (make-filename it ".vcsh_init"))
    (load-if-exists (make-filename it "vcsh.init")))
  (load-if-exists ".vcsh_init")
  (load-if-exists "vcsh.init"))

(define (initialize-user-package)
  (unless (find-package "user")
    (make-package! "user")
    (use-package! "scheme" "user")))

(define *shutdown-hook* ())

(define (%run0)
  (let ((retval (catch '%end-it-all
                  (catch 'read-failed
                    (handler-bind ((runtime-error handle-runtime-error))
                      (initialize-user-package)
                      (dynamic-let ((*package* (find-package "user")))
                        (with-default-read-error-handling
                         (process-arguments #t))
                        (run)))))))
    ;; The shutdown hook is always invoked, to allow for any cleanxup
    ;; that needs to occur before the heap is shut down itself.
    (invoke-hook '*shutdown-hook* retval)
    retval))

(define (run)
  (unless *no-startup-files*
    (process-startup-files))
  (unless *silent*
    (format #t "; Welcome to VCSH\n")
    (format #t ";    VM Build ID    : ~a\n" (system-info :vm-build-id))
    (format #t ";    Scheme Build ID: ~a\n" (system-info :scheme-build-id))
    (format #t ";\n")
    (format #t "; (C) Copyright 2001-2009 East Coast Toolworks Inc.\n")
    (format #t "; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.\n")
    (format #t "\n"))
  (let ((rc (if *no-repl* 0 (toplevel-repl))))
    (unless *silent*
      (format #t "; Exiting with rc=~a\n" rc))
    rc))



