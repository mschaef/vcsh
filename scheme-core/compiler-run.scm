
;;;; compiler-run.scm --
;;;;
;;;; Code to invoke the FASL compiler from the command line
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define-package "compiler-run"
  (:uses "scheme"
         "compiler"))

;;;; Startup Code

(define *output-file-name* #f)

(define-command-argument (("o" "output") output-file-name)
  "Name of the file to which compiled output is written"
  (set! *output-file-name* output-file-name))

(define-command-argument ("v" "verbose")
  "Write out additonal status messages during compile"
  (set! *verbose* #t))

(define-command-argument ("cdebug")
  "Enables support for debugging the compiler itself. Mainly detailed logging messages"
  (set! *verbose* #t)
  (set! *debug* #t)
  (set! *enter-repl-on-runtime-error* #t))

(define-command-argument ("show-all")
  "Causes all status messages to be displayed."
  (set! *show-meanings* #t)
  (set! *show-expansions* #t)
  (set! *show-actions* #t))

(define-command-argument ("show-meanings")
  "Causes a status message to be displayed for each form meaning anslysis"
  (set! *show-meanings* #t))

(define-command-argument ("show-expansions")
  "Causes a status message to be displayed for each form expansion"
  (set! *show-expansions* #t))

(define-command-argument ("show-actions")
  "Causes a status message to be displayed for each action on a form taken by the compiler"
  (set! *show-actions* #t))

(define *compiler-load-files* ())

(define-command-argument ("load-file" filename)
  "Loads the specified file immediately before beginning compilation. If this
   option is specified  multiple times, all files are loaded in the order specified
   on the command line. All files are loaded after cross-compiler setup."
  (when (= 0 (length filename))
    (bad-command-argument-value 'filename "Missing load file name" filename))
  (unless (file-exists? filename)
    (bad-command-argument-value 'filename "The load file does not exist" filename))
  (set! *compiler-load-files*
        (append *compiler-load-files* (cons filename))))

(define *files-to-compile* '())

(define-file-argument-handling
  (set! *files-to-compile* (append *files-to-compile* (list arg))))


(define (load-compiler-load-files)
  (dolist (file *compiler-load-files*)
    (compiler::trace-message #t "; Loading compiler load file: ~s\n" file)
    (load file) ; REVISIT: error check this?
    (compiler::trace-message #t "; Done loading compiler load file: ~s\n" file)))


(define (run)
  (enlarge-heap 50)
  (scheme::%set-stack-limit #f)
  (scheme::initialize-user-package)
    
  (load-compiler-load-files)
  (when *debug*
    (display-packages))

  (time
   ;; Play nice with batch files launching the compiler...
   ;; we never want to be interactive.
     
   (catch-all
    (compile-file *files-to-compile* *output-file-name*))))

