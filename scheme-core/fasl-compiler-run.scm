;;;; fasl-compiler-run.scm
;;;; August 18th, 2007
;;;; Mike Schaeffer
;;
;; Code to invoke the FASL compiler from the command line

(define-package "fasl-compiler-run"
  (:uses "scheme"
         "fasl-compiler"))

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
  (set! *enter-repl-on-runtime-error* #t)
  )

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

(define-command-argument ("initial-package" package-name)
  "Sets the initial value of *package* at the beginning of compilation."
  (set! *initial-package* package-name))

(define *cross-compile* #f)

(define-command-argument ("cross-compile" cross-compiler-mode)
  "Enables support for 'cross compiling' code that might redefine
   core parts of the runtime as it is compiled. The two supported
   cross compiler modes are \"environment\" and \"package-renaming\"."
  (let ((mode (if (= 0 (length cross-compiler-mode))
                  :environment
                  (intern-keyword! cross-compiler-mode))))
    (unless (memq mode '(:environment :package-renaming))
       (bad-command-argument-value 'cross-compiler-mode
                                   "Bad cross compiler mode, valid options are \"environment\" and \"package-renaming\"." mode))
    (set! *cross-compile* mode)))

(define *compiler-load-files* ())

(define-command-argument ("load-file" filename)
  "Loads the specified file immediately before beginning compilation. If this
   option is specified  multiple times, all files are loaded in the order specified
   on the command line."
  (when (= 0 (length filename))
    (bad-command-argument-value 'filename "Missing load file name" filename))
  (unless (file-exists? filename)
    (bad-command-argument-value 'filename "The load file does not exist" filename))
  (set! *compiler-load-files*
        (append *compiler-load-files* (cons filename))))

(define *files-to-compile* '())

(define-file-argument-handling
  (set! *files-to-compile* (append *files-to-compile* (list arg))))

(define (show-compiler-settings)
  (when *verbose*
    (format #t "~&\nCompiler settings:")
    (format #t "~&----------------------------------------------------------------")
    (format #t "~&Input file~a              : ~a" (if (> (length *files-to-compile*) 1)
                                                      "s " "  ") *files-to-compile*)
    (format #t "~&Output file name          : ~a" *output-file-name*)
    (format #t "~&Initial package           : ~a" *initial-package*)
    (format #t "~&Debug output              : ~a" (if *debug* "on" "off"))
    (format #t "~&Verbose output            : ~a" (if *verbose* "on" "off"))
    (format #t "~&Cross Compiling           : ~a" (aif *cross-compile* it "no"))
    (format #t "~&Load files                : ~a" *compiler-load-files*)
    (newline)))

(define (setup-cross-compiler/global-environment)
  "Setup for cross compiling using a separate global environment. "
  (set! *compiler-target-bindings* (copy-global-environment :compiler-target-binding))
  (when *debug*
    (format #t "~&; Global Bindings = ~@ ~s"
            (scheme::%current-global-environment)
            (vector-ref (scheme::%current-global-environment) 0))
    (format #t "~&; Compiler Target Bindings = ~@ ~s"
            *compiler-target-bindings* (vector-ref *compiler-target-bindings* 0))))

(define (setup-cross-compiler/package-renaming)
  "Setup for cross compiling using renamed packages."
  (format #t "; Configuring for cross compile by renaming packages.\n")
  (let ((excluded-packages (map find-package '("system" "keyword"))))
    (dolist (p (list-all-packages))
      (unless (memq p excluded-packages)
        (rename-package! p (string-append "host-" (package-name p))))))
  (make-package! "scheme")
  (make-package! "user")
  (use-package! "system" "scheme")
  (use-package! "scheme" "user")
  (in-package! "scheme")
  (load-internal "s-core")
  (set! fasl-compiler::*compiler-reader*
        (symbol-value (intern! "read" (find-package "scheme")))))


(define (maybe-enable-cross-compile)
  (when *cross-compile*
    (case *cross-compile*
      ((:environment)      (setup-cross-compiler/global-environment))
      ((:package-renaming) (setup-cross-compiler/package-renaming))
      ((#f) #f) ; default case, no cross compile
      (#t                  (error "Invalid cross compile mode: ~s" *cross-compile*)))))

(define (load-compiler-load-files)
  (dolist (file *compiler-load-files*)
    (format #t "; Loading compiler load file: ~s\n" file)
    (load file)
    (format #t "; Done loading compiler load file: ~s\n" file))) ; REVISIT: error check this?

(define (run)
  (dynamic-let ((*info* #f))
    (enlarge-heap 50)
    (scheme::%set-stack-limit #f))
  (show-compiler-settings)
  (scheme::initialize-user-package)
  (time
   (let ((pkg (find-package *initial-package*)))
     (unless pkg
        (error "Initial package not found: ~s" pkg))
     (dynamic-let ((*package* pkg))
       (maybe-enable-cross-compile) ; REVISIT: this might not interact well with *initial-package*
       ;; Play nice with batch files launching the compiler...
       ;; we never want to be interactive.
       (load-compiler-load-files)
       (when *debug*
         (display-packages))
       (catch-all
        (compile-file *files-to-compile* *output-file-name*))))))

