;;;; compiler-run.scm
;;;; August 18th, 2007
;;;; Mike Schaeffer
;;
;; Code to invoke the FASL compiler from the command line

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

(define-command-argument ("initial-package" package-name)
  "Sets the initial value of *package* at the beginning of compilation."
  (set! *initial-package* package-name))

(define-command-argument ("cross-compile" cross-compiler-mode)
  "Enables support for 'cross compiling' code that might redefine
   core parts of the runtime as it is compiled. The two supported
   cross compiler modes are \"environment\" and \"package-renaming\"."
  (let ((mode (if (= 0 (length cross-compiler-mode))
                  :environment
                  (intern-keyword! cross-compiler-mode))))
    (unless (memq mode '(:environment 
                         :package-renaming
                         ))
       (bad-command-argument-value 'cross-compiler-mode
                                   "Bad cross compiler mode, valid options are \"environment\" and \"package-renaming\"." mode))
    (set! *cross-compile* mode)))

(define-command-argument ("no-load-unit-boundaries")
  "Disable generation of load unit boundary code. Normally, the compiler generates
   code to preserve *package* across load units. This turns that facility off."
  (set! *disable-load-unit-boundaries* #t))

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

(define (load-compiler-load-files)
  (dolist (file *compiler-load-files*)
    (format #t "; Loading compiler load file: ~s\n" file)
    (load file)
    (format #t "; Done loading compiler load file: ~s\n" file))) ; REVISIT: error check this?


(define (shared-target-symbols)
  (set-union '(compiler::%%begin-load-unit-boundaries
               scheme::%define
               scheme::begin
               scheme::include
               scheme::eval-when)
             '(scheme::free-cell
               scheme::nil
               scheme::boolean
               scheme::cons
               scheme::fixnum
               scheme::flonum
               scheme::character
               scheme::symbol
               scheme::package
               scheme::subr
               scheme::closure
               scheme::macro
               scheme::string
               scheme::vector
               scheme::structure
               scheme::hash
               scheme::port
               scheme::end-of-file
               scheme::values-typle
               scheme::instance
               scheme::unbound-marker
               scheme::trip-wire
               scheme::fast-op
               scheme::genv)
             '(scheme::*package-list*
               scheme::*provided-packages*)
             '(scheme::iterate-sequence-expander)
             '(scheme::it
               scheme::_)
             '(scheme::and
               scheme::or
               scheme::not
               scheme::>
               scheme::>=
               scheme::<
               scheme::<=
               scheme::= 
               scheme::eq?
               scheme::equal?
               scheme::member)
             (map caar (scheme::all-iterate-sequence-types))
             (compiler::special-form-symbols)))

(define (setup-cross-compiler)
  "Setup for cross compiling using renamed packages."
  (define (package->host/target! package)
    (let ((name (package-name package)))
      (rename-package! package (string-append "host-" name))
      (let ((new-package (make-package! name)))
        (provide-package! new-package)
        (cons package new-package))))
  (format #t "; Configuring for cross compile by renaming packages.\n")
  (let* ((excluded (map find-package '("system" "keyword")))
         (host/targets (map package->host/target! (remove #L(memq _ excluded) (list-all-packages))))
         (host->target (a-list->hash host/targets)))

    ;; 0) Make sure we're providing all the packages we've created
    (dolist (package scheme::*provided-packages*)
      (awhen (hash-ref host->target package #f)
        (provide-package! it)))
    
    (dolist (special-form-sym (shared-target-symbols))
      (let ((target-package (hash-ref host->target (symbol-package special-form-sym))))
        (import! special-form-sym target-package)))

    (dolist (h/t host/targets)
      (dbind (host . target) h/t
        (dolist (host-sym (local-package-symbols host))
          ;; 2) Re-home all of the host package symbols to the target package
          (scheme::set-symbol-package! host-sym target)
    
          ;; 3) Create a separate global binding in the target packages for each host package global binding
          (when  (symbol-bound? host-sym)
            (format #t "; interning ~a in ~a\n" (symbol-name host-sym) target)
            (scheme::%define-global (intern! (symbol-name host-sym) target)
                                    (symbol-value host-sym)))))))
  )

(define (run)
  (enlarge-heap 50)
  (scheme::%set-stack-limit #f)
  (show-compiler-settings)
  (scheme::initialize-user-package)
  (when (eq? *cross-compile* :package-renaming)
    (setup-cross-compiler)
    (set! compiler::*package-var* (scheme::intern! "*package*" "scheme"))
    (scheme::repl-print '*package*)
    (scheme::repl-print compiler::*package-var*)) 
  (time
   (let ((pkg (find-package *initial-package*)))
     (unless pkg
       (error "Initial package not found: ~s" pkg))
     (scheme::%define-global compiler::*package-var* pkg)
     (load-compiler-load-files)
     (when *debug*
       (display-packages))
     ;; Play nice with batch files launching the compiler...
     ;; we never want to be interactive.
     (catch-all
      (compile-file *files-to-compile* *output-file-name*)))))

