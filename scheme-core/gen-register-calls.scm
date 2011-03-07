
;;;; gen-register-calls.scm --
;;;;
;;;; This generates source text for a C function that registers
;;;; the internal files declared in the specified source files.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.


(define *files-to-scan* ())

(define-file-argument-handling
  (push! arg *files-to-scan*))

(define (internal-file-variables source-filename)
  (let ((variables ()))
    (doiterate ((:file-lines line source-filename))
      (when (string-search "DECL_INTERNAL_FILE" line)
        (push! (second (split-string line " "))
               variables)))
    variables))

(define (write-registration-source variables)
  (format #t "\n")
  (dolist (variable variables)
    (format #t "extern DECL_INTERNAL_FILE ~a;\n" variable))
  (format #t "\n")
  (format #t "void auto_register_internal_files()\n")
  (format #t "{\n")
  (dolist (variable variables)
    (format #t "     register_internal_file(&~a);\n" variable))
  (format #t "}\n")
  (format #t "\n"))

(define (run)
  (set-port-translate-mode! (current-output-port) #f)
  (write-registration-source
   (append-map internal-file-variables *files-to-scan*)))