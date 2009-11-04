
(define (files-included-at-toplevel source-filename)
  "Scan through the file named by <source-filename>, returning a list
   of files included at the toplevel. This only looks for literal
   scheme:include forms at the toplevel of the file, so it's not very
   sophisticated at all.  Also, note that the file is scanned in the
   current *package*, so if scheme:include is not available in that
   package, this may not return the expected list."
  (iterate/r ((file-forms form source-filename))
             ((dep-filenames ()))
             (if (and (list? form)
                      (eq? (car form) 'scheme:include))
                 (cons (cadr form) dep-filenames)
                 dep-filenames)
             dep-filenames))

(define *files-to-grovel* ())

(define-file-argument-handling
  (push! arg *files-to-grovel*))

(define (grovel-file source-filename)
  (let ((output-filename (string-append (filename-no-extension source-filename) ".scf"))
        (deps (files-included-at-toplevel source-filename)))
    (format #t "~a: ~a" output-filename source-filename)
    (dolist (dep deps)
      (format #t " ~a" dep))
    (newline)))

(define (run)
  (set-port-translate-mode! (current-output-port) #f)
  (dolist (source-filename *files-to-grovel*)
    (grovel-file source-filename)))