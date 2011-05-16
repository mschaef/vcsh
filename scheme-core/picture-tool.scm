
(define file-sha1-digest #.(scheme::%subr-by-name "file-sha1-digest"))

(define *picture-target* "~/picture-target")

(define *dry-run* #t)

(define (system-command cmd)
  (format (current-error-port) "; system: ~s\n" cmd)
  (unless *dry-run*
    (system cmd)))

(define (find-import-details filenames)
  (map (lambda (filename)
         (let ((details `#h(:eq source-filename ,filename)))
           (when (file-exists? filename)
             (let ((source-digest (file-sha1-digest filename)))
               (hash-set! details 'source-digest source-digest)
               (hash-set! details 'target-filename (make-filename *picture-target*
                                                                  (string-append source-digest "." (filename-extension filename))))))
           details))
       filenames))

(define (copy-file from to)
  (system-command #"cp ${from} ${to}")) ;; TODO: does this syntax handle explicit packages correctly? What about symbols in packages other than *package*?

(define (do-import filenames)
  (dolist (import-info (find-import-details filenames))
    (when (hash-has? import-info 'target-filename)
      (copy-file (hash-ref import-info 'source-filename)
                 (hash-ref import-info 'target-filename)))))