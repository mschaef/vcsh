
(define file-sha1-digest #.(scheme::%subr-by-name "file-sha1-digest"))

;; TODO: Add ~ substitution, so that ~/picture-target will work.
(define *picture-target* "/Users/mschaef/picture-target")

(define *dry-run* #f)

(define *verbose* #t)

(define (system-command cmd)
  (when *verbose*
    (format (current-error-port) "; system: ~s\n" cmd))
  (if *dry-run*
      0
      (system cmd)))

(define (find-import-details filenames)
  (map (lambda (filename)
         (let ((details `#h(:eq source-filename ,filename)))
           (when (file-exists? filename)
             (let ((source-digest (file-sha1-digest filename)))
               ;; TODO: Add multiple hash-set!?
               (hash-set! details 'source-digest source-digest)
               (hash-set! details 'target-filename (make-filename *picture-target*
                                                                  (string-append source-digest "." (filename-extension filename))))))
           details))
       filenames))

(define (copy-file from to)
  (cond ((file-exists? to)
         (format (current-error-port) "; Duplicate target file: ~s\n" to)
         :duplicate)
        ((= 0 (system-command #"cp ${from} ${to}"))
         :copied)
        (#t
         #f)))

(define (import-picture import-info)
  (when (hash-has? import-info 'target-filename)
    ;; TODO: Extend slot-ref to work with hashes, so that the @(import-info source-filename) syntax will work
    (copy-file (hash-ref import-info 'source-filename)
               (hash-ref import-info 'target-filename))
    (hash-set! import-info 'target-digest
               (file-sha1-digest (hash-ref import-info 'target-filename)))))

(define (do-import filenames)
  (let ((details (find-import-details filenames)))
    (dolist (import-info details)
      (import-picture import-info))
    details))

