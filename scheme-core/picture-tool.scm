
(define file-sha1-digest #.(scheme::%subr-by-name "file-sha1-digest"))

(define *picture-target* "~/picture-target")

(define *dry-run* #f)

(define (system-command cmd)
  (format (current-error-port) "; system: ~s\n" cmd)
  (unless *dry-run*
    (system cmd)))

(define (sha1-digests filenames)
  (map (lambda (filename)
          (list filename
                (file-sha1-digest filename)))
       filenames))

(define (summarize-import filenames)
  (map (lambda (filename/digest)
         (dbind (filename digest) filename/digest
           (list filename
                 (make-filename *picture-target*
                                (string-append digest "." (filename-extension filename))))))
       (sha1-digests filenames)))

(define (copy-file from to)
  (system-command #"cp ${from} ${to}"))

(define (do-import filenames)
  (dolist (s/t (summarize-import filenames))
    (dbind (source target) s/t
      (copy-file source target))))