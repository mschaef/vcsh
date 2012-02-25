
(define *automatic-files* (make-hash :equal))

(define (parse-automatic-filename file-desc)
  (check string? file-desc)
  (check (> 2) (length file-desc))
  (let ((mode-flag (string-ref file-desc 0))
        (filename (substring file-desc 1)))
    (values (case mode-flag
              ((#\>) :output)
              ((#\<) :input)
              (#t (error "Bad automatic file mode descriptor: ~a" file-desc)))
            filename)))

(define (open-automatic-file filename)
  (mvbind (mode name) (parse-automatic-filename filename)
    (case mode
      ((:input)
       (open-input-file name))
      ((:output)
       (open-output-file name)))))

(define (->automatic-file filename)
  (aif (hash-ref *automatic-files* filename #f)
       it
       (let ((port (open-automatic-file filename)))
         (hash-set! *automatic-files* filename port)
         port)))

(define (reset-automatic-files!)
  (dohash (filename port *automatic-files*) 
     (catch-all
      (close-port port))
     (hash-clear! *automatic-files*)))

(define (->file file)
  (if (string? file)
      (->automatic-file file)
      file))