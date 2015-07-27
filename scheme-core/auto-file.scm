
(define *auto-files* (make-hash))

(define (parse-auto-filename file-desc)
  (check string? file-desc)
  (check (> 2) (length file-desc))
  (let ((mode-flag (string-ref file-desc 0))
        (filename (substring file-desc 1)))
    (values (case mode-flag
              ((#\>) :output)
              ((#\<) :input)
              (#t (error "Bad automatic file mode descriptor: ~a" file-desc)))
            filename)))

(define (open-auto-file filename)
  (mvbind (mode name) (parse-auto-filename filename)
    (open-file name :mode (case mode
                            ((:input) :read)
                            ((:output) :write)))))

(define (->auto-file filename)
  (let ((auto-port (hash-ref *auto-files* filename #f)))
    (if (and auto-port (port-open? auto-port))
        auto-port
        (let ((port (open-auto-file filename)))
          (hash-set! *auto-files* filename port)
          port))))

(define (reset-auto-files!)
  (dohash (filename port *auto-files*) 
     (catch-all
      (close-port port))
     (hash-clear! *auto-files*)))

(define (->file file)
  (if (string? file)
      (->auto-file file)
      file))
