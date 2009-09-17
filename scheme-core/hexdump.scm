;;;; hexdump.scm
;;;; January 12th, 2006
;;;; Mike Schaeffer
;
; A utility to dump binary files as hexadecimal

(define (number->field-string num width base)
  (let* ((s (string-append (make-string #\0 width) (number->string num base)))
	 (l (string-length s)))
    (substring s (- l width))))

;;;; Startup Code

(define *input-filename* #f)
(define *bytes-per-line* 16)
(define *relative-offset* #f)
(define *show-offset* #t)
(define *show-string* #t)
(define *show-bytes* #t)
(define *decimal-offset* #f)

(define-command-argument ("bytes-per-line")
  "Specifies the number of bytes shown per line"
  (let ((val (arg-integer-value arg)))
    (when (<= val 0)
      (throw 'invalid-argument "bytes-per-line must be >0.")) ; !!! this message should print
    (set! *bytes-per-line* val)))

(define-command-argument ("no-offset")
  "Suppresses display of the file offset."
  (set! *show-offset* #f))

(define-command-argument ("decimal-offset")
  "Displays the offset in decimal, rather than hexadecimal."
  (set! *decimal-offset* #t))

(define-command-argument ("no-string")
  "Suppresses display of the string representation."
  (set! *show-string* #f))

(define-command-argument ("no-bytes")
  "Suppresses display of the individual bytes."
  (set! *show-bytes* #f))

(define-command-argument ("relative-offset")
  "Displays file addresses with a relative offset from the specified byte."
  (set! *relative-offset* (arg-integer-value arg)))

(define-file-argument-handling
  (set! *input-filename* arg))

(define (dump-string str)
  (define (next-character index)
    (cond ((and *show-bytes* (< index (string-length str)))
	   (format #t "~a " (number->field-string (char->integer (string-ref str index)) 2 16))
	   (next-character (+ index 1)))
	  (*show-string*
	   (format #t " ~s\n" str))
	  (#t
	   (newline))))
  (next-character 0))

(define (dump-port ip)
  (define (loop ofs)
    (let ((next (read-binary-string *bytes-per-line* ip)))
      (unless (eof-object? next)
	(when *show-offset*
	  (format #t "~a: " (number->field-string ofs 8 (if *decimal-offset* 10 16)))
	  (when  *relative-offset*
	    (let ((delta (- ofs *relative-offset*)))
	      (if (< delta 0)
		  (format #t "(ofs-~a)" (number->field-string (- delta) 8 (if *decimal-offset* 10 16)))
		  (format #t "(ofs+~a)" (number->field-string (- ofs *relative-offset*) 8 (if *decimal-offset* 10 16))))))
	  (format #t ": "))

	(dump-string next)
	(loop (+ ofs *bytes-per-line*)))))
  (loop 0))

(define (run)
  (time
   (dynamic-let ((*info* #f))
     (catch-all ; we never want to go interactive, even for errors
      (when *input-filename*
	(format #t "; ~a\n" *input-filename*)
	(with-port input-port (open-input-file *input-filename* :binary)
	  (dump-port input-port)))))))


