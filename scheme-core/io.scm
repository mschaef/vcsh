
;;;; io.scm --
;;;;
;;;; I/O Utilities.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (internal-files)
  (map car (%internal-files)))

(define (find-internal-file filename)
  "Searched for <filename> in the current list of internal files. Returns
   either the c-data port associated with the file if the file exists,  or #f
   if the file does not exist."
  (aif (assoc filename (%internal-files))
       (cdr it)
       #f))

(define (input-port? port)
  "Determines if <port> is an input port. Returns the port if it is an
input port and #f otherwise."
  (if (and (port? port)
            (eq? (port-mode port) :input))
      port
      #f))

(define (output-port? port)
  "Determines if <port> is an input port. Returns the port if it is an
input port and #f otherwise."
  (if (and (port? port)
           (eq? (port-mode port) :output))
      port
      #f))

(define (port-closed? port)
  "Determines if <port> is closed. Returns the port if it is closed,
and #f otherwise. If <port> is not a port, throws an error."
  (check port? port)
  (if (eq? (port-mode port) :closed)
      port
      #f))

(define (port-open? port)
  "Determines if <port> is open. Returns the port if it is open, and
#f otherwise. If <port> is not a port, throws an error."
  (check port? port)
  (if (not (eq? (port-mode port) :closed))
      port
      #f))

(define (port-at-end? port)
  "Returns a boolean signaling if the input port <port> is at its end."
  (eof-object? (peek-char port)))

(define (call-with-port port fn) ;; REVISIT: This file name doesn't seem quite right, in comparision to call-with-output-port
  "Returns the result(s) of calling <fn> with the one argument <port>. <port> is
   guaranteed to be closed with a call to close-port when <fn> returns."
  (unwind-protect (lambda () (fn port))
                  (lambda () (close-port port))))

(defmacro (with-port v open-expr . code)
  "Runs <code> with an open port bound to <v>. The port is initialized with
  <open-expr> and is guaranteed to be closed with a call to close-port when
  <code> returns."
  `(call-with-port ,open-expr (lambda (,v) ,@code)))

(define (call-with-input-file filename fn :optional (mode :text))
  "Returns the result(s) of calling <fn> with a file input port for the
   file named by <filename>. The resulting port is guaranteed to be closed
   with a call to close-port when <fn> returns."
  (call-with-port (open-input-file filename mode) fn))

;; REVISIT: with-input-file

(define (call-with-output-file filename fn :optional (mode :text))
  "Returns the result(s) of calling <fn> with a file output port for the
   file named by <filename>. The resulting port is guaranteed to be closed
   with a call to close-port when <fn> returns."
  (call-with-port (open-output-file filename mode) fn))

;; REVISIT: with-output-file

;; REVISIT: a compiler error keeps these definitions from working, currently
;; the compiler tries to serialize the subr bound to close-port at the
;; tine of compilation. July 14, 2007
;;;
;;; (define close-input-port close-port)
;;; (define close-output-port close-port)

(define (file-exists? filename)
  "Returns <filename> if the file named by <filename> exists, #f otherwise."
  (if (file-details filename #t)
      filename
      #f))


(define (read-text-until-character port char-specifier :keyword (length-limit #f))
  "Reads text from <port> until encountering either a character matching
   <char-specifier> or the end of the file. The text read from the port
   is returned  as a string and the terminating character is on the input
   port. <char-specifier> can be either a character, in which case it is the
   terminating character, or a predicate on characters in which case a #t
   return value will signal the terminating character.  The keyword argument
   <length-limit> allows an optional bound on the number of to be read. See
   also (read-line ...)"
  (let ((char-matches? (cond ((char? char-specifier) #L(char=? _ char-specifier))
                             ((procedure? char-specifier) char-specifier)
                             (#t (error "Invalid character specifier: ~a" char-specifier)))))
    (let ((op (open-output-string)))
      (set-port-translate-mode! op #f)
      (let loop ((ch (peek-char port)) (ii 0))
        (cond ((or (eof-object? ch) (char-matches? ch)
                   (and length-limit (>= ii length-limit)))
               (get-output-string op))
              (#t
               (write-strings op (read-char port))
               (loop (peek-char port) (+ 1 ii))))))))

(define (read-exact-number :optional (port (current-input-port)))
  "Reads an exact number from input port <port>, starting at the current
   port location. Throws an error if there is no exact number to be read
   at that location. The port is left at the character immediately following
   the number."
  (let ((num (read-from-string (read-text-until-character port (negate char-numeric?)))))
    (when (eof-object? num)
          (error "No exact number found in port: ~s" port))
    num))

;;; I/O utilities

(define (current-input-port)
   (%control-field system::VMCTRL_CURRENT_INPUT_PORT))

(define (current-output-port)
   (%control-field system::VMCTRL_CURRENT_OUTPUT_PORT))

(define (current-error-port)
  (%control-field system::VMCTRL_CURRENT_ERROR_PORT))

(define (current-debug-port)
  (%control-field system::VMCTRL_CURRENT_DEBUG_PORT))

(define (check-for-text-mode-input-port port)
  (check (and input-port? (not binary-port?)) port))

(define (check-for-text-mode-output-port port)
  (check (and output-port? (not binary-port?)) port))

(define (set-current-input-port port)
  "Sets the current standard input port to <port>, returning the previous
   value. Throws an error if <port> is not a text mode input port."
  (check-for-text-mode-input-port port)
  (begin-1
   (%control-field system::VMCTRL_CURRENT_INPUT_PORT)
   (%set-control-field system::VMCTRL_CURRENT_INPUT_PORT port)))

(define (set-current-output-port port)
  "Sets the current standard output port to <port>, returning the previous
   value. Throws an error if <port> is not a text mode output port."
  (check-for-text-mode-output-port port)
  (begin-1
   (%control-field system::VMCTRL_CURRENT_OUTPUT_PORT)
   (%set-control-field system::VMCTRL_CURRENT_OUTPUT_PORT port)))

(define (set-current-error-port port)
  "Sets the current standard error port to <port>, returning the previous
   value. Throws an error if <port> is not a text mode output port."
  (check-for-text-mode-output-port port)
  (begin-1
   (%control-field system::VMCTRL_CURRENT_ERROR_PORT)
   (%set-control-field system::VMCTRL_CURRENT_ERROR_PORT port)))

(define (set-current-debug-port port)
  "Sets the current standard debug port to <port>, returning the previous
   value. Throws an error if <port> is not a text mode output port."
  (check-for-text-mode-output-port port)
  (begin-1
   (%control-field system::VMCTRL_CURRENT_DEBUG_PORT)
   (%set-control-field system::VMCTRL_CURRENT_DEBUG_PORT port)))

(define *location-mapping* #f) ; REVISIT: fix the need for this fwd decl.

(define (read-from-string string :optional (location-map #f))
  "Reads the first Lisp object from <string>. Any read errors will be
   propragated to the caller. A <location-map> can optionally be specified
   to capture the location of input forms read from the string, but by default
   no location information will be retained."
  (dynamic-let ((*location-mapping* location-map))
    (read (open-input-string string) #f)))

(define (readall port :optional (object-reader read))
  "Reads objects from <port> until the end of the port is reached. The
   objects read are returned as a list. <port> can also be provided as
   a string, in which case open-input-string is used to create an input
   string port from which objects are read."
  (if (string? port)
      (readall (open-input-string port) object-reader)
      (let loop ((objects ()))
        (if (eof-object? (flush-whitespace port))
            (reverse! objects)
            (loop (cons (object-reader port) objects))))))

(define (read-lines port)
  (readall port read-line))

(define (file-lines filename)
  (with-port ip (open-input-file filename)
    (read-lines ip)))

(define (call-with-output-port fn port)
  "Calls function <fn>, with the current output port bound to <port>."
  (let ((last-port (current-output-port)))
    (unwind-protect
     (lambda ()
       (set-current-output-port port)
       (fn))
     (lambda ()
       (set-current-output-port last-port)))))

(define (call-with-input-port fn port)
  "Calls function <fn>, with the current input port bound to <port>."
  (let ((last-port (current-input-port)))
    (unwind-protect
     (lambda ()
       (set-current-input-port port)
       (fn))
     (lambda ()
       (set-current-input-port last-port)))))

(define (call-with-output-to-string fn)
  "Calls function <fn>, capturing output to the current output port as a string. The
   current output port is reset to its original value on exit.  If <fn> alters the
   current output port itself, output after the change will not necessarily be captured."
  (let ((output-string (open-output-string)))
    (call-with-output-port fn output-string)
    (get-output-string output-string)))

(defmacro (with-output-port port . code)
  `(call-with-output-port (lambda () ,@code) ,port))

(defmacro (with-input-port port . code)
  `(call-with-input-port (lambda () ,@code) ,port))

(defmacro (with-output-to-string . code)
  `(call-with-output-to-string (lambda () ,@code)))

;;; TODO: with-input-from-string

(define *use-debug-printer* #f)

(define (write obj :optional (port (current-output-port)))
  (if *use-debug-printer*
      (%debug-printer obj port #t))
      (printer obj port #t))

(define (display obj :optional (port (current-output-port)))
  (if *use-debug-printer*
      (%debug-printer obj port #f))
      (printer obj port #f))

(defmacro (with-temporary-file filename-var prefix . code)
  (with-gensyms (internal-filename-var)
    ;; We store the temporary filename twice, once in a local variable
    ;; that's not visible outside the macro definition itself. This allows
    ;; the macro to guarantee that it deletes the file that was created,
    ;; even if <code> rebinds <filename-var>.
    `(let* ((,internal-filename-var (temporary-file-name ,prefix))
            (,filename-var ,internal-filename-var))
       (unwind-protect (lambda () ,@code)
                       (lambda ()
                         (when (file-exists? ,internal-filename-var)
                           (delete-file ,internal-filename-var)))))))

