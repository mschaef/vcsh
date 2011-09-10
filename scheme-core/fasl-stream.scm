
;;;; fasl-stream.scm --
;;;;
;;;; FASL Streams - Support for writing Fast Load files.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define-structure fasl-stream
  target-port
  (objects-to-write :default ()))

(define-structure fasl-op
  fasl-opcode
  param-objects)

(define (open-fasl-output-stream port)
  "Open a new FASL output stream targeting <port>."
  (make-fasl-stream :target-port port))

(define (fasl-write stream object)
  "Write <object> to FASL stream <stream>.  Note that the object is not actually writen
   to the stream's target port until the stream itself is closed."
  (set-fasl-stream-objects-to-write! stream (cons object (fasl-stream-objects-to-write stream)))
  stream)

(define (fasl-write-op stream fasl-opcode . params)
  "Writes an arbitrary FASL opcode, <fasl-opcode>, to FASL stream <stream>. The paramater
   objects in the list <param-objects>, are written to the FASL stream immediately after
   the opcode."
  (fasl-write stream (make-fasl-op :fasl-opcode fasl-opcode
                                   :param-objects params)))

(define (abort-fasl-writes stream)
  "Aborts all pending writes to <stream> since the stream was opened
   or last committed."
  (set-fasl-stream-objects-to-write! stream ())
  ())

(define (commit-fasl-writes stream)
  "Commits FASL stream <stream>. This is the FASL stream operation that actually
   writes the stream's content's to the target port."
  (let ((shared-structure-table (find-shared-structures (fasl-stream-objects-to-write stream))))

    (fast-write-opcode system::FASL_OP_RESET_READER_DEFS (fasl-stream-target-port stream))

    (dolist (obj (reverse (fasl-stream-objects-to-write stream)))
      (cond ((fasl-op? obj)
             (fast-write-opcode (fasl-op-fasl-opcode obj) (fasl-stream-target-port stream))
             (dolist (obj (fasl-op-param-objects obj))
               (fast-write-using-shared-structure-table obj
                                                        (fasl-stream-target-port stream)
                                                        shared-structure-table)))
            (#t
             (fast-write-using-shared-structure-table obj (fasl-stream-target-port stream)
                                                      shared-structure-table))))
    stream))

(defmacro (with-fasl-stream s port . code)
  `(let ((,s (open-fasl-output-stream ,port)))
     (unwind-protect
      (lambda () ,@code)
      (lambda () (commit-fasl-writes ,s)))))

(defmacro (with-fasl-file s filename . code)
  (with-gensyms (port-sym)
    `(with-port ,port-sym (open-output-file ,filename :binary)
       (with-fasl-stream ,s ,port-sym
           ,@code))))
