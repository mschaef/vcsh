
;;;; fasl-stream.scm --
;;;;
;;;; FASL Streams - Support for writing Fast Load files.
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (make-fasl-op opcode params)
  {'type-of 'fasl-op
   :fasl-opcode opcode
   :param-objects params})

(define (fasl-op? obj)
  (eq? (type-of obj) 'fasl-op))

(define (open-fasl-output-stream port)
  "Open a new FASL output stream targeting <port>."
  {'type-of 'fasl-output-stream
   :port port
   :output-objs ()})

(define (fasl-write stream object)
  "Write <object> to FASL stream <stream>.  Note that the object is not actually writen
   to the stream's target port until the stream itself is closed."
  (hash-set! stream :output-objs (cons object (:output-objs stream)))
  stream)

(define (fasl-write-op stream fasl-opcode . params)
  "Writes an arbitrary FASL opcode, <fasl-opcode>, to FASL stream <stream>. The paramater
   objects in the list <param-objects>, are written to the FASL stream immediately after
   the opcode."
  (fasl-write stream (make-fasl-op fasl-opcode params)))

(define (abort-fasl-writes stream)
  "Aborts all pending writes to <stream> since the stream was opened
   or last committed."
  (hash-set! stream :output-objs ())
  ())

(define (commit-fasl-writes stream)
  "Commits FASL stream <stream>. This is the FASL stream operation that actually
   writes the stream's content's to the target port."
  (let ((shared-structure-table (find-shared-structures (:output-objs stream))))

    (fast-write-opcode system::FASL_OP_RESET_READER_DEFS (:port stream))

    (dolist (obj (reverse (:output-objs stream)))
      (cond ((fasl-op? obj)
             (fast-write-opcode (:fasl-opcode obj) (:port stream))
             (dolist (obj (:param-objects obj))
               (fast-write-using-shared-structure-table obj
                                                        (:port stream)
                                                        shared-structure-table)))
            (#t
             (fast-write-using-shared-structure-table obj (:port stream)
                                                      shared-structure-table))))
    stream))

(defmacro (with-fasl-stream s port . code)
  `(let ((,s (open-fasl-output-stream ,port)))
     (unwind-protect
      (lambda () ,@code)
      (lambda () (commit-fasl-writes ,s)))))

(defmacro (with-fasl-file s filename . code)
  (with-gensyms (port-sym)
    `(with-port ,port-sym (open-file ,filename :mode :write :encoding :binary)
       (with-fasl-stream ,s ,port-sym
           ,@code))))
