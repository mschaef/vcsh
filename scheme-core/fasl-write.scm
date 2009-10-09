;;;; fasl-write.scm
;;;; February 23rd, 2006
;;
;; FASL Object Writer - Writes objects in FASL format. FASL format represents
;; objects with a  one byte type code followed by a number of data bytes that
;; varies based on the type of the object. There are two layers to this package:
;; fast-* and fasl-*.  fast-* routines are low level routines that encode
;; objects in binary form, but do not handle shared or circular structures, which
;; can send them into an infinite loop.  The fasl-* layer is a higher layer
;; set of routines, built on fast-*, that are used to generate FASL files.
;; These routines detect circularity and shared structure and generate
;; SET/REF opcodes to reconstruct it at load-time.

;; TODO: The API design for the FASL writer is pretty confusing.

(define *fasl-write-debugging* #f)
(define *fasl-write-check-structure-sharing* #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fast-*

(define *fasl-bytes-written* 0)

(define (fast-write-float value port)
  "Wraps write-binary-flonum, adding debugging output to the current error
   port if *fasl-write-debugging* is true."
  (incr! *fasl-bytes-written* 8)
  (when *fasl-write-debugging*
    (format (current-error-port) " [~s]" value))
  (write-binary-flonum value port))

(define (fast-write-integer value length signed port)
  "Wraps write-binary-fixnum, adding debugging output to the current error
   port if *fasl-write-debugging* is true."
  (incr! *fasl-bytes-written* length)
  (when *fasl-write-debugging*
    (format (current-error-port) " ~s" value))
  (write-binary-fixnum value length signed port))

(define (fast-write-characters value port)
  "Wraps write-binary-string, adding debugging output to the current error
   port if *fasl-write-debugging* is true."
  (when *fasl-write-debugging*
    (incr! *fasl-bytes-written* (length value))
    (format (current-error-port) " ~s" value))
  (write-binary-string value port))

(define (fast-write-opcode code port)
  "Writes a type code <code> to <port>."
  (fast-write-integer code 1 #f port))

(define (fast-write-comment text port)
  "Writes <text> in FASL comment format on <port>. If <text> is composed of
   multiple lines of text, it is broken up into multiple FASL comment records."
  (aif (string-search "\n" text)
    (begin
      (fast-write-comment (substring text 0 it) port)
      (fast-write-comment (substring text (+ 1 it)) port))
    (begin
      (fast-write-opcode 59 port)
      (fast-write-characters " " port)
      (fast-write-characters text port)
      (fast-write-characters "\n" port))))

;; There are some subtleties in this assignment of opcode numbers:
;;
;; 1. To allow textual comments to be incorporated in FASL files, the #\;
;;    character is interpreted as a 'skip until #\newline or #\cr' opcode.
;;
;; 2. Since we don't know what our end of line convention is, we treat both
;;    #\newline and #\cr as no-ops. That way, no matter how a textual comment
;;    string ends, we can still read past the line terminator.
;;
;; 3. #\# is treated the same way as #\;. to allow FASL files on Unix machines
;;    to point to an interpreter, like a shell script.
;;
;; 4. Control+Z is a no-op, since it's useful to cause DOS machines to stop
;;   typing a file to the screen.
;;
;; 5. #\nul is also a no-op, since it seems too important to use for arbitrary
;;    reasons.

;; 0 reserved to allow for Unicode double byte characters, somehow
(define FASL-OP-NIL                  1  )
(define FASL-OP-TRUE                 2  )
(define FASL-OP-FALSE                3  )
(define FASL-OP-CHARACTER            4  )
(define FASL-OP-LIST                 8  )
(define FASL-OP-LISTD                9  )
(define FASL-OP-NOP-1                10 ) ; #\newline
(define FASL-OP-NOP-2                13 ) ; #\cr
(define FASL-OP-FIX8                 16 )
(define FASL-OP-FIX16                17 )
(define FASL-OP-FIX32                18 )
(define FASL-OP-FIX64                19 )
(define FASL-OP-FLOAT                21 )
(define FASL-OP-COMPLEX              22 )
(define FASL-OP-STRING               24 )
(define FASL-OP-NOP-3                26 ) ; Control+Z
(define FASL-OP-PACKAGE              28 )
(define FASL-OP-RSYMBOL              29 )
(define FASL-OP-VECTOR               30 )
(define FASL-OP-BASE-INSTANCE        32 )
(define FASL-OP-INSTANCE             33 )
(define FASL-OP-HASH                 34 )
(define FASL-OP-COMMENT-1            35 ) ; #\#
(define FASL-OP-CLOSURE              36 )
(define FASL-OP-MACRO                37 )
(define FASL-OP-SYMBOL               48 )
(define FASL-OP-SUBR                 50 )
(define FASL-OP-COMMENT-2            59 ) ; #\;
(define FASL-OP-STRUCTURE            60 )
(define FASL-OP-STRUCTURE-LAYOUT     61 )
(define FASL-OP-FAST-OP-0            64 )
(define FASL-OP-FAST-OP-1            65 )
(define FASL-OP-FAST-OP-2            66 )
(define FASL-OP-INSTANCE-MAP         96 )
(define FASL-OP-RESET-READER-DEFS    192)
(define FASL-OP-READER-DEFINITION    193)
(define FASL-OP-READER-REFERENCE     194)
(define FASL-OP-LOADER-DEFINEQ       208)
(define FASL-OP-LOADER-DEFINEA0      210)
(define FASL-OP-LOADER-APPLY0        216)
(define FASL-OP-BEGIN-LOAD-UNIT      224)
(define FASL-OP-END-LOAD-UNIT        225)
;; 254, 255 reserved for Unicode Byte Order Marker

;; Note: these next symbols are generated at load/compile time, and used by the
;; FASL writer to keep track of shared object indices and a table of shared
;; structure layout information.  There is a possibility that when this file,
;; fasl-write.scm, is itself compiled and FASL written, that the presence of
;; this symbol in the output stream will cause the shared object index counter
;; to be overwritten... Beware!

(define *fasl-index-key* (gensym "fasl-index-key"))
(define *fasl-structure-layout-key* (gensym "fasl-structure-layout-key"))

(define (fast-write-using-shared-structure-table object port shared-structure-table)
  "Writes <object> on <port> in FASL format. <shared-structure-table> is a hash table
  `mapping shared objects to object IDs.  It is used to avoid writing shared
   objects (including circular references) more than once. <shared-structure-table> can
   also be #f, which disables shared structure detection. In this case, this
   function will not terminate when passed a circular structure."

  (define (length-excluding-shared xs)
    "Returns the length of the object <xs> and a boolean indicating if the
   measured list has a non-null final CDR. If <xs> is a list, it stops
   counting if it encounters structures in the hash <shared-structure-table>,
   other than the first instance of <xs> itself."
    (define (is-shared? object)
      (and shared-structure-table
           (hash-has? shared-structure-table object)))
    (if (pair? xs)
        (let loop ((len 1) (xs (cdr xs)))
          (if (or (atom? xs)
                  (is-shared? xs))
              (values len (not (null? xs)))
              (loop (+ len 1) (cdr xs))))
        (values (length xs) #f)))

  (define (check-sharing-and-write object)
    (if (and shared-structure-table
             (hash-has? shared-structure-table object))
        (aif (hash-ref shared-structure-table object)
             (begin
               (when *fasl-write-debugging* (newline))
               (fast-write-opcode FASL-OP-READER-REFERENCE port)
               (fast-write-object it))
             (begin
               (let ((next-index (hash-ref shared-structure-table *fasl-index-key*)))
                 (hash-set! shared-structure-table *fasl-index-key* (+ next-index 1))
                 (hash-set! shared-structure-table object next-index)
                 (when *fasl-write-debugging* (newline))
                 (fast-write-opcode FASL-OP-READER-DEFINITION port)
                 (fast-write-object next-index)
                 (fast-write-object object))))
	(fast-write-object object)))

  (define (check-sharing-and-write-instance-map inst)
    (let ((inst-map (%instance-map inst)))
      (define (write-map)
        (fast-write-opcode FASL-OP-INSTANCE-MAP port)
        (check-sharing-and-write (%instance-proto inst))
        (fast-write-object (direct-instance-slots inst)))
      (if (and shared-structure-table
               (hash-has? shared-structure-table inst-map))
          (aif (hash-ref shared-structure-table inst-map)
               (begin
                 (when *fasl-write-debugging* (newline))
                 (fast-write-opcode FASL-OP-READER-REFERENCE port)
                 (fast-write-object it))
               (begin
                 (let ((next-index (hash-ref shared-structure-table *fasl-index-key*)))
                   (hash-set! shared-structure-table *fasl-index-key* (+ next-index 1))
                   (hash-set! shared-structure-table inst-map next-index)
                   (when *fasl-write-debugging* (newline))
                   (fast-write-opcode FASL-OP-READER-DEFINITION port)
                   (fast-write-object next-index)
                   (write-map))))
          (write-map))))

  (define (fast-write-structure-layout layout)
    (define (do-write)
      (fast-write-opcode FASL-OP-STRUCTURE-LAYOUT port)
      (check-sharing-and-write layout))
    (if shared-structure-table
        (let ((layout-table (hash-ref shared-structure-table  *fasl-structure-layout-key*)))
          (aif (hash-ref layout-table layout)
               (begin
                 (when *fasl-write-debugging* (newline))
                 (fast-write-opcode FASL-OP-READER-REFERENCE port)
                 (fast-write-object it))
               (begin
                 (let ((next-index (hash-ref shared-structure-table *fasl-index-key*)))
                   (hash-set! shared-structure-table *fasl-index-key* (+ next-index 1))
                   (hash-set! layout-table layout next-index)
                   (when *fasl-write-debugging* (newline))
                   (fast-write-opcode FASL-OP-READER-DEFINITION port)
                   (fast-write-object next-index)
                   (do-write)))))
          (do-write)))

  (define (fast-write-object object)
    (when *fasl-write-debugging*
      (newline (current-error-port)))

    (case (%representation-of object)
      ((nil)
       (fast-write-opcode FASL-OP-NIL port))

      ((boolean)
       (fast-write-opcode (if object FASL-OP-TRUE FASL-OP-FALSE) port))

      ((character)
       (fast-write-opcode FASL-OP-CHARACTER port) ; 4 for UCS 16, 5 for UCS 32
       (fast-write-integer (char->integer object) 1 #f port))

      ((cons)
       (values-bind (length-excluding-shared object shared-structure-table) (len dotted?)
	 (fast-write-opcode (if dotted? FASL-OP-LISTD FASL-OP-LIST) port)
	 (check-sharing-and-write len)
	 (let loop ((i 0) (xs object))
	   (cond ((< i len)
		  (check-sharing-and-write (car xs))
		  (loop (+ i 1) (cdr xs)))
		 (dotted?
		  (check-sharing-and-write xs))))))

      ((fixnum)
       (cond
	((and (>= object -128) (<= object 127))
	 (fast-write-opcode FASL-OP-FIX8 port)
	 (fast-write-integer object 1 #t port))
	((and (>= object -32768) (<= object 32767))
	 (fast-write-opcode FASL-OP-FIX16 port)
	 (fast-write-integer object 2 #t port))
	((and (>= object -2147483648) (<= object 2147483647))
	 (fast-write-opcode FASL-OP-FIX32 port)
	 (fast-write-integer object 4 #t port))
	(#t
	 (fast-write-opcode FASL-OP-FIX64 port)
	 (fast-write-integer object 8 #t port))))

      ((flonum)
       (fast-write-opcode FASL-OP-FLOAT port)
       (fast-write-float object port))

      ((complex)
       (fast-write-opcode FASL-OP-COMPLEX port)
       (fast-write-float (real-part object) port)
       (fast-write-float (imag-part object) port))

      ((string)
       (fast-write-opcode FASL-OP-STRING port)
       (check-sharing-and-write (length object))
       (fast-write-characters object port))

      ((package)
       (fast-write-opcode FASL-OP-PACKAGE port)
       (check-sharing-and-write (package-name object)))

      ((symbol)
       (fast-write-opcode FASL-OP-SYMBOL port)
       (check-sharing-and-write (symbol-name object))
       (check-sharing-and-write (symbol-package object)))

      ((vector)
       (fast-write-opcode FASL-OP-VECTOR port)
       (check-sharing-and-write (length object))
       (dolist (x object)
         (check-sharing-and-write x)))

      ((instance)
       (fast-write-opcode FASL-OP-INSTANCE port)
       (check-sharing-and-write-instance-map object)
       (check-sharing-and-write (map #L(slot-ref object _)
                                     (direct-instance-slots object))))

      ((hash)
       (fast-write-opcode FASL-OP-HASH port)
       (check-sharing-and-write (eq? :eq (hash-type object)))
       (check-sharing-and-write (hash->a-list object)))

      ((subr)
       (fast-write-opcode FASL-OP-SUBR port)
       (check-sharing-and-write (procedure-name object)))

      ((closure)
       (fast-write-opcode FASL-OP-CLOSURE port)
       (check-sharing-and-write (%closure-env object))
       (check-sharing-and-write (%closure-code object))
       (check-sharing-and-write (%property-list object)))

      ((macro)
       (fast-write-opcode FASL-OP-MACRO port)
       (check-sharing-and-write (%macro-transformer object)))

      ((structure)
       (fast-write-opcode FASL-OP-STRUCTURE port)
       (fast-write-structure-layout (%structure-layout object))
       (let ((len (%structure-length object)))
         (check-sharing-and-write len)
         (dotimes (ii len)
           (check-sharing-and-write (%structure-ref object ii)))))

      ((fast-op)
       (values-bind (parse-fast-op object #f) (fop-opcode args)
          (fast-write-opcode (case (length args)
                               ((0) FASL-OP-FAST-OP-0)
                               ((1) FASL-OP-FAST-OP-1)
                               ((2) FASL-OP-FAST-OP-2)
                               (#t (error "Unsupported fast-op arity: ~s" object)))
                             port)
          (check-sharing-and-write fop-opcode)
          (dolist (arg args)
            (check-sharing-and-write arg))))

      (#t
       (error "fast-write of unsupported type ~a : ~s" (%representation-of object) object))))

  (check-sharing-and-write object))

;;;; fasl-*

(define (shared-structures object) ; REVISIT: Switch to tail recursive algorithm (and the writer itself)
  "Returns an identity hash of all objects referenced by <object>
   more than once. This includes both circular and shared structure. The
   value associated with each hash is #f."
  (let ((visited-objects (make-hash :eq))
        (visited-layouts (make-hash :eq)))
    (let visit ((o object))
      (unless (%immediate? o) ; Ignore immediates, they're shared by definition
        (cond ((hash-has? visited-objects o)
               (hash-set! visited-objects o #t))
              (#t
               (hash-set! visited-objects o #f)
               (case (%representation-of o)
                 ((closure)
                  (visit (%closure-code o))
                  (visit (%closure-env o))
                  (visit (%property-list o)))
                 ((macro)
                  (visit (%macro-transformer o)))
                 ((cons)
                  (visit (car o))
                  (visit (cdr o)))
                 ((symbol)
                  (visit (symbol-package o))
                  (visit (symbol-name o)))
                 ((vector)
                  (dolist (x o)
                    (visit x)))
                 ((structure)
                  (hash-set! visited-layouts (%structure-layout o) #f)
                  (dotimes (ii (%structure-length o))
                    (visit (%structure-ref o ii))))
                 ((instance)
                  (visit (%instance-proto o))
                  (visit (%instance-map o))
                  (dolist (slot-name (direct-instance-slots o))
                    (visit slot-name)
                    (visit (slot-ref o slot-name))))
                 ((hash)
                  (dolist (k/v (hash->a-list o))
                    (visit (car k/v))
                    (visit (cdr k/v))))
                 ((fast-op)
                  (dolist (op-piece (%fast-op-args o))
                    (visit op-piece)))
                 (#t
                  ()
                  ))))))
    (let ((table (make-hash :eq)))
      (dohash (k v visited-objects (hash-set! (hash-set! table *fasl-structure-layout-key*
                                                         visited-layouts)
                                              *fasl-index-key* 0))
          (when v
            (hash-set! table k #f))))))

(define (fast-write/ignore-sharing object port)
  "Writes <object> to <port> in FASL format. No attempt is made to preserve
   structure sharing; if <object> is circular, this function will never
   terminate."
  (fast-write-using-shared-structure-table object port #f))

(define (fast-write object port)
  "Writes <object> to <port> in FASL format."
  (fast-write-using-shared-structure-table object port (shared-structures object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FASL Layer

(define-structure fasl-stream
  target-port
  (objects-to-write :default ())
  visited-objects)

(define-structure fasl-comment
  comment-text)

(define-structure fasl-op
  fasl-opcode
  param-objects)

(define (open-fasl-output-stream port)
  "Open a new FASL output stream targeting <port>."
  (make-fasl-stream
   :target-port port
   :visited-objects (make-hash :eq)))

(define (fasl-write object stream)
  "Write <object> to FASL stream <stream>.  Note that the object is not actually writen
   to the stream's target port until the stream itself is closed."
  (set-fasl-stream-objects-to-write! stream (cons object (fasl-stream-objects-to-write stream)))
  stream)

(define (fasl-write-comment comment-text stream)
  "Writes a human readable comment, <comment-text>, to FASL stream <stream>."
  (fasl-write (make-fasl-comment :comment-text comment-text) stream))

(define (fasl-write-op fasl-opcode param-objects stream)
  "Writes an arbitrary FASL opcode, <fasl-opcode>, to FASL stream <stream>. The paramater
   objects in the list <param-objects>, are written to the FASL stream immediately after
   the opcode."
  (fasl-write (make-fasl-op
               :fasl-opcode fasl-opcode
               :param-objects param-objects)
	      stream))

(define (abort-fasl-writes stream)
  "Aborts all pending writes to <stream> since the stream was opened
   or last committed."
  (set-fasl-stream-objects-to-write! stream ())
  ())

(define (commit-fasl-writes stream)
  "Commits FASL stream <stream>. This is the FASL stream operation that actually
   writes the stream's content's to the target port."
  (let ((shared-structure-table (if *fasl-write-check-structure-sharing*
				    (shared-structures (fasl-stream-objects-to-write stream))
				    #f))
	(index 0))

    (define (fasl-index< x y)
      (cond ((not x) (fasl-index< -1 y))
	    ((not y) (fasl-index< x -1))
	    (#t (< x y))))

    (define (display-shared-structures)
      (format (current-error-port) "\n\nShared Object Indices\n--------------------------------\n")
      (dolist (obj/index (qsort (hash->a-list shared-structure-table) fasl-index< cdr))
	(format (current-error-port) "~a: ~a\n" (cdr obj/index) (car obj/index)))
      (newline))

    (hash-set! shared-structure-table *fasl-index-key* 0)

    (when *fasl-write-check-structure-sharing*
      (fast-write-opcode FASL-OP-RESET-READER-DEFS (fasl-stream-target-port stream)))

    (dolist (obj (reverse (fasl-stream-objects-to-write stream)))
      (cond ((fasl-op? obj)
	     (fast-write-opcode (fasl-op-fasl-opcode obj) (fasl-stream-target-port stream))
             (dolist (obj (fasl-op-param-objects obj))
               (fast-write-using-shared-structure-table obj
                                                        (fasl-stream-target-port stream)
                                                        shared-structure-table)))
	    ((fasl-comment? obj)
	     (fast-write-comment (fasl-comment-comment-text obj) (fasl-stream-target-port stream)))
	    (#t
	     (fast-write-using-shared-structure-table obj (fasl-stream-target-port stream)
						      shared-structure-table))))

    (when *fasl-write-debugging*
      #;(display-shared-structures)
      #f
      )

    stream))

(defmacro (with-fasl-stream s port . code)
  `(let ((,s (open-fasl-output-stream ,port)))
     (unwind-protect (lambda () ,@code)
		     (lambda () (commit-fasl-writes ,s)))))

(defmacro (with-fasl-file s filename . code)
  (with-gensyms (port-sym)
    `(with-port ,port-sym (open-output-file ,filename :binary)
       (with-fasl-stream ,s ,port-sym
	 ,@code))))


(define close-fasl-stream commit-fasl-writes)