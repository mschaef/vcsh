
;;;; fast-write.scm --
;;;;
;;;; Fast Load object writer
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;; Need this to support forward reference to compiler::parse-fast-op
(eval-when (:load-toplevel :compile-toplevel :execute)
  (ensure-package! "compiler"))

(defmacro (fast-write-opcode code port)
  "Writes a type code <code> to <port>."
  `(write-binary-fixnum ,code 1 #f ,port))

(define-structure sharing-map
  (indicies :default (make-hash :eq))
  (next-index :default 0)
  (structure-layouts :default (make-hash :eq)))

(define (find-shared-structures object) ; REVISIT: Switch to tail recursive algorithm (and the writer itself)
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
                  (dolist (op-piece (scheme::%fast-op-args o))
                    (visit op-piece)))
                 (#t
                  ()))))))

    (let ((smap (make-sharing-map :structure-layouts visited-layouts)))
      (dohash (object shared? visited-objects)
        (when shared?
          (hash-set! (sharing-map-indicies smap) object #f)))
      smap)))

;; Note: these next symbols are generated at load/compile time, and used by the
;; FASL writer to keep track of shared object indices and a table of shared
;; structure layout information.  There is a possibility that when this file,
;; fasl-write.scm, is itself compiled and FASL written, that the presence of
;; this symbol in the output stream will cause the shared object index counter
;; to be overwritten... Beware!

(define *fasl-index-key* (gensym "fasl-index-key"))
(define *fasl-structure-layout-key* (gensym "fasl-structure-layout-key"))

(define (fast-write-using-shared-structure-table object port smap)
  "Writes <object> on <port> in FASL format. <smap> is a hash table
  `mapping shared objects to object IDs.  It is used to avoid writing shared
   objects (including circular references) more than once. <smap> can
   also be #f, which disables shared structure detection. In this case, this
   function will not terminate when passed a circular structure."

  (define (length-excluding-shared xs)
    "Returns the length of the object <xs> and a boolean indicating if the
   measured list has a non-null final CDR. If <xs> is a list, it stops
   counting if it encounters structures in the hash <smap>,
   other than the first instance of <xs> itself."
    (define (is-shared? object)
      (and smap
           (hash-has? (sharing-map-indicies smap) object)))
    (if (pair? xs)
        (let loop ((len 1) (xs (cdr xs)))
          (if (or (atom? xs)
                  (is-shared? xs))
              (values len (not (null? xs)))
              (loop (+ len 1) (cdr xs))))
        (values (length xs) #f)))

  (define (check-sharing-and-write object)
    (if (and smap
             (hash-has? (sharing-map-indicies smap) object))
        (aif (hash-ref (sharing-map-indicies smap) object)
             (begin
               (fast-write-opcode system::FASL_OP_READER_REFERENCE port)
               (fast-write-object it))
             (begin
               (let ((next-index (sharing-map-next-index smap)))
                 (set-sharing-map-next-index! smap (+ next-index 1))
                 (hash-set! (sharing-map-indicies smap) object next-index)
                 (fast-write-opcode system::FASL_OP_READER_DEFINITION port)
                 (fast-write-object next-index)
                 (fast-write-object object))))
        (fast-write-object object)))

  (define (check-sharing-and-write-instance-map inst)
    (let ((inst-map (%instance-map inst)))
      (define (write-map)
        (fast-write-opcode system::FASL_OP_INSTANCE_MAP port)
        (check-sharing-and-write (%instance-proto inst))
        (fast-write-object (direct-instance-slots inst)))
      (if (and smap
               (hash-has? (sharing-map-indicies smap) inst-map))
          (aif (hash-ref (sharing-map-indicies smap) inst-map)
               (begin
                 (fast-write-opcode system::FASL_OP_READER_REFERENCE port)
                 (fast-write-object it))
               (begin
                 (let ((next-index (sharing-map-next-index smap)))
                   (set-sharing-map-next-index! smap (+ next-index 1))
                   (hash-set! (sharing-map-indicies smap) inst-map next-index)
                   (fast-write-opcode system::FASL_OP_READER_DEFINITION port)
                   (fast-write-object next-index)
                   (write-map))))
          (write-map))))

  (define (fast-write-structure-layout layout)
    (define (do-write)
      (fast-write-opcode system::FASL_OP_STRUCTURE_LAYOUT port)
      (check-sharing-and-write layout))
    (if smap
        (let ((layout-table (sharing-map-structure-layouts smap)))
          (aif (hash-ref layout-table layout)
               (begin
                 (fast-write-opcode system::FASL_OP_READER_REFERENCE port)
                 (fast-write-object it))
               (begin
                 (let ((next-index (sharing-map-next-index smap)))
                   (set-sharing-map-next-index! smap (+ next-index 1))
                   (hash-set! layout-table layout next-index)
                   (fast-write-opcode system::FASL_OP_READER_DEFINITION port)
                   (fast-write-object next-index)
                   (do-write)))))
        (do-write)))

  (define (fast-write-object object)

    (case (%representation-of object)
      ((nil)
       (fast-write-opcode system::FASL_OP_NIL port))

      ((boolean)
       (fast-write-opcode (if object system::FASL_OP_TRUE system::FASL_OP_FALSE) port))

      ((character)
       (fast-write-opcode system::FASL_OP_CHARACTER port)
       (write-binary-fixnum (char->integer object) 1 #f port))

      ((cons)
       (mvbind (len dotted?) (length-excluding-shared object smap)
         (fast-write-opcode (if dotted? system::FASL_OP_LISTD system::FASL_OP_LIST) port)
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
         (fast-write-opcode system::FASL_OP_FIX8 port)
         (write-binary-fixnum object 1 #t port))
        ((and (>= object -32768) (<= object 32767))
         (fast-write-opcode system::FASL_OP_FIX16 port)
         (write-binary-fixnum object 2 #t port))
        ((and (>= object -2147483648) (<= object 2147483647))
         (fast-write-opcode system::FASL_OP_FIX32 port)
         (write-binary-fixnum object 4 #t port))
        (#t
         (fast-write-opcode system::FASL_OP_FIX64 port)
         (write-binary-fixnum object 8 #t port))))

      ((flonum)
       (fast-write-opcode system::FASL_OP_FLOAT port)
       (write-binary-flonum object port))

      ((complex)
       (fast-write-opcode system::FASL_OP_COMPLEX port)
       (write-binary-flonum (real-part object) port)
       (write-binary-flonum (imag-part object) port))

      ((string)
       (fast-write-opcode system::FASL_OP_STRING port)
       (check-sharing-and-write (length object))
       (write-binary-string object port))

      ((package)
       (fast-write-opcode system::FASL_OP_PACKAGE port)
       (check-sharing-and-write (package-name object)))

      ((symbol)
       (fast-write-opcode system::FASL_OP_SYMBOL port)
       (check-sharing-and-write (symbol-name object))
       (check-sharing-and-write (symbol-package object)))

      ((vector)
       (fast-write-opcode system::FASL_OP_VECTOR port)
       (check-sharing-and-write (length object))
       (dovec (x object)
         (check-sharing-and-write x)))

      ((instance)
       (fast-write-opcode system::FASL_OP_INSTANCE port)
       (check-sharing-and-write-instance-map object)
       (check-sharing-and-write (map #L(slot-ref object _)
                                     (direct-instance-slots object))))

      ((hash)
       (fast-write-opcode system::FASL_OP_HASH port)
       (check-sharing-and-write (eq? :eq (hash-type object)))
       (check-sharing-and-write (hash->a-list object)))

      ((subr)
       (fast-write-opcode system::FASL_OP_SUBR port)
       (check-sharing-and-write (procedure-name object)))

      ((closure)
       (fast-write-opcode system::FASL_OP_CLOSURE port)
       (check-sharing-and-write (%closure-env object))
       (check-sharing-and-write (%closure-code object))
       (check-sharing-and-write (%property-list object)))

      ((macro)
       (fast-write-opcode system::FASL_OP_MACRO port)
       (check-sharing-and-write (%macro-transformer object)))

      ((structure)
       (fast-write-opcode system::FASL_OP_STRUCTURE port)
       (fast-write-structure-layout (%structure-layout object))
       (let ((len (%structure-length object)))
         (check-sharing-and-write len)
         (dotimes (ii len)
           (check-sharing-and-write (%structure-ref object ii)))))

      ((fast-op)
       (mvbind (fop-opcode fop-name args) (compiler::parse-fast-op object #f)
         (fast-write-opcode (case (length args)
                              ((0) system::FASL_OP_FAST_OP_0)
                              ((1) system::FASL_OP_FAST_OP_1)
                              ((2) system::FASL_OP_FAST_OP_2)
                              (#t (error "Unsupported fast-op arity: ~s" object)))
                            port)
         (check-sharing-and-write fop-opcode)
         (dolist (arg args)
           (check-sharing-and-write arg))))

      (#t
       (error "fast-write of unsupported type ~a : ~s" (%representation-of object) object))))

  (check-sharing-and-write object))

(define (fast-write/ignore-sharing object port)
  "Writes <object> to <port> in FASL format. No attempt is made to preserve
   structure sharing; if <object> is circular, this function will never
   terminate."
  (fast-write-using-shared-structure-table object port #f))

(define (fast-write object port)
  "Writes <object> to <port> in FASL format."
  (fast-write-using-shared-structure-table object port (find-shared-structures object)))

