
;;;; structure.scm --
;;;;
;;;; Lisp code for structures
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;;; Internally, structures are very closely related to vectors with two
;;; additions: 1) user definable type tags and 2) specific
;;; per-structure-type accessor functions. Most of what's in this file is
;;; the machinery for implementing this higher level logic.
;;;
;;; The metadata for a structure type has three main parts:
;;;
;;; 1) type name - This is a symbol that names the type; It is the value
;;;      returned  when type-of is called on a structure of that type.
;;;      The remaining metadata for the type is stored in the 'structure-meta
;;;      property of the type name symbol.
;;;
;;; 2) layout - This is a list composed of the name of the structure type
;;;      and  list of slots annotated with layout information. In
;;;      the current implementation of structures, layout information is
;;;      just an ordinal number for each slot, the index in which the slot
;;;      is stored in the underlying vector. Note also that the layout of a
;;;      structure defines its identity as a type. Two structures with the same
;;;      (eq?) layout are considered to be of the same type.  A structure
;;;      read from a FASL file is compatible with an in-memory structure type
;;;      if its layout is the same (equal?) to an in-memory structure with
;;;      the same type name.
;;;
;;; 3) meta - This is the complete metadata for the type, including
;;;      documentation strings and other 'non-essential' components
;;;      of a structure type definition. It is stored as a list of
;;;      layout folowed by an association list of type properties.

(define (parse-structure-name structure-name)
  "Parses a structure name, returning three value: a target package
   for the structure, the base name for the structure type, and a
   flag indicating if the structure type is internal. Internal
   structure types are those prefixed with a #\\%  character."
  (runtime-check (and symbol? symbol-package (not keyword?)) structure-name
         "Bad structure name.")
  (let* ((name-text (symbol-name structure-name))
         (impl? (and (char=? #\% (string-ref name-text 0))
                     (> (length name-text) 1))))
    (values (symbol-package structure-name)
            (if impl? (substring name-text 1) name-text)
            impl?)))

(define (parse-structure-slot slot-spec prefix base-name)
  "Parses the slot specification <slot-spec>, returning a list whose car
   is the name of the slot and whose cdr is an a-list completely describing
   the properties of the slot. If the slot specification is invalid,
   throws an appropriate error."

  (define (validate-attr-value attr val)
    (case attr
      ((:documentation)
       (unless (string? val)
         (error "Invalid structure slot documentation: ~s, must be a string: ~s" slot-spec val)))
      ((:get :set)
       (unless (or (eq? val #f) (string? val))
         (error "Invalid structure slot getter/setter ~s, must be either #f or a string: ~s" slot-spec val)))
      ((:default))
      (#t
       (error "Invalid structure slot specification: ~s, bad attribute: ~s" slot-spec attr)))
    val)
    
  (let ((slot-spec (->list slot-spec)))
    (let ((slot-name (intern-keyword! (symbol-name (car slot-spec))))
          (slot-spec (if (string? (cadr slot-spec))
                         (cons* :documentation (cadr slot-spec) (cddr slot-spec))
                         (cdr slot-spec))))
      (cons slot-name
            (minimal-alist
             (p-list-fold (lambda (attr val rest)
                            (alist-cons attr (validate-attr-value attr val) rest))
                          (alist :name slot-name
                                 :default ()
                                 :get #"${prefix}${base-name}-${slot-name}"
                                 :set #"${prefix}set-${base-name}-${slot-name}!")
                          slot-spec))))))

(define (assign-structure-slot-offsets slots)
  (let recur ((index 0)
              (slots slots))
    (if (null? slots)
        ()
        (cons (list (caar slots) index)
              (recur (+ index 1) (cdr slots))))))

(define (make-structure-layout type-name slots-meta)
  (list type-name
        (assign-structure-slot-offsets slots-meta)))

(define (structure-layout-name layout)
  (car layout))

(define (structure-layout-slots layout)
  (cadr layout))

(define (structure-layout-slot-names layout)
  (map car (cadr layout)))

(define (structure-layout-slot-offset layout slot-name)
  (aand (assoc slot-name (cadr layout))
        (second it)))

(define (mark-structure-layout-orphaned! layout)
  (set-car! layout (list :orphaned (car layout))))

(define (structure-meta-layout meta)
  (car meta))

(define (parse-structure-definition structure-spec slots-spec)
  "Parses a structure definition composed of two parts, a
   <structure-spec> and a <slots-spec>, returning three values:
   the structure name, the structure metadata, and a list of
   procedures the structure definition s requested be created."
  (mvbind (doc-string slots-spec) (accept-documentable-block slots-spec)
    (mvbind (pkg base-name internal-type?) (parse-structure-name structure-spec)
      (let* ((prefix (if internal-type? "%" ""))
             (slots-meta (map #L(parse-structure-slot _ prefix base-name) slots-spec)))
        (define (slot-procedures)
          (append-map (lambda (slot-defn)
                        `(,@(aif (cdr (assoc :set slot-defn))
                                 `((:set ,(intern! it pkg) ,(cdr (assoc :name slot-defn))))
                                 ())
                          ,@(aif (cdr (assoc :get slot-defn))
                                 `((:get ,(intern! it pkg) ,(cdr (assoc :name slot-defn))))
                                 ())))
                      slots-meta))
        (define (slot-defaults)
          (map #L(cons (car _) (cdr (assoc :default _))) slots-meta))
        (let ((type-name (intern! base-name pkg) )
              (constructor-name (intern! #"${prefix}make-${base-name}" pkg)))
          (values type-name
                  (list (make-structure-layout type-name slots-meta)
                        (cons :constructor-name constructor-name)
                        (cons :documentation (or doc-string ""))
                        (cons :slots slots-meta))
                  (cons* (list :constructor constructor-name (slot-defaults))
                         (list :copier      (intern! #"${prefix}copy-${base-name}" pkg))
                         (list :predicate   (intern! #"${prefix}${base-name}?" pkg))
                         (slot-procedures))))))))

(define *global-structure-dictionary* {})

(forward structure-meta)

(define (%structure-meta structure)
  "Returns the metadata associated with <structure>, which can be
either a structure or a structure type name. Throws an error if
<structure> is itself an invalid type name or an orphaned
structure. If metadata is not available, returns #f."
  (cond ((structure? structure)
         (and (not (orphaned-structure? structure))
              (%structure-meta (structure-type structure))))
        ((symbol? structure)
         (get-property structure 'structure-meta))
        (#t
         (error "Invalid structure or structure type, cannot retrieve metadata: ~s " structure))))

(define (structure-meta structure)
  "Returns the metadata associated with <structure>, which can be
either a structure or a structure type name. Throws an error if
<structure> is itself an invalid type name or structure metadata is
not available. Because full metadata is not available for orphaned
structures, this throws an error if applied to an orphaned structure."
  (or (%structure-meta structure)
      (error "No metadata available for structure of type: ~s" structure)))

(define (structure-layout structure)
  "Returns the metadata associated with <structure>, which can be
either a structure or a structure type name. Throws an error if
<structure> is itself an invalid type name or structure metadata is
not available. Because structures carry layout information directly,
this can be called on an orphaned structure."
  (cond ((%structure? structure)
         (%structure-layout structure))
         ((symbol? structure)
          (car (structure-meta structure)))
         (#t
          (error "Expected structure or structure type name: ~s" structure))))

(define (structure-documentation structure)
  "Returns the documentation associated with <structure>, which can be either a
   structure or a structure type name. Returns #f if <structure> is orphaned
   or an unknown type. Throws an error if <structure> is itself an invalid
   type name."
  (runtime-check (or structure? symbol?) structure
         "Expected structure or structure type name.")
  (let ((meta (structure-meta structure)))
    (aand (and meta (assoc :documentation meta))
          (cdr it))))

(define (invalidate-existing-structure-type! structure-type-name)
  (awhen (get-property structure-type-name 'structure-meta)
    (info "New structure definition orphaning existing structures of type: ~s" structure-type-name)
    (mark-structure-layout-orphaned! (structure-meta-layout it))
    (remove-property! structure-type-name 'structure-meta)))

(define (%register-structure-type! structure-type-name meta)
  "Registers a new structure type, <name>, with metadata <meta>. This includes
   de-registering any older type of the same name, and orphaning any existing
   objects of that type."
  (invalidate-existing-structure-type! structure-type-name)
  ;; Let the type system know about our new type
  (make-class< structure-type-name 'structure)
  ;; Register the type itself.
  (set-property! structure-type-name 'structure-meta meta)
  (hash-set! *global-structure-dictionary* structure-type-name structure-type-name))

(define (trap-resolve-fasl-struct-layout trapno frp new-layout)
  (let* ((existing-meta (%structure-meta (structure-layout-name new-layout)))
         (old-layout (if existing-meta (structure-meta-layout existing-meta) ())))
    (if (equal? new-layout old-layout)
        old-layout
        (mark-structure-layout-orphaned! new-layout))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_RESOLVE_FASL_STRUCT_LAYOUT trap-resolve-fasl-struct-layout))

;; REVISIT: Do we need a way to 'forget' structure types?

(define (structure? structure)
  "Returns <structure> if it is a structure, returns #f otherwise."
  (and (%structure? structure)
       structure))

(define (structure-type structure)
  "Returns the type name of the structure <structure>. Note that this is usually
   a symbol, but for an instance of a orphaned structure type, the type name is
   returned as a two element list composed of :orphaned and the former type name."
  (runtime-check %structure? structure)
  (first (structure-layout structure)))

;;; TEST: unit tests for orphaned structures

(define (orphaned-structure? structure)
  "Returns <structure> if it is an instance of a orphaned structure type, returns #f
   otherwise."
  (and (structure? structure)
       (list? (structure-type structure))
       (eq? (car (structure-type structure)) :orphaned)
       structure))

;; REVISIT: structure inheritance

(define (structure-slots structure)
  "Returns a list of the slots in <structure>. <structure> can either
be a structure (including orphaned structures) or a symbol naming a
structure type. Throws an error is <structure> is neither a valid
structure nor a type name."
  (structure-layout-slot-names (structure-layout structure)))

(define (structure-has-slot? structure slot-name)
  "Returns <slot-name> if it is the name of a valid slot in <structure>,
   returns #f otherwise. <structure> can either be a structure object or
   a symbol naming a structure type. If a structure type is not found,
   throws an error."
  (runtime-check keyword? slot-name)
  (and (memq slot-name (structure-slots structure))
       slot-name))

(define (copy-structure s)
  "Returns a duplicate copy of structure <s>, performing a slot-by-slot shallow
   copy."
  (%copy-structure s))

(define (all-structure-types)
  "Returns a list of all structure type names."
  (hash-keys *global-structure-dictionary*))

(define (make-structure-by-name type-name . args)
  (aif (assoc :constructor-name (structure-meta type-name))
       (apply (symbol-value (cdr it)) args)
       (error "Structure type not found: ~s" type-name)))

(define (%structure-slot-index structure slot-name)
  (or (structure-layout-slot-offset (structure-layout structure) slot-name)
      (error "Slot ~s not found in structure ~s." slot-name structure)))

(define (structure-slot-by-name structure slot-name)
  "Retrieves the value of the slot named <slot-name> in <structure>."
  (%structure-ref structure (%structure-slot-index structure slot-name)))

(define (set-structure-slot-by-name! structure slot-name new-value)
  "Updates the slot named <slot-name> in <structure> to have the value <new-value>."
  (%structure-set! structure
                   (%structure-slot-index structure slot-name)
                   new-value))

(define (%require-structure-type s expected-layout)
  (unless (%structure? s expected-layout)
    (error "Expected a structure of type ~s, but found ~s." (structure-layout-name expected-layout) s)))

(defmacro (define-structure name . slots)
  (mvbind (name meta procs) (parse-structure-definition name slots)
    (let ((layout (car meta)))
      (define (structure-docs)
        (cdr (assoc :documentation meta)))
      (define (slot-docs slot-name)
        (let* ((slots (cdr (assoc :slots meta)))
               (slot (cdr (assoc slot-name slots))))
          (aif (assoc :documentation slot)
               #"Slot documentation: ${(cdr it)}"
               "")))
      (define (constructor-form proc-name defaults)
        (let ((defaults (map #L(list (car _) (gensym _) (cdr _)) defaults)))
          (with-gensyms (initial-values-sym structure-sym)
            `(define (,proc-name :keyword ,@defaults)
               ,#"Constructs a new instance of structure type ${name}. ${(structure-docs)}"
               (%structurecons (vector ,@(map cadr defaults)) ',layout)))))
      (define (copier-form proc-name)
        `(define (,proc-name s)
           ,#"Copies an instance <s> of structure type ${name}, throwing an error
              if <s> is not the expected type."
           (%require-structure-type s ',layout)
           (%copy-structure s)))
      (define (predicate-form proc-name)
        `(define (,proc-name s)
           ,#"Returns <s> if it is an instance of structure type ${name},
              #f otherwise."
           (and (%structure? s ',layout)
                s)))
      (define (getter-form proc-name slot-name)
        `(define (,proc-name s)
           ,#"Retrieves the value of the ${slot-name} slot of <s>, which must
              be of structure type ${name}. Throws an error if <s> is not of the
             expected type. ${(slot-docs slot-name)}"
           (%require-structure-type s ',layout)           
           (%structure-ref s ,(structure-layout-slot-offset layout slot-name))))
      (define (setter-form proc-name slot-name)
        `(define (,proc-name s v)
           ,#"Updates the ${slot-name} slot of of <s> to <v>. <s> must be of structure
            type ${name}, an error is thrown otherwise. ${(slot-docs slot-name)}"
           (%require-structure-type s ',layout)                      
           (%structure-set! s ,(structure-layout-slot-offset layout slot-name) v)))

      (awhen (duplicates? (structure-layout-slot-names layout))
        (error "Duplicate slots ~s in definition of structure type: ~s." it name))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,@(map (lambda (proc)
                  (apply (case (car proc)
                           ((:constructor) constructor-form)
                           ((:copier)      copier-form)
                           ((:predicate)   predicate-form)
                           ((:get)         getter-form)
                           ((:set)         setter-form)
                           (#t (error "Invalid structure procedure type, ~s" (car proc))))
                         (cdr proc)))
                procs)
         (%register-structure-type! ',name ',meta)
         ',name))))

;;;; Keyed data type support

(define (slot-ref object key :optional (default-value #f))
  "Retrieves the value of the slot in <object> named by <key>. <object> can be
   either a hash, an instance, or a structure. Attempts to retrieve unknown
   slots from a structure will return <default-value>."
  (cond ((structure? object)
         (if (structure-has-slot? object key)
             (structure-slot-by-name object key default-value)
             default-value))
        ((hash? object)
         (hash-ref object key default-value))
        (#t
         (error "Invalid value for slot-ref: ~s"))))

(define (slot-set! object key value)
  "Updates the value of the slot in <object> named by <key> to <value>. <object>
   can be either a hash, an instance, or a structure. In the case of a structure,
   attempts to set a non-existant <key> will throw an error."
  (cond ((structure? object)
          (set-structure-slot-by-name! object key value))
        ((hash? object)
         (hash-set! object key value))
        (#t
         (error "Invalid value for slot-set!: ~s"))))

