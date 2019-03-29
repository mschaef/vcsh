
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
      (a-list->hash
       (p-list-fold (lambda (attr val rest)
                      (alist-cons attr (validate-attr-value attr val) rest))
                    (alist :slot-name slot-name
                           :default ()
                           :get #"${prefix}${base-name}-${slot-name}"
                           :set #"${prefix}set-${base-name}-${slot-name}!")
                    slot-spec)))))

(define (slot-meta-field field slot-meta)
  (hash-ref slot-meta field))

(define (assign-structure-slot-offsets type-name slots)
  (let ((indices {}))
    (let recur ((index 0) (slots slots))
      (let ((slot (car slots)))
        (cond ((null? slots)
               indices)
              ((hash-has? indices (slot-meta-field :slot-name slot))
               (error "Duplicate slots ~s in definition of structure type: ~s." (slot-meta-field :slot-name slot)
                      type-name))
              (#t
               (hash-set! indices (slot-meta-field :slot-name slot) index)
               (recur (+ index 1) (cdr slots))))))))

(define (make-structure-layout type-name slots-meta)
  (cons type-name
        (assign-structure-slot-offsets type-name slots-meta)))

(define (structure-layout-name layout)
  (car layout))

(define (structure-layout-slot-names layout)
  (hash-keys (cdr layout)))

(define (structure-layout-slot-offset layout slot-name)
  (hash-ref (cdr layout) slot-name))

(define (mark-structure-layout-orphaned! layout)
  (set-car! layout (list :orphaned (car layout))))

(define (structure-layout-orphaned? layout)
  (let ((name (structure-layout-name layout)))
    (and (pair? name)
         (eq? (car name) :orphaned))))

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
                        `(,@(aif (slot-meta-field :set slot-defn)
                                 `((:set ,(intern! it pkg) ,(slot-meta-field :slot-name slot-defn))))
                          ,@(aif (slot-meta-field :get slot-defn)
                                 `((:get ,(intern! it pkg) ,(slot-meta-field :slot-name slot-defn))))))
                      slots-meta))
        (define (slot-defaults)
          (map #L(cons (slot-meta-field :slot-name _) (slot-meta-field :default _)) slots-meta))
        (let ((type-name (intern! base-name pkg) )
              (constructor-name (intern! #"${prefix}make-${base-name}" pkg)))
          (let ((layout (make-structure-layout type-name slots-meta)))
            (values {:type-name type-name
                     :layout layout
                     :constructor-name constructor-name
                     :documentation (or doc-string "")
                     :slots slots-meta}
                    (cons* (list :constructor constructor-name (slot-defaults))
                           (list :copier      (intern! #"${prefix}copy-${base-name}" pkg))
                           (list :predicate   (intern! #"${prefix}${base-name}?" pkg))
                           (slot-procedures)))))))))

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
          (:layout (structure-meta structure)))
         (#t
          (error "Expected structure or structure type name: ~s" structure))))

(define (structure-documentation structure)
  "Returns the documentation associated with <structure>, which can be either a
   structure or a structure type name. Returns #f if <structure> is orphaned
   or an unknown type. Throws an error if <structure> is itself an invalid
   type name."
  (runtime-check (or structure? symbol?) structure
         "Expected structure or structure type name.")
  (aand (structure-meta structure)
        (:documentation it)))

(define (invalidate-existing-structure-type! structure-type-name)
  (awhen (get-property structure-type-name 'structure-meta)
    (info "New structure definition orphaning existing structures of type: ~s" structure-type-name)
    (mark-structure-layout-orphaned! (:layout it))
    (remove-property! structure-type-name 'structure-meta)))

(define (%register-structure-type! structure-type-name meta)
  "Registers a new structure type, <name>, with metadata <meta>. This includes
   de-registering any older type of the same name, and orphaning any existing
   objects of that type."
  (invalidate-existing-structure-type! structure-type-name)
  (make-class< structure-type-name 'structure)
  (set-property! structure-type-name 'structure-meta meta))

(define (trap-resolve-fasl-struct-layout trapno frp new-layout)
  (let* ((existing-meta (%structure-meta (structure-layout-name new-layout)))
         (old-layout (if existing-meta (:layout existing-meta) ())))
    (if (equal? new-layout old-layout)
        old-layout
        (mark-structure-layout-orphaned! new-layout))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_RESOLVE_FASL_STRUCT_LAYOUT trap-resolve-fasl-struct-layout))

(define (structure? structure)
  "Returns <structure> if it is a structure, returns #f otherwise."
  (and (%structure? structure)
       structure))

(define (structure-type structure)
  "Returns the type name of the structure <structure>. Note that this is usually
   a symbol, but for an instance of a orphaned structure type, the type name is
   returned as a two element list composed of :orphaned and the former type name."
  (runtime-check %structure? structure)
  (structure-layout-name (structure-layout structure)))

(define (orphaned-structure? structure)
  "Returns <structure> if it is an instance of a orphaned structure type, returns #f
   otherwise."
  (and (structure? structure)
       (structure-layout-orphaned? (structure-layout structure))
       structure))

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
  (and (structure-layout-slot-offset (structure-layout structure) slot-name)
       slot-name))

(define (copy-structure s)
  "Returns a duplicate copy of structure <s>, performing a slot-by-slot shallow
   copy."
  (%copy-structure s))

(define (make-structure-by-name type-name . args)
  (aif (:constructor-name (structure-meta type-name))
       (apply (symbol-value it) args)
       (error "Structure type not found: ~s" type-name)))

(defmacro (define-structure name . slots)
  (mvbind (meta procs) (parse-structure-definition name slots)
    (let ((name (:type-name meta))
          (layout (:layout meta)))
      (define (slot-docs slot-name)
        (aif (:documentation (find #L(eq? slot-name (:slot-name _)) (:slots meta)))
             #"Slot documentation: ${it}"
             ""))
      (define (constructor-form proc-name defaults)
        (let ((defaults (map #L(list (car _) (gensym _) (cdr _)) defaults)))
          `(define (,proc-name :keyword ,@defaults)
             ,#"Constructs a new instance of structure type ${name}. ${(:documentation meta)}"
             (%structurecons (vector ,@(map cadr defaults)) ',layout))))
      (define (copier-form proc-name)
        `(define (,proc-name s)
           ,#"Copies an instance <s> of structure type ${name}, throwing an error
              if <s> is not the expected type."
           (%copy-structure s ',layout)))
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
           (%structure-ref s ,(structure-layout-slot-offset layout slot-name) ',layout)))
      (define (setter-form proc-name slot-name)
        `(define (,proc-name s v)
           ,#"Updates the ${slot-name} slot of of <s> to <v>. <s> must be of structure
            type ${name}, an error is thrown otherwise. ${(slot-docs slot-name)}"
           (%structure-set! s ,(structure-layout-slot-offset layout slot-name) v ',layout)))

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

