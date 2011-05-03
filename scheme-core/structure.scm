
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
  (check (and symbol? symbol-package (not keyword?)) structure-name
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
  (let ((slot-spec (->list slot-spec)))
    (let ((name (intern-keyword! (symbol-name (car slot-spec)))))
      (define (validate-attr-value attr val)
        (define (bad-value)
          (error "Invalid structure slot specification: ~s, bad ~s, ~s"
                 slot-spec attr val))
        (case attr
          ((:documentation) (unless (string? val) (bad-value)))
          ((:get :set)      (unless (or (eq? val #f) (string? val)) (bad-value)))
          ((:default)
           () ; REVISIT: fix when the  body-less case clause error is fixed in the compiler
           ;; anything is permissable here
           )
          (#t
           (error "Invalid structure slot specification: ~s, bad attribute: ~s"
                  slot-spec attr))))

      (let ((doc-string (aif (string? (cadr slot-spec)) it #f))
            (slot-spec (if (string? (cadr slot-spec))
                           (cddr slot-spec) (cdr slot-spec)))
            (base-alist (alist :name name
                               :default ()
                               :get #"${prefix}${base-name}-${name}"
                               :set #"${prefix}set-${base-name}-${name}!")))
        (cons name
              (minimal-alist
               (p-list-fold (lambda (attr val rest)
                              (validate-attr-value attr val)
                              (alist-cons attr val rest))
                            (if doc-string
                                (alist-cons :documentation doc-string base-alist)
                                base-alist)
                            slot-spec)))))))


(define (parse-structure-definition structure-spec slots-spec)
  "Parses a structure definition composed of two parts, a
   <structure-spec> and a <slots-spet>, returning three values:
   the structure name, the structure metadata, and a list of
   procedures the structure definition s requested be created."
  (let ((doc-string (aif (string? (car slots-spec)) it #f))
        (slots-spec (if (string? (car slots-spec)) (cdr slots-spec) slots-spec)))
    (mvbind (pkg base-name internal-type?)
        (parse-structure-name (if (pair? structure-spec)
                                  (car structure-spec)
                                  structure-spec))
      (let* ((prefix (if internal-type? "%" ""))
             (slots-meta (map #L(parse-structure-slot _ prefix base-name) slots-spec))
             (name (intern! base-name pkg))
             (layout (list name (slots->layout slots-meta))))
        (define (slot-procedures)
          (append-map (lambda (slot-defn)
                        `(,@(aif (cdr (assq :set slot-defn))
                                 `((:set ,(intern! it pkg) ,(cdr (assq :name slot-defn))))
                                 ())
                          ,@(aif (cdr (assq :get slot-defn))
                                 `((:get ,(intern! it pkg) ,(cdr (assq :name slot-defn))))
                                 ())))
                      slots-meta))
        (define (slot-defaults)
          (map #L(cons (car _) (cdr (assq :default _))) slots-meta))
        (values name
                (if doc-string
                    (list layout (cons :documentation doc-string)
                          (cons :slots slots-meta))
                    (list layout (cons :slots slots-meta)))
                (cons* (list :constructor (intern! #"${prefix}make-${base-name}" pkg)
                             (slot-defaults))
                       (list :copier      (intern! #"${prefix}copy-${base-name}" pkg))
                       (list :predicate   (intern! #"${prefix}${base-name}?" pkg))
                       (slot-procedures)))))))


(define *global-structure-dictionary* ())

(define (%structure-meta structure)
  "Returns the metadata associated with <structure>, which can be either a
   structure or a structure type name. Returns #f if <structure> is orphaned
   or an unknown type. Throws an error if <structure> is itself an invalid
   type name."
  (check (or structure? symbol?) structure
         "Expected structure or structure type name.")
  (if (structure? structure)
      (if (orphaned-structure? structure)
          #f
          (%structure-meta (structure-type structure)))
      (get-property structure 'structure-meta)))

(define (structure-documentation structure)
  "Returns the documentation associated with <structure>, which can be either a
   structure or a structure type name. Returns #f if <structure> is orphaned
   or an unknown type. Throws an error if <structure> is itself an invalid
   type name."
  (check (or structure? symbol?) structure
         "Expected structure or structure type name.")
  (let ((meta (%structure-meta structure)))
    (aif (and meta (assq :documentation meta))
         (cdr it)
         #f)))


(define (%register-structure-type! structure-type-name meta)
  "Registers a new structure type, <name>, with metadata <meta>. This includes
   de-registering any older type of the same name, and orphaning any existing
   objects of that type."
  (awhen (get-property structure-type-name 'structure-meta)
    (info "New structure definition orphaning existing structures of type: ~s" structure-type-name)
    (let ((existing-layout (car it)))
      (set-car! existing-layout (list :orphaned (car existing-layout))))
    (remove-property! structure-type-name 'structure-meta))
  ;; Let the type system know about our new type ; REVISIT: Disabled, because structure.scm was moved ahead of the type graph code in the bootup sequence
  (make-class< structure-type-name 'structure)
  ;; Register the type itself.
  (set-property! structure-type-name 'structure-meta meta)
  (unless (member structure-type-name *global-structure-dictionary*)
    (push! structure-type-name *global-structure-dictionary*)))

(define (trap-resolve-fasl-struct-layout trapno frp new-layout)
  (unless (pair? new-layout)
    (error "Expected list for structure layout ~s" new-layout))
  (let* ((structure-type-name (car new-layout))
         (existing-meta (assq 'scheme::structure-meta (%property-list structure-type-name)))
         (old-layout (if existing-meta (cadr existing-meta) ()))
         (obsolete? (not (equal? new-layout old-layout))))
    (if obsolete?
        (set-car! new-layout (list :orphaned (car new-layout)))
        old-layout)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_RESOLVE_FASL_STRUCT_LAYOUT trap-resolve-fasl-struct-layout))

;; REVISIT: Do we need a way to 'forget' structure types?

(define (slots->layout slots)
  (let recur ((index 0) ;; REVISIT: implement iterate from Scheme48
              (slots slots))
    (if (null? slots)
        ()
        (cons (list (caar slots) index)
              (recur (+ index 1) (cdr slots))))))

(define (structure? structure)
  "Returns <structure> if it is a structure, returns #f otherwise."
  (if (%structure? structure)
      structure
      #f))

(define (structure-type structure)
  "Returns the type name of the structure <structure>. Note that this is usually
   a symbol, but for an instance of a orphaned structure type, the type name is
   returned as a two element list composed of :orphaned and the former type name."
  (check %structure? structure)
  (first (%structure-layout structure)))

;;; TEST: unit tests for orphaned structures

(define (orphaned-structure? structure)
  "Returns <structure> if it is an instance of a orphaned structure type, returns #f
   otherwise."
  (if (and (structure? structure)
           (list? (structure-type structure))
           (eq? (car (structure-type structure)) :orphaned))
      structure
      #f))

;; REVISIT: structure inheritance

(define (structure-slots structure)
  "Returns a list of the slots in <structure>. <structure> can either
   be a structure object or a symbol naming a structure type. If a
   structure type is not found, returns #f."
  (map car (cond ((%structure? structure)
                  (second (%structure-layout structure)))
                 ((symbol? structure)
                  (aif (get-property structure 'structure-meta)
                       (second (car it))
                       #f))
                 (#t
                  (error "Expected structure or structure type name: ~s" structure)))))

(define (structure-has-slot? structure slot-name)
  "Returns <slot-name> if it is the name of a valid slot in <structure>,
   returns #f otherwise. <structure> can either be a structure object or
   a symbol naming a structure type. If a structure type is not found,
   throws an error."
  (check keyword? slot-name)
  (aif (memq slot-name (aif (structure-slots structure)
                            it
                            (error "Unknown structure type: ~s" structure)))
       slot-name
       #f))

(define (initialize-structure structure slot-name/values)
  "Initializes <structure> by setting the slots in <slot-name/values>
   to the listed values. <slot-name/values> is a list in Common Lisp
   property list syntax."
  (p-list-fold (lambda (slot-name value structure)
                 (set-structure-slot-by-name! structure slot-name value))
               structure
               slot-name/values))

(define (copy-structure s)
  "Returns a duplicate copy of structure <s>, performing a slot-by-slot shallow
   copy."
  (%copy-structure s))

(define (all-structure-types)
  "Returns a list of all structure type names."
  *global-structure-dictionary*)

(define (%structure-slot-index structure slot-name)
  (aif (member-index slot-name (structure-slots structure))
       it
       (error "Slot ~s not found in structure ~s." slot-name structure)))

(define (make-structure-by-name type-name . args)
  (aif (get-property type-name 'structure-constructor)
       (apply it args)
       (error "Structure type not found: ~s" type-name)))

(define (structure-slot-by-name structure slot-name)
  "Retrieves the value of the slot named <slot-name> in <structure>."
  (%structure-ref structure (%structure-slot-index structure slot-name)))

(define (set-structure-slot-by-name! structure slot-name new-value)
  "Updates the slot named <slot-name> in <structure> to have the value <new-value>."
  (%structure-set! structure
                   (%structure-slot-index structure slot-name)
                   new-value))

(define (slot-ref object key :optional (default-value ()))
  "Retrieves the value of the slot in <object> named by <key>. <object> can be
   either a hash, an instance, or a structure. Attempts to retrieve unknown
   slots from a structure will return <default-value>."
  (if (structure? object)
      (if (structure-has-slot? object key)
          (structure-slot-by-name object key default-value)
          default-value)
      (%slot-ref object key default-value)))

(define (slot-set! object key value)
  "Updates the value of the slot in <object> named by <key> to <value>. <object>
   can be either a hash, an instance, or a structure. In the case of a structure,
   attempts to set a non-existant <key> will throw an error."
  (if (structure? object)
      (set-structure-slot-by-name! object key value)
      (%slot-set! object key value)))

(defmacro (define-structure name . slots)
  (mvbind (name meta procs) (parse-structure-definition name slots)
    (let ((layout (car meta)))
      (define (structure-docs)
        (aif (assoc :documentation meta)
             #"Type documentation: ${(cdr it)}"
             ""))
      (define (slot-docs slot-name)
        (let* ((slots (cdr (assoc :slots meta)))
               (slot (cdr (assoc slot-name slots))))
          (aif (assoc :documentation slot)
               #"Slot documentation: ${(cdr it)}"
               "")))
      (define (constructor-form proc-name defaults)
        (let ((defaults (map #L(list (car _) (gensym _) (cdr _)) defaults)))
          (with-gensyms (initial-values-sym structure-sym)
              `(begin
                 (define (,proc-name :keyword ,@defaults)
                   ,#"Constructs a new instance of structure type ${name}. ${(structure-docs)}"
                   (%structurecons (vector ,@(map cadr defaults)) ',layout))
                 (set-property! ',name 'structure-constructor ,proc-name)))))
      (define (copier-form proc-name)
        `(define (,proc-name s)
           ,#"Copies an instance <s> of structure type ${name}, throwing an error
              if <s> is not the expected type."
           (unless (%structure? s ',layout)
             (error "Expected a structure of type ~s, but found ~s." ',(car layout) s))
           (%copy-structure s)))
      (define (predicate-form proc-name)
        `(define (,proc-name s)
           ,#"Returns <s> if it is an instance of structure type ${name},
              #f otherwise."
           (if (%structure? s ',layout)
               s
               #f)))
      (define (getter-form proc-name slot-name)
        `(define (,proc-name s)
           ,#"Retrieves the value of the ${slot-name} slot of <s>, which must
              be of structure type ${name}. Throws an error if <s> is not of the
             expected type. ${(slot-docs slot-name)}"
           (unless (%structure? s ',layout)
             (error "Expected a structure of type ~s, but found ~s." ',(car layout) s))
           (%structure-ref s ,(second (assq slot-name (cadr layout))))))
      (define (setter-form proc-name slot-name)
        `(define (,proc-name s v)
           ,#"Updates the ${slot-name} slot of of <s> to <v>. <s> must be of structure
            type ${name}, an error is thrown otherwise. ${(slot-docs slot-name)}"
           (unless (%structure? s ',layout)
             (error "Expected a  structure of type ~s, but found ~s." ',(car layout) s))
           (%structure-set! s ,(second (assq slot-name (cadr layout))) v)))

      (awhen (duplicates? (map car (cadr layout)))
        (error "Duplicate slots ~s in definition of structure type: ~s."
               it name))
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

