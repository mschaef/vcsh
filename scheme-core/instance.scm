
;;;; instance.scm --
;;;;
;;;; The implementation of prototype based object instances.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (message-not-understood-handler trapno instance message-name)
  (error "Message ~s not understood by ~s" message-name instance))

(define (primitive-instance-handler trapno primitive)
  (error "Cannot send message to primitive: ~s" primitive))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_MSG_NOT_UNDERSTOOD message-not-understood-handler)
  (%set-trap-handler! system::TRAP_PRIMITIVE_INSTANCE primitive-instance-handler))

(define (has-slot? instance slot-name)
  (if (%find-slot-instance instance slot-name)
      instance
      #f))

(define (slot-ref instance key)
  "Retrieves the value of the slot in <instance> named by <key>."
  (aif (%find-slot-instance instance key)
       (%slot-ref it key)
       (error "Slot ~s not found in instance ~s" instance key)))

(define (slot-set! instance key value)
  "Updates the value of the slot in <instance> named by <key> to <value>."
  (%slot-set! instance key value))

(define (is-a? object type)
  "Determines if <object> is of the type specified by <type>."
  (cond ((symbol? type) (eq? (type-of object) type))
        (#t (error "Wrong type for type specifier: ~s" type))))

(define (make-instance proto . slots)
  (p-list-fold (lambda (slot-name slot-value instance)
                 (slot-set! instance slot-name slot-value))
               (%instancecons proto)
               slots))

(defmacro (define-proto proto-name . init-values)
  "Defines an object prototype. proto-name can be either a symbol naming the
   prototype or a list containing the prototype name and the base prototype.
   This macro expands to code the defines the prototype as well as a predicate
   identifying objects with the same class-name as the prototype"
  (define (parse-proto-name)
    (cond ((symbol? proto-name)
           (values proto-name #f))
          ((and (list? proto-name) (length=2? proto-name))
           (values (first proto-name) (second proto-name)))
          (#t
           (error "Invalid proto name specification, must be either <name> or ( <name> <base> ): ~s" proto-name))))
  (mvbind (name base) (parse-proto-name)
    (unless (symbol? name)
      (error "Proto names must be symbols: ~s" name))
    (unless (or (null? name) (symbol? name))
      (error "base proto names must be symbols: ~s" base))
    `(define ,name (make-instance ',base
                                  'class-name ',name
                                  ,@init-values))))

(defmacro (message-lambda args . body)
  "Syntax for a lambda suitable for use as an instance message handler."
  `(lambda (self . ,args) ,@body))

(defmacro (define-message proto lambda-list . code)
  (unless (and (pair? lambda-list)
               (symbol? (car lambda-list)))
    (error "define-message: invalid lambda-list: ~s" lambda-list))
  `(slot-set! ,proto ',(car lambda-list)
              (message-lambda ,(cdr lambda-list) ,@code)))

(define (instance-proto instance)
  "Returns the prototype instance of <instance>, #f if none."
  (check instance? instance)
  (let* ((proto-slot-value (%instance-proto instance))
         (prototype (cond ((symbol? proto-slot-value)
                           (symbol-value proto-slot-value))
                          (#t
                           proto-slot-value))))
    (and (not (null? prototype))
         prototype)))

(define (direct-instance-slots instance)
  "Returns a list of all slots directly defined in <instance>."
  (hash-keys (%instance-slots instance)))

(define (all-instance-slots instance)
  "Returns a list of all the slots available to <instance>, regardless of
   whether or not they are directly available or available through inheritance.."
  (set-union/eq (aif (instance-proto instance) (direct-instance-slots it) ())
                (direct-instance-slots instance)))

;;; REVISIT: Add separate slot not found handlers for slot-ref and slot-set!.

(define (flatten-instance instance)
  "Returns a copy of <instance> with a flattened prototype chain. The returned
   instance has no protoype and has a local binding for each slot visible in
   <instance>."
 (let ((new-instance (make-instance)))
   (dolist (slot (all-instance-slots instance))
     (slot-set! new-instance slot (slot-ref instance slot)))
   new-instance))

(define (slots-ref instance slots :optional (default-value #f))
  "Returns a list of the values of the specified <slots> of
   <instance>. If a slot is not found, it is returned as <default-value>."
 (map #L(slot-ref instance _ default-value) slots))

(define (instance-with-slot? obj slot-name)
  "Determines if <obj> is an instance that contains a slot named <slot-name>. If it is,
   return a symbol indicating the type of slot, which will be either :local or :inherited.
   If not, returns #f."
  (and (instance? obj)
       (has-slot? obj slot-name)))

(define (instance-understands? obj message-name)
  "Returns #t if <obj> is an instance that understands the <message-name> message,
   returns #f otherwise. An instance is deemed to understand a message if it contains
   a slot of that name and that slot is bound to a procedure of at least arity 1 (for
   the self argument)."
  (and (instance-with-slot? obj message-name)
       (aand (slot-ref obj message-name)
             (procedure? it)
             (mvbind (arity rest?) (procedure-arity it)
               (or (> arity 0)
                   rest?)))))
