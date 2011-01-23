;;;; instance.scm
;;;;
;;;; Instances.

(define (message-not-understood-handler trapno instance message-name)
  (error "Message ~s not understood by ~s" message-name instance))

(define (primitive-instance-handler trapno primitive)
  (error "Cannot send message to primitive: ~s" primitive))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_MSG_NOT_UNDERSTOOD message-not-understood-handler)
  (%set-trap-handler! system::TRAP_PRIMITIVE_INSTANCE primitive-instance-handler))

(define (is-a? object type)
  "Determines if <object> is of the type specified by <type>."
  (cond ((symbol? type) (eq? (type-of object) type))
        (#t (error "Wrong type for type specifier: ~s" type))))

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

(defmacro (mlambda args . body)
  "Syntax for a lambda suitable for use as an instance message handler."
  `(lambda (self . ,args) ,@body))

(defmacro (defmesg proto lambda-list . code)
  (unless (and (pair? lambda-list)
               (symbol? (car lambda-list)))
    (error "defmesg: invalid lambda-list: ~s" lambda-list))
  `(slot-set! ,proto ',(car lambda-list)
              (mlambda ,(cdr lambda-list) ,@code)))

(define (instance-proto instance)
  "Returns the prototype instance of <instance>, #f if none. Throws an error
   if the prototype is invalid."
  (check instance? instance)
  (let* ((proto-slot-value (%instance-proto instance))
         (prototype (cond ((symbol? proto-slot-value)
                           (symbol-value proto-slot-value))
                          (#t
                           proto-slot-value))))
    (unless (or (instance? prototype) (eq? prototype #f))
      (error "Invalid prototype ~s for instance ~s." prototype instance))
    prototype))

(define (direct-instance-slots instance)
  "Returns a list of all slots directly defined in <instance>."
  ;; This function makes the guarantee that instance slots are ordered
  ;; in the list as they are ordered in the in-memory layout of the
  ;; object. (This is also the same as the order in which they
  ;; were ordered to the object). This guarantee is explicitly
  ;; unmade to consumers of this function outside of the languauge
  ;; itself: it's way too implementation-specific.
  (let ((map-table (%instance-map instance)))
    (qsort (hash-keys map-table)
           <
           #L(hash-ref map-table _))))


(define (all-instance-slots instance)
  "Returns a list of all the slots available to <instance>, regardless of
   whether or not they are directly available or available through inheritance.."
  (set-union/eq (aif (instance-proto instance) (direct-instance-slots it) ())
                (direct-instance-slots instance)))

;;; TODO: Add separate slot not found handlers for slot-ref and slot-set!.

(define (flatten-instance instance)
  "Returns a copy of <instance> with a flattened prototype chain. The returned
   instance has no protoype and has a local binding for each slot visible in
   <instance>."
 (let ((new-instance (make-instance)))
   (dolist (slot (all-instance-slots instance))
     (slot-set! new-instance slot (slot-ref instance slot)))
   new-instance))

(define (slots-ref instance slots :optional (default-value ())) ; REVISIT: some slot defaults are (), some are #f.
  "Returns a list of the values of the specified <slots> of
   <instance>. If a slot is not found, it is returned as <default-value>."
 (map #L(slot-ref instance _ default-value) slots))

(define (columns->instance columns :optional (default-value #f)) ; REVISIT: specify base columns
  "Given a list of column names, <columns>, return an instance with the slots
   listed in <slots>, each bound to <default-value>."
 (let ((instance (make-instance)))
   (dolist (column columns)
     (slot-set! instance column default-value))
   instance))

(define (data->instance columns data-values :optional (base-instance #f))
  "Given a list of column names, <columns>, return an instance with the slots
   listed in <columns>, each bound to the corresponding value in <data-values>."
 (let ((instance (make-instance base-instance)))
   (iterate/r ((list column columns)
               (list data-value data-values)) ()
              (slot-set! instance column data-value)
              instance)))

(define (table->instances table)
  "Given a data table (a list of lists, where the first list is a list of field names
   for each subsequent list), return a list of instances with a slot for each field."
 (let* ((header (map intern-keyword! (first table)))
        (base (columns->instance header)))
   (map #L(data->instance header _ base) (rest table))))

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
               (> arity 0)))))

