;;;; class-graph.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; The class inheritance graph.

(define (typecode->name tc)
  (case tc
    ((#.system::TC_FREE_CELL)      'free-cell)  
    ((#.system::TC_NIL)            'nil)  
    ((#.system::TC_BOOLEAN)        'boolean)  
    ((#.system::TC_CONS)           'cons)  
    ((#.system::TC_FIXNUM)         'fixnum)  
    ((#.system::TC_FLONUM)         'flonum)  
    ((#.system::TC_CHARACTER)      'character)  
    ((#.system::TC_SYMBOL)         'symbol)  
    ((#.system::TC_PACKAGE)        'package)  
    ((#.system::TC_SUBR)           'subr)  
    ((#.system::TC_CLOSURE)        'closure) 
    ((#.system::TC_MACRO)          'macro) 
    ((#.system::TC_STRING)         'string) 
    ((#.system::TC_VECTOR)         'vector) 
    ((#.system::TC_STRUCTURE)      'structure) 
    ((#.system::TC_HASH)           'hash) 
    ((#.system::TC_PORT)           'port) 
    ((#.system::TC_END_OF_FILE)    'end-of-file) 
    ((#.system::TC_VALUES_TUPLE)   'values-typle) 
    ((#.system::TC_INSTANCE)       'instance) 
    ((#.system::TC_UNBOUND_MARKER) 'unbound-marker) 
    ((#.system::TC_GC_TRIP_WIRE)   'trip-wire) 
    ((#.system::TC_FAST_OP)        'fast-op)
    (#t (error "Bad typecode: ~s." tc))))

(define (%representation-of obj)
  (let ((tc (%typecode obj)))
    (if (= tc system::TC_FLONUM)
        (if (imag-part obj #f) 'complex 'flonum)
        (typecode->name (%typecode obj)))))



(define *class-graph* (make-hash))

(define (valid-class-name? maybe-class-name)
  "Determines if <maybe-class-name> is a valid class name (symbol, #t, or #f)."
  (or (symbol? maybe-class-name)
      (null? maybe-class-name)
      (eq? maybe-class-name #t)
      (eq? maybe-class-name #f)))

(define (validate-class-name . class-names)
  (let loop ((class-names class-names))
    (unless (null? class-names)
      (check valid-class-name? (car class-names))
      (loop (cdr class-names)))))

(define (class-superclass class-name)
  "Returns the superclass of <class-name>, #f if there are none defined."
  (validate-class-name class-name)
  (hash-ref *class-graph* class-name #f))

(define (class-superclasses class-name)
  "Returns a list of all superclasses of <class-name>, including <class-name>, in class order."
  (validate-class-name class-name)
  (if (not class-name)
      ()
      (cons class-name (class-superclasses (class-superclass class-name)))))

(define (class=? x y)
  "Returns #t if class <x> is equivalent to class <y>."
  (eq? x y))

(define (classes=? xs ys)
  "Returns #t if all of the classes of <xs> are class=? to their
   corresponding classes in <ys>."
  (cond ((and (null? xs) (null? ys))
         #t)
        ((or (null? xs) (null? ys))
         #f)
        ((class=? (car xs) (car ys))
         (classes=? (cdr xs) (cdr ys)))
        (#t
         #f)))

(define (class<=? x y)
  "Returns #t if <x> is a strict subclass of <y>."
  (validate-class-name x y)
  (cond ((not x) #t)
        ((not y) #f)
        ((or (eq? y #t) (class=? x y))
         #t)
        ((hash-has? *class-graph* x)
         (class<=? (class-superclass x) y))
        (#t #f)))



(define (classes<=? xs ys)
  "Returns #t if all of the classes of <xs> are class<=? to the corresponding
   classes of <ys>."
  (let loop ((xs xs) (ys ys))
    (cond ((null? ys) #t)
          ((null? xs) #f)
          ((or (class=? (car xs) (car ys))
               (class<=? (car xs) (car ys)))
           (loop (cdr xs) (cdr ys)))
          (#t
           #f))))

(define (invalidate-method-list-cache!) #t) ; Forward declaration

(define (make-class< sub super)
  "Defined <sub> to be a subclass of <super>."
  (validate-class-name sub super)
  (when (class<=? super sub)
    (error "Adding ~a as a subclass of ~a would introduce a circular inheritance path." sub super))
  (awhen (class-superclass sub)
         (info "Replacing the existing superclass of ~a, ~a, with ~a"
               sub it super))
  (invalidate-method-list-cache!)
  (hash-set! *class-graph* sub super))

(define (all-classes)
  "Returns a list of all currently defined classes in the class graph. These
   classes are returned in an order contstrained by the type graph: subclasses
   preceed superclasses"
  (let ((classes (make-hash :eq)))
    (dohash (sub super *class-graph* (qsort (hash-keys classes) class<=?))
            (hash-set! classes sub #t)
            (hash-set! classes super #t))))


(make-class< 'fixnum 'number)
(make-class< 'flonum 'number)
(make-class< 'complex 'flonum)
(make-class< 'subr 'procedure)
(make-class< 'closure 'procedure)
(make-class< 'nil 'cons)

(define (type-of obj)
  "Returns the type of <obj> as a symbol. For instances, the name of the type
   is the name of the prototype symbol closest up the prototype list. For
   structures, returns the structure type name."
  (cond ((instance? obj) ; REVISIT: Is there a need for instances to define their own type?
         'instance)
        ((structure? obj)
         (structure-type obj))
        (#t
         (%representation-of obj))))

;;; Method definition

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

