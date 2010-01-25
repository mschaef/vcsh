;;;; class-graph.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; The class inheritance graph.

(in-package! "scheme")

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
  (cond ((instance? obj)
         ;; TODO: Instances should always be of type 'instance, unless they implement
         ;; a type-of message that returns an alternative type.
         (let next-instance ((obj obj))
           (let ((proto (%instance-proto obj)))
             (cond ((symbol? proto)
                    proto)
                   ((instance? proto)
                    (next-instance proto))
                   (#t
                    'instance)))))
        ((structure? obj)
         (structure-type obj))
        (#t
         (%representation-of obj))))

;;; Method definition



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
  (values-bind (parse-proto-name) (name base)
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
             (values-bind (procedure-arity it) (arity rest?)
               (> arity 0)))))

;;; Generic Functions

(define (signature-list->english type-names)
  "Given a list of symbols, <type-names>, return an English string
   listing each element."
  (define (vowel? letter)
    (member letter '(#\a #\e #\i #\o #\u #\y #\A #\E #\I #\O #\U #\Y)))
  (define (type-name-connective type-name)
    (cond ((eq? type-name #t) "a")
          ((vowel? (string-ref (symbol-name type-name) 0)) "an")
          (#t "a")))  (let ((need-commas? (> (length type-names) 2))
        (need-and? (> (length type-names) 1))
        (op (open-output-string)))
    (let loop ((names type-names) (pos 0))
      (cond ((null? names) (get-output-string op))
            (#t
             (unless (or (symbol? (car names)) (eq? (car names) #t))
               (error "Type names must be symbols or #t: ~a" (car names)))
             (when (and (> pos 0) need-commas?) (display ", " op))

             (when (null? (cdr names))
               (when need-and?
                 (when (= pos 1) (display " " op))
                 (display "and " op)))
             (format op "~a ~a" (type-name-connective (car names)) (car names))
             (loop (cdr names) (+ 1 pos)))))))

(define (generic-function? obj)
  "Returns #t if <obj> is a generic function."
  (and (closure? obj)
       (aif (assq 'generic-function? (%property-list obj))
            (cdr it)
            #f)))

(define *generic-function-method-list-cache* (make-hash :equal))

(define (invalidate-method-list-cache!)
  "%Invalidiates the method list cache."
  (set! *generic-function-method-list-cache* (make-hash :equal)))

(define (generic-function-methods generic-function)
  (check generic-function? generic-function)
  (get-property generic-function 'method-table ()))

(define (compute-methods-for-signature generic-function signature)
  "Computes the methods defined on <generic-function> that apply to <signature>."
  (let ((method-list (get-property generic-function 'method-table ())))
    (map cdr (filter (lambda (method-list-entry)
                       (classes<=? signature (car method-list-entry)))
                     method-list))))

(define (methods-for-signature generic-function signature)
  "Returns the methods defined on  <generic-function> that apply to <signature>."
  (let ((sig-key (cons generic-function signature)))
    (aif (hash-ref *generic-function-method-list-cache* sig-key #f)
         it
         (let ((method-list (compute-methods-for-signature generic-function signature)))
           (hash-set! *generic-function-method-list-cache* sig-key method-list)
           method-list))))


(define (%generic-function gf-name arity doc-string)
  (letrec ((new-gf (lambda gf-args
                     (let ((current-methods (methods-for-signature new-gf (map type-of gf-args))))
                       (assert (not (null? current-methods)))
                       (letrec ((call-next-method (lambda ()
                                                 (let ((next-fn (pop! current-methods)))
                                                   (apply next-fn call-next-method gf-args)))))
                         (call-next-method))))))

    (set-property! new-gf 'name gf-name)
    (set-property! new-gf 'generic-function? #t)
    (set-property! new-gf 'documentation (aif doc-string it "Generic Function"))
    (set-property! new-gf 'method-table ())
    (set-property! new-gf 'generic-function-arity arity)

    new-gf))

(defmacro (define-generic-function lambda-list . code)
  (unless (list? lambda-list)
    (error "Generic function lambda lists must be proper lists: ~a" lambda-list))
  (let ((fn-name (car lambda-list))
        (fn-args (cdr lambda-list)))
    (unless (symbol? fn-name)
      (error "Generic function names must be symbols: ~a" fn-name))
    (unless (every? symbol? fn-args)
      (error "Generic function argument names must be symbols: ~a" fn-args))

    (values-bind (parse-code-body code) (doc-string declarations code)
      (with-gensyms (function-sym)
        `(begin
           (%define-global ',fn-name (%generic-function ',fn-name ,(length fn-args) ,doc-string))

           ,(if (null? code)
                `(define-method (,fn-name ,@(map #L(list _ #t) (cdr lambda-list)))
                   (error "generic function ~a undefined on arguments of type ~a" ,function-sym
                          (list ,@(map (lambda (arg-name) `(type-of ,arg-name)) fn-args))))
                `(define-method (,fn-name ,@(map #L(list _ #t) (cdr lambda-list)))
                   ,@code))

           ,fn-name)))))


(define (generic-function-signatures function)
  "Returns a list of the defined signatures of generic function <function>."
  (aif (get-property function 'method-table)
       (map car it)
       '()))

(define (%update-method-list! method-list method-signature method-closure)
  "Updates <method-list> to include <method-closure> as the definition
   for <method-signature>. If <method-signature> is already defined in the
   method list, then the existing definition is replaced."
  (aif (find (lambda (method-list-entry)
               (classes=? (car method-list-entry) method-signature))
             method-list)
       (begin
         (set-cdr! it method-closure)
         method-list)
       (insert-ordered method-list (cons method-signature method-closure) classes<=? car)))

(define (%add-generic-function-method generic-function method-signature method-closure)
  "Adds the method defined by <method-closure>, with the signature <method-signature>,
   to <generic-function>"
  (unless (generic-function? generic-function)
    (error "Generic function expected" generic-function))
  (unless (closure? method-closure)
    (error "Closure expected" method-closure))
  (unless (or (null? method-signature) (list? method-signature))
    (error "Method signatures must be lists" method-signature))

  (invalidate-method-list-cache!)
  (set-property! generic-function 'method-table
                            (%update-method-list! (get-property generic-function 'method-table)
                                                  method-signature method-closure)))


(defmacro (define-method lambda-list . code)
  (unless (list? lambda-list)
    (error "Method lambda lists must be proper lists: ~a" lambda-list))
  (let ((fn-expr (car lambda-list))
        (fn-args (cdr lambda-list)))

    (unless (every? (lambda (fn-arg) (or (symbol? fn-arg)
                                         (and (list? fn-arg) (length=2? fn-arg))))
                    fn-args)
      (error "Method arguments must be specified as 2-element lists or symbols." fn-args))

    (let ((fn-args (map (lambda (fn-arg) (if (symbol? fn-arg) (list fn-arg #t) fn-arg)) fn-args)))
      (unless (every? (lambda (argument) (symbol? (car argument))) fn-args)
        (error "Method argument names must be symbol." fn-args))
      (unless (every? (lambda (argument) (every? valid-class-name? (cdr argument))) fn-args)
        (error "Methods argument types must be classes." fn-args))

      (let ((fn-arg-names (map car fn-args))
            (fn-signature (map cadr fn-args)))
        `(begin
           (unless (= ,(length fn-args) (get-property ,fn-expr 'generic-function-arity -1))
             (error "Arity mismatch in method definition for ~a, generic function expects ~a arguments, method expects ~a."
                    ',fn-expr
                    (get-property ,fn-expr 'generic-function-arity -1)
                    (length ',fn-args)))

           (%add-generic-function-method ,fn-expr ',fn-signature
                                         (lambda (call-next-method ,@fn-arg-names)  ,@code)))))))
