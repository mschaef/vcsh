;;;; sem.lisp - The semantic analyzer for the CS375 java->classfile
;;;; compiler.
;;;;
;;;; Michael Schaeffer (CS375, Hartmann)

;;;; Modifications
; rev 1 - Initial Release
;
; rev 2 - int Release
;  * Added normalization and parent-scope support for == and != nodes
;  * Extended normalization traversal to code ast's
;  * Changed internal references from unique numbers to direct pointers
;  * Subsetted out compound assignment statements (+=, etc.)
;  * Added support for reference depth attribute
;
; rev 3 - Final 375 Release
;  * Placed literals into symbol table
;  * Added support for ref attribute on literal references
;  * Added support for ref attribute on class member variables

;;;; Error handler for semantic errors

(define (sem-error error-message &rest paramaters)
  "Function call to issue a syntactic error"
  (apply 'compiler-error
	 (nconc (list 'semantic 'unknown error-message) paramaters)))

;;;; Code for manipulating Symbol Tables

(define (retrieve-symbol ast symbol-name)
  "Given an ast node and a symbol name, resolve the name into a declaration"
  (let ((parent-node
	 (get-ast-node-attribute ast 'parent))
	(symbol-binding
	 (find-if #'(lambda (x)
		      (attribute-equal? x 'name symbol-name))
		  (get-ast-node-attribute ast 'symbols))))
    (if symbol-binding
	symbol-binding
      (if (null parent-node)
	  (sem-error "Symbol ~a undeclared" symbol-name)
	(retrieve-symbol parent-node symbol-name)))))

;;; Code for defining ast traversals
;
; This code maintains a database (stored in property lists) of
; code fragments attached to symbols.  This code is used by
; the traverse-ast function to traverse the tree

(define (set-node-type-action node-type action-name action-fn)
  "Define an action for a ast node type"
  (setf (get node-type action-name) action-fn))

(define (get-node-action node action-name)
  "Retrieve an action for a ast node type"
  (get (ast-node-type node) action-name))

; !!! Make obsolete by adding support for multiple node types in s-n-t-a

(define (make-action-set-fn attr value)
  "Make a function for defining a node type action"
  #'(lambda (x)
      (set-node-type-action x attr value)))

; This is a very controlled traversal of the tree, it takes an ast
; node and a symbol as a paramater.  It then looks to see if the
; ast node has an action defined for that type of symbol
; and runs it, if true

(define (traverse-ast ast fn-attribute)
  (if (or (null ast) (atom ast))
      (sem-error "Internal error: Invalid call to traverse-ast")
    (let ((action (get-node-action ast fn-attribute)))
      (if (null action)
	  (sem-error "Internal error: Cannot find action ~a for node type ~a~%"
		     fn-attribute (ast-node-type ast))
	(funcall action ast)))))

;;; The Type System

; Types in this compiler are represented as Lisp s-exprs, base types
; are atoms and composed types are represented as lists

; atomic types: VOID, BOOLEAN, CHAR, BYTE, SHORT, INT, FLOAT
; array types: (ARRAY <type>)
; class types: (CLASS <type-id-1> ...)
; function types (FUNCTION <ret-type> <p-type-1> ... )

; A few examples:
; int i => 'int
; int i[] => '(array int)
; String strs[] => '(array (class "String"))
; void sort(int i[]) => (function void (array int))
; boolean permutation(int i[], int j[]) =>
;             '(function boolean (array int) (array int))

; The type system will provide a set of operations for
; creating new types, defining relations among types, and
; enforcing type restrictions in ast nodes, according to
; these relationships.

;; Functions to help identify and manipulate type expressions

(define (function-type-p type)
  "Determine if a type specifies a function"
  (and (consp type) (eq (first type) 'function)))

(define (function-return-type type)
  "Return the return type of a function"
  (second type))

(define (function-paramater-types type)
  "Return a list of paramater type signatures"
  (cddr type))

(define (array-type-p type)
  "Determine if a type specifies an array"
  (and (consp type) (eq (first type) 'function)))

(define (class-type-p type)
  "Determine if a type specifies a class"
  (and (consp type) (eq (first type) 'class)))

(define (compound-type-p type)
  (atom type))

(define (make-class-type-from-name name)
  "Given a type name, make a class type signature"
  `(class ,name))

;; The repositoty for type coercion, this provides a catalog of defined
;; types, and valid coercions among them

(defvar *type-table*
  (make-hash-table :test #'equal)
  "A table defining all valid type conversions")

; This function adds a new type coercion to the database.  It
; also creates any transitive coercions (ie a->b, b->c,
; therefore a->c) A side effect is that the most general types
; have to be registered first

(define (register-type-coercion old-type new-type coerce-type)
  "Register a new type coercion with the type system"
  (push (cons new-type coerce-type) (cdr (gethash old-type *type-table*)))
  (if (not (equal old-type new-type))
      (mapc #'(lambda (x)
		(if (not (eq (cdr x) 'is))
		    (push x (cdr (gethash old-type *type-table*)))))
	    (cdr (gethash new-type *type-table*)))))

(define (type-registered-p type)
  "Determine if a type has an entry in the type registry"
  (not (null (gethash type *type-table*))))


(define (type-defined-p type)
  "Determine if a class type has an entry in the type registry"
  (or (not (class-type-p type))
      (type-registered-p type)))

(define (get-type-decl-spec type)
  "Get a declaration ast node for a registered class type"
  (caar (gethash type *type-table*)))

(define (get-type-coercions type)
  "Return a list of all valid type coercions"
  (cdr (gethash type *type-table*)))

; This function registers a new type.  It goes a little out of
; the way because it also tries to set up a link to a new
; class' class-decl ast node

(define (register-type type &optional decl-spec super-type)
  "Register a new type, creating any coercions needed"
  (if (type-registered-p type)
    (sem-error "Duplicate definition of type ~a" (make-printable-type type))
    (progn
      (push (cons decl-spec '()) (gethash type *type-table*))
      (register-type-coercion type type 'is)
      (if (not (null super-type))
	  (if (type-defined-p super-type)
	      (register-type-coercion type super-type 'generalize)
	    (sem-error "Undefined superclass ~a of type ~a~%"
		       super-type type))))))

; This routine intelligently traverses the type table, and
; prints a nice report, explicitly detailing all valid
; type coercions

(define (show-all-type-coercions )
  "Print a list of all currently valid type coercions"
  (maphash #'(lambda (old-type type-definition)
	       (mapc #'(lambda (type-coercion)
			 (format t "~a -(~a)-> ~a~%"
				 (make-printable-type old-type)
				 (cdr type-coercion)
				 (make-printable-type (car type-coercion))))
		     (cdr type-definition)))
	   *type-table*))

;; Equivalence checking of types

; This function really returns true iff type-1 can be coerced into
; type-2.  As a result, type equivalence is not commutitive.

(define (type-equivalent-p type-1 type-2)
  "Determine if two types are equivalent (not commutitive)"
  (if (and (function-type-p type-1) (function-type-p type-2))
      (every #'type-equivalent-p (cdr type-1) (cdr type-2))
    (assoc type-2 (get-type-coercions type-1) :test #'equal)))

; Given two types, this function returns the type 'higher up'
; in the inheritance hierarchy

(define (less-specific-type type-1 type-2)
  "Determine the more general type"
  (cond ((type-equivalent-p type-1 type-2) type-2)
	((type-equivalent-p type-2 type-1) type-1)
	(t 'nil)))

; This function will use type-equivalent-p to generate a new
; node guaranteed to have the desired type. If it fails, it'll
; bomb out.

(define (make-type-equivalent-node ast desired-type)
  "Make a node type equivalent to a given type"
  (let* ((current-type (get-ast-node-attribute ast 'type))
	 (equivalence-relation
	  (cdr (type-equivalent-p current-type desired-type))))
    (case equivalence-relation
      ((is)
       ast)
      ((nil)
       (sem-error "Invalid type, cannot automatically coerce ~a into ~a"
		  (make-printable-type current-type)
		  (make-printable-type desired-type)))
      (otherwise
       (set-ast-node-attribute! (make-node equivalence-relation ast)
			   'type desired-type)))))

; Given a type signature, and (optionally) a name, this function will
; return a prinable string representation of said type

(define (make-printable-type type &optional name)
  "Make a string containing a printable representation of a type"
  (if (and (not (atom type)) (eq (first type) 'function))
      (format nil "~a ~a(~{~a~^, ~})"
	      (make-printable-type (second type))
	      name
	      (mapcar #'make-printable-type (cddr type)))
    (let ((base-type-name
	   (cond ((atom type)
		  (case type
		    ((void) "void")
		    ((numeric) "<numeric abstract>")
		    ((boolean) "boolean")
		    ((char) "char")
		    ((byte) "byte")
		    ((int) "int")
		    ((short) "short")
		    ((long) "long")
		    ((float) "float")
		    ((double) "double")
		    (otherwise "*")))
		 ((eq (first type) 'array)
		  (string-append (make-printable-type (second type)) "[]"))
		 ((eq (first type) 'class)
		  (second type))
		 (t "*"))))
      (if name (string-append base-type-name " " name)
	base-type-name))))

(define (make-class-file-type-signature type)
  (cond ((atom type)
	 (case type
	   ((void) "v")
	   ((boolean) "Z")
	   ((char) "C")
	   ((byte) "B")
	   ((int) "I")
	   ((short) "S")
	   ((float) "F")
	   ((double) "D")))
	((eq (first type) 'array)
	 (string-append "["
			     (make-class-file-type-signature (second type))))
	((eq (first type) 'class)
	 (string-append "L" (second type) ";"))
	((eq (first type) 'function)
	 (format nil "(~{~a~^,~})~a"
		 (mapcar #'make-class-file-type-signature (cddr type))
		 (make-class-file-type-signature (second type))))
	(t "*")))

(define (init-type-system )
  "Initialize the type system for system types"
  (clrhash *type-table*)
  (register-type 'void)
  (register-type 'boolean)
  (register-type 'char)
  (register-type 'byte)
  (register-type 'short)
  (register-type 'int)
  (register-type 'long)
  (register-type 'float)
  (register-type 'double)

  (register-type-coercion 'double 'numeric 'promote)
  (register-type-coercion 'float 'double 'promote)
  (register-type-coercion 'long 'double 'promote)
  (register-type-coercion 'int 'long 'promote)
  (register-type-coercion 'short 'int 'promote)
  (register-type-coercion 'byte 'short 'promote)
  (register-type-coercion 'char 'byte 'promote))

;; Functions for deriving types from declarations

; The type ast nodes returned from the syn phase are really
; nasty.  This function will extract the useful information
; from such a node, and return it.

(define (simplify-type-ast type-ast)
  "Given a type ast return type x prefix x type-dimen"
  (if (equal (first type-ast) 'type)
      (values
       (if (equal (third type-ast) 'identifier)
	   (cons 'class (fourth type-ast))
	 (third type-ast))
       (fifth type-ast)
       (sixth type-ast))
    (sem-error "Internal error: invalid type ast")))

(define (make-array-type type dimens)
  "Given a type spec, and a count of diminsions, return an array spec"
  (if (= 0 dimens)
      type
    `(array ,(make-array-type type (1- dimens)))))

; This will take an atomic (var or fun) declaration, and return a
; type signature

(define (derive-decl-type ast)
  "Given a declaration, return a type expression"
  (let ((decl-type (ast-node-type ast))
	(type-ast (first (ast-node-param-list ast))))
    (multiple-value-bind (type t-prefix t-dimens) (simplify-type-ast type-ast)
      (case decl-type
	((var-decl paramater)
	 (make-array-type type (fifth ast)))
	((fn-decl)
	 (cons 'function (cons type (mapcar #'derive-decl-type (fifth ast)))))
	(otherwise
	 (sem-error "Internal error: Invalid call to derive-decl-type"))))))

;; Code for deriving type data for a parse tree

; Much of the code that follows is code for enforcing type restrictions
; _and_ deriving operation types.  Any coercions that are needed to make
; the garbage the user has fed us consistent are made.

(define (paramater-typecheck ast)
  "Placeholder function for type checking paramaters"
  ; paramaters are not type checked here, so we can
  ; provide a better error message if the user screwed
  ; up the paramater list
  )

(set-node-type-action 'paramater 'typecheck #'paramater-typecheck)
(set-node-type-action 'nothing 'typecheck #'paramater-typecheck)

(define (return-typecheck ast)
  (let ((op (first (ast-node-param-list ast))))
    (set-ast-node-attribute! op 'usage 'r-value)
    (typecheck-ast op)))

(set-node-type-action 'return 'typecheck #'return-typecheck)

(define (string-literal-typecheck ast)
  "Bomb if the user tries to use a string literal"
  (sem-error "Strings not supported in this compiler"))

(set-node-type-action 'string-literal 'typecheck #'string-literal-typecheck)

(define (char-literal-typecheck ast)
  "Check a character literal, enforcing the r-value restriction"
  (let ((literal-decl (retrieve-symbol ast (car (ast-node-param-list ast)))))
    (if (member (get-ast-node-attribute ast 'usage) '(al-value l-value lr-value))
	(sem-error "Cannot use constant as l-value")
      (progn
	(set-ast-node-attribute! ast 'ref literal-decl)
	(set-ast-node-attribute! ast 'type 'char)))))

(set-node-type-action 'character-literal 'typecheck #'char-literal-typecheck)

(define (integral-literal-typecheck ast)
  "Check a integral literal, enforcing the r-value restriction"
  (let ((literal-decl (retrieve-symbol ast (car (ast-node-param-list ast)))))
    (if (member (get-ast-node-attribute ast 'usage) '(al-value l-value lr-value))
	(sem-error "Cannot use constant as l-value")
      (progn
	(set-ast-node-attribute! ast 'ref literal-decl)
	(set-ast-node-attribute! ast 'type 'int)))))

(mapc (make-action-set-fn 'typecheck #'integral-literal-typecheck)
      '(hex-literal octal-literal integer-literal))

(define (float-literal-typecheck ast)
  "Check a character literal, enforcing the r-value restriction"
  (let ((literal-decl (retrieve-symbol ast (car (ast-node-param-list ast)))))
    (if (member (get-ast-node-attribute ast 'usage) '(al-value l-value lr-value))
	(sem-error "Cannot use constant as l-value")
      (progn
	(set-ast-node-attribute! ast 'ref literal-decl)
	(set-ast-node-attribute! ast 'type 'float)))))

(set-node-type-action 'float-literal 'typecheck #'float-literal-typecheck)

(define (for-typecheck ast)
  "Typecheck a for loop"
  (if (member (get-ast-node-attribute ast 'usage) '(al-value l-value lr-value))
      (sem-error "Cannot use arithmatic operation as l-value")
    (let ((body (ast-node-param-list ast)))
      (mapc #'typecheck-ast body)
      (set-ast-node-attribute! (second body) 'usage 'r-value)
      (if (not (attribute-equal? (second body) 'type 'boolean))
	  (sem-error "For loop guards must be boolean")))))

(set-node-type-action 'for 'typecheck #'for-typecheck)

(define (conditional-typecheck ast)
  "Typecheck a conditional flow control statement"
  (if (member (get-ast-node-attribute ast 'usage) '(al-value l-value lr-value))
      (sem-error "Cannot use conditional as l-value")
    (let ((guard (first (ast-node-param-list ast)))
	  (body (rest (ast-node-param-list ast))))
      (set-ast-node-attribute! guard 'usage 'r-value)
      (typecheck-ast guard)
      (if (attribute-equal? guard 'type 'boolean)
	  (mapc #'typecheck-ast body)
	(sem-error "Conditional guards must be boolean")))))

(mapc (make-action-set-fn 'typecheck #'conditional-typecheck)
      '(if while do))

(define (compound-assign-typecheck ast)
  "Typecheck a compound assignment operator"
  (sem-error "Compound assigment statements not supported"))

(mapc (make-action-set-fn 'typecheck #'compound-assign-typecheck)
      '(plus-assign minus-assign star-assign slash-assign
	and-assign or-assign xor-assign percent-assign
	shl-assign shr-assign ushr-assign))

; The general theory of operation of the next few functions is that
; they typecheck the child nodes, and then attempt to do any
; coercion that is needed to make things right and consistant

(define (logical-op-typecheck ast)
  "Typecheck a logical composition operator"
  (if (member (get-ast-node-attribute ast 'usage) '(al-value l-value lr-value))
      (sem-error "Cannot use logical composition as l-value")
    (let ((lhs (first (ast-node-param-list ast)))
	  (rhs (second (ast-node-param-list ast))))
      (set-ast-node-attribute! ast 'type 'boolean)
      (set-ast-node-attribute! lhs 'usage 'r-value)
      (set-ast-node-attribute! rhs 'usage 'r-value)
      (typecheck-ast lhs)
      (typecheck-ast rhs)
      (let* ((lhs-type (get-ast-node-attribute lhs 'type))
	     (rhs-type (get-ast-node-attribute rhs 'type))
	     (common-type (less-specific-type lhs-type rhs-type)))
	(if (not (and (type-equivalent-p lhs-type 'boolean)
		      (type-equivalent-p rhs-type 'boolean)))
	    (sem-error "Logical compositions expect logical operands"))))))

(mapc (make-action-set-fn 'typecheck #'logical-op-typecheck)
      '(logical-and logical-or))

(define (binary-arith-compare-typecheck ast)
  "Typecheck a compare operator"
  (if (member (get-ast-node-attribute ast 'usage) '(al-value l-value lr-value))
      (sem-error "Cannot use arithmatic comparion as l-value")
    (let ((lhs (first (ast-node-param-list ast)))
	  (rhs (second (ast-node-param-list ast))))
      (set-ast-node-attribute! lhs 'usage 'r-value)
      (set-ast-node-attribute! rhs 'usage 'r-value)
      (typecheck-ast lhs)
      (typecheck-ast rhs)
      (let* ((lhs-type (get-ast-node-attribute lhs 'type))
	     (rhs-type (get-ast-node-attribute rhs 'type))
	     (common-type (less-specific-type lhs-type rhs-type)))
	(if (and (type-equivalent-p lhs-type 'numeric)
		 (type-equivalent-p rhs-type 'numeric))
	    (progn
	      (set-ast-node-attribute! ast 'type 'boolean)
	      (setf (first (ast-node-param-list ast))
		    (make-type-equivalent-node lhs common-type))
	      (setf (second (ast-node-param-list ast))
		    (make-type-equivalent-node rhs common-type)))
	  (sem-error "Arithmatic comparisons expect numeric operands"))))))

(mapc (make-action-set-fn 'typecheck #'binary-arith-compare-typecheck)
      '(less-or-equal greater-or-equal greater less equal not-equal))

(define (arith-negate-typecheck ast)
  "Typecheck a compare operator"
  (if (member (get-ast-node-attribute ast 'usage) '(al-value l-value lr-value))
      (sem-error "Cannot use arithmatic negation as l-value")
    (let ((op (first (ast-node-param-list ast))))
      (set-ast-node-attribute! op 'usage 'r-value)
      (typecheck-ast op)
      (let ((op-type (get-ast-node-attribute op 'type)))
	(if (type-equivalent-p op-type 'numeric)
	    (set-ast-node-attribute! ast 'type op-type)
	  (sem-error "Arithmatic negations expect numeric operands"))))))

(mapc (make-action-set-fn 'typecheck #'arith-negate-typecheck)
      '(arithmatic-negate))

(define (binary-arith-op-typecheck ast)
  "Typecheck a binary arithmatic operation"
  (if (member (get-ast-node-attribute ast 'usage) '(al-value l-value lr-value))
      (sem-error "Cannot use arithmatic operation as l-value")
    (let ((lhs (first (ast-node-param-list ast)))
	  (rhs (second (ast-node-param-list ast))))
      (set-ast-node-attribute! lhs 'usage 'r-value)
      (set-ast-node-attribute! rhs 'usage 'r-value)
      (typecheck-ast lhs)
      (typecheck-ast rhs)
      (let* ((lhs-type (get-ast-node-attribute lhs 'type))
	     (rhs-type (get-ast-node-attribute rhs 'type))
	     (common-type (less-specific-type lhs-type rhs-type)))
	(if (and (type-equivalent-p lhs-type 'numeric)
		 (type-equivalent-p rhs-type 'numeric))
	    (progn
	      (set-ast-node-attribute! ast 'type common-type)
	      (setf (first (ast-node-param-list ast))
		    (make-type-equivalent-node lhs common-type))
	      (setf (second (ast-node-param-list ast))
		    (make-type-equivalent-node rhs common-type)))
	  (sem-error "Arithmatic operators expect numeric operands"))))))

(mapc (make-action-set-fn 'typecheck #'binary-arith-op-typecheck)
      '(plus minus star slash bitwise-and bitwise-xor bitwise-or
	     shift-right shift-left unsigned-shift-right))

(define (strip-indirections type levels)
  "Given an array type signature, strip off n (array <n>)'s"
  (if (= levels 0)
      type
    (strip-indirections (cadr type) (1- levels))))

(define (arrayref-typecheck ast)
  "Type check an array reference"
  (let ((symbol-decl (retrieve-symbol ast (first (ast-node-param-list ast))))
	(ind-levels 0)
	(reference-depth (get-ast-node-attribute ast 'ref-depth)))
    (if (null reference-depth)
	(set-ast-node-attribute! ast 'ref-depth 0))
    (mapc #'(lambda (x)
	      (typecheck-ast x)
	      (incf ind-levels)) (second (ast-node-param-list ast)))
    (set-ast-node-attribute! ast 'type
			(strip-indirections (get-ast-node-attribute symbol-decl 'type)
					    ind-levels))))


(set-node-type-action 'array-ref 'typecheck #'arrayref-typecheck)

(define (funcall-typecheck ast)
  "Type check a function call"
  (let ((function (retrieve-symbol ast (first (ast-node-param-list ast))))
	(reference-depth (get-ast-node-attribute ast 'ref-depth)))
    (if (null reference-depth)
	(set-ast-node-attribute! ast 'ref-depth 0))
    (mapc #'typecheck-ast (second (ast-node-param-list ast)))
    (set-ast-node-attribute! ast 'ref function)
    (set-ast-node-attribute! ast 'type (function-return-type
				   (get-ast-node-attribute function 'type)))
    (setf (second (ast-node-param-list ast))
	  (mapcar #'(lambda (x y)
		      (make-type-equivalent-node x y))
		  (second (ast-node-param-list ast))
		  (function-paramater-types (get-ast-node-attribute function 'type))))))

(set-node-type-action 'funcall 'typecheck #'funcall-typecheck)

(define (variable-typecheck ast)
  "Type check a variable reference"
  (let* ((symbol-decl (retrieve-symbol ast (first (ast-node-param-list ast))))
	 (symbol-succ (second (ast-node-param-list ast)))
	 (symbol-type (get-ast-node-attribute symbol-decl 'type))
	 (reference-depth (get-ast-node-attribute ast 'ref-depth)))
    (if (null reference-depth)
	(progn
	  (setf reference-depth 0)
	  (set-ast-node-attribute! ast 'ref-depth 0)))
    (set-ast-node-attribute! ast 'type symbol-type)
    (set-ast-node-attribute! ast 'actual-type symbol-type)
    (set-ast-node-attribute! ast 'ref symbol-decl)
    (if (class-type-p symbol-type)
	(if symbol-succ
	    (progn
	      (set-ast-node-attribute! symbol-succ 'ref-depth
				  (1+ reference-depth))
	      (set-ast-node-attribute! symbol-succ 'parent
				  (get-type-decl-spec symbol-type))
	      (set-ast-node-attribute! symbol-succ 'usage
				  (get-ast-node-attribute ast 'usage))
	      (set-ast-node-attribute! ast 'usage 'r-value)
	      (typecheck-ast symbol-succ)
	      (set-ast-node-attribute! ast 'type
				  (get-ast-node-attribute  symbol-succ 'type))))
      (if symbol-succ
	  (sem-error "Cannot dereference an atomic type")))))

(set-node-type-action 'variable-ref 'typecheck #'variable-typecheck)

(define (assign-typecheck ast)
  "Type check an assignment statement"
  (let ((lhs (first (ast-node-param-list ast)))
	(rhs (second (ast-node-param-list ast))))
    (set-ast-node-attribute! lhs 'usage 'al-value)
    (set-ast-node-attribute! rhs 'usage 'r-value)
    (typecheck-ast lhs)
    (typecheck-ast rhs)
    (setf (second (ast-node-param-list ast))
	  (make-type-equivalent-node rhs (get-ast-node-attribute lhs 'type)))))

(set-node-type-action 'assign 'typecheck #'assign-typecheck)

(define (block-typecheck ast)
  "Type check a code block"
  (mapc #'typecheck-ast
	(get-ast-node-attribute ast 'symbols))
  (mapc #'typecheck-ast (ast-node-param-list ast)))

(set-node-type-action 'block 'typecheck #'block-typecheck)

(define (fn-typecheck ast)
  "Type check a function, including the formal paramater list"
  (let ((type (get-ast-node-attribute ast 'type))
	(name (get-ast-node-attribute ast 'name)))
    (if (function-type-p type)
	(progn
	  (mapc #'(lambda (x)
		    (if (not (type-defined-p x))
			(sem-error "type ~a undefined in declaration:~%   ~a~%"
				   (make-printable-type x)
				   (make-printable-type type name))))
		(cdr type))
	  (typecheck-ast (get-ast-node-attribute ast 'code)))
      (sem-error "Internal error: Function ~a has invalid type signature~%"
		 name))))

(set-node-type-action 'fn-decl 'typecheck #'fn-typecheck)

(define (var-typecheck ast)
  (let ((type (get-ast-node-attribute ast 'type)))
    (if (not (type-defined-p type))
	(sem-error "Type ~a undefined~%" (make-printable-type type)))))

(set-node-type-action 'var-decl 'typecheck #'var-typecheck)

; The class type checker also extends the sub-type symbol table
; to point to the base class symbols.

(define (class-typecheck ast)
  "Typecheck a class ast and register the class type"
  (let ((class-name (get-ast-node-attribute ast 'name))
	(base-class-name (get-ast-node-attribute ast 'base-name)))
    (let ((class (make-class-type-from-name class-name))
	  (base-class (if (not (null base-class-name))
			  (make-class-type-from-name base-class-name))))
      (register-type class ast base-class)
      (mapc #'(lambda (x)
		(if (member (ast-node-type x) '(fn-decl var-decl))
		    (typecheck-ast x)))
	    (get-ast-node-attribute ast 'symbols))
      (set-ast-node-attribute! ast 'symbols
			(append (get-ast-node-attribute ast 'symbols)
				(get-ast-node-attribute (get-type-decl-spec
						     base-class)
						    'symbols))))))

(set-node-type-action 'class-decl 'typecheck #'class-typecheck)

(define (file-typecheck ast)
  "Typecheck all of the symbols defined in a file"
  (mapc #'(lambda (x) (typecheck-ast x))
	(get-ast-node-attribute ast 'symbols)))

(set-node-type-action 'file 'typecheck #'file-typecheck)

(define (typecheck-ast ast)
  "Type check an entire ast, top down"
  (if (not (null ast))
      (traverse-ast ast 'typecheck))
  ast)

;;;; Code for normalizing code blocks into a standard form
;
; The next section of code is the 'normalizer'.  Basically what it
; does is to take the messy ast from the syn phase, and massage
; it into something I can use.  This entails moving most declarations
; and typing data into ast attributes.  The net result is an
; ast that has no 'child nodes' the first few levels of depth.  It
; also happens to yield a structure that is _very_ close to being
; a passable symbol table and ast all rolled into one neat, clean
; package

(define (consolodate-decls code &optional decls executable)
  "Given a code block, consolodate and remove all declarations"
  (if (null code)
      (values decls executable)
    (let ((first-statement (first code))
	  (remaining-statements (rest code)))
      (if (eq (ast-node-type first-statement) 'decl-list)
	  (consolodate-decls remaining-statements
			     (append decls
				     (ast-node-param-list first-statement))
			     executable)
	(consolodate-decls remaining-statements
			   decls
			   (append executable (list first-statement)))))))

(define (file-normalize ast)
  "Normalize a file"
  (let ((file-decls
	 (mapcar #'(lambda (x) (normalize-ast x))
		 (ast-node-param-list ast))))
    (set-ast-node-attribute! ast 'symbols file-decls))
  (set-ast-node-param-list! ast '())
  ast)

(set-node-type-action 'file 'normalize #'file-normalize)

(define (class-normalize ast)
  "Normalize a class declaration"
  (let ((node-params (ast-node-param-list ast)))
    (let ((class-name (first node-params))
	  (base-class-name (second node-params))
	  (decl-list (cdddr node-params)))
      (multiple-value-bind (decls executable)
	  (consolodate-decls (cdddr node-params))
	(set-ast-node-attribute! ast 'symbols
			    (append
			     (mapcar #'(lambda (x)
					 (normalize-ast x)
					 (set-ast-node-attribute! x 'parent ast))
				     decls)
			     (get-literal-lexeme-ast)))
	(set-ast-node-attribute! ast 'base-name base-class-name)
	(set-ast-node-attribute! ast 'name class-name)
	(set-ast-node-param-list! ast '()))))
  ast)

(set-node-type-action 'class-decl 'normalize #'class-normalize)

(define (fn-normalize ast)
  "Normalize a function definition"
  (let ((node-params (ast-node-param-list ast)))
    (let ((type-signature (derive-decl-type ast))
	  (name (second node-params))
	  (param-list (third node-params))
	  (type-modifiers (fifth (first node-params)))
	  (code (fifth node-params)))
      (set-ast-node-attribute! ast 'code (normalize-ast code))
      (set-ast-node-attribute! ast 'param-list (mapcar #'normalize-ast param-list))
      (set-ast-node-attribute! ast 'type-modifiers type-modifiers)
      (set-ast-node-attribute! ast 'type type-signature)
      (set-ast-node-attribute! ast 'name name)
      (set-ast-node-attribute! code 'symbols
			  (append  (mapcar
				    #'(lambda (x)
					(set-ast-node-attribute! x 'type-modifiers
							    (append
							     '(local)
							     (get-ast-node-attribute
							      x 'type-modifiers)))
					x)
				    (get-ast-node-attribute code 'symbols))
				   param-list))
      (set-ast-node-param-list! ast '())))
  ast)

(set-node-type-action 'fn-decl 'normalize #'fn-normalize)

(define (var-normalize ast)
  "Normalize a variable definition"
  (let ((node-params (ast-node-param-list ast)))
    (let ((type-signature (derive-decl-type ast))
	  (name (second node-params))
	  (type-modifiers (if (eq (first ast) 'paramater)
			      (cons 'paramater (fifth (first node-params)))
			    (fifth (first node-params)))))
      (set-ast-node-attribute! ast 'type-modifiers type-modifiers)
      (set-ast-node-attribute! ast 'type type-signature)
      (set-ast-node-attribute! ast 'unique (gensym))
      (set-ast-node-attribute! ast 'name name)
      (set-ast-node-param-list! ast '())))
  ast)

(set-node-type-action 'paramater 'normalize #'var-normalize)
(set-node-type-action 'var-decl 'normalize #'var-normalize)

(define (block-normalize ast)
  "Normalize a statement block"
  (multiple-value-bind (decls executable)
      (consolodate-decls (ast-node-param-list ast))
    (set-ast-node-attribute! ast 'symbols (mapcar #'normalize-ast decls))
    (set-ast-node-param-list! ast (mapcar #'normalize-ast executable))
    ast))

(set-node-type-action 'block 'normalize #'block-normalize)

(define (null-normalize ast)
  "Do nothing"
  ast)

(mapc (make-action-set-fn 'normalize #'null-normalize)
      '(nothing variable-ref assign plus minus star slash
        numeric-literal octal-literal integer-literal
	hex-literal character-literal
	logical-or logical-and not-equal funcall
	bitwise-or bitwise-and bitwise-xor bitwise-and
	shift-right shift-left unsigned-shift-right
	less greater less-or-equal greater-or-equal equal
	plus-assign minus-assign star-assign slash-assign
	and-assign or-assign xor-assign percent-assign
	shl-assign shr-assign ushr-assign return))

(define (if-normalize ast)
  (normalize-ast (third ast))
  (normalize-ast (fourth ast))
  ast)

(set-node-type-action 'if 'normalize #'if-normalize)

(define (for-normalize ast)
  (normalize-ast (sixth ast))
  ast)

(set-node-type-action 'for 'normalize #'for-normalize)

(define (loop-normalize ast)
  (normalize-ast (fifth ast))
  ast)

(set-node-type-action 'do 'normalize #'loop-normalize)
(set-node-type-action 'while 'normalize #'loop-normalize)

(define (normalize-ast ast)
  "Normalize an arbitrary ast"
  (if (not (null ast))
      (traverse-ast ast 'normalize)))

;;;; 'Parent Scoper'
;
; This code is _really_ straight forward.  All it does is set up the
; parent links back up the ast.  This is critically important because
; symbol name resolution wouldn't even come close to working with out
; said links.
;
; The net result of the parentizing of the nodes will be a circular data
; structure.  This next statement keeps a common lisp print routine
; from looping infinitely

(setf *print-circle* 't)

(define (atomic-set-parent-scope ast)
  "Do nothing"
  ast)

(mapc (make-action-set-fn 'parent-scope #'atomic-set-parent-scope)
      '(nothing
	hex-literal octal-literal float-literal integer-literal
	character-literal string-literal))

(define (variable-set-parent-scope ast)
  (let ((succ (second (ast-node-param-list ast))))
    (if (not (null succ))
	(progn
	  (set-ast-node-attribute! succ 'parent ast)
	  (set-ast-parent-scope succ))))
  ast)

(set-node-type-action 'variable-ref 'parent-scope #'variable-set-parent-scope)

(define (composite-set-parent-scope ast)
  "Set the parent attributes for a composute statement"
  (mapc #'(lambda (x)
	    (if (not (null x))
		(set-ast-node-attribute! x 'parent ast))
	    (set-ast-parent-scope x))
	(ast-node-param-list ast))
  ast)

(mapc (make-action-set-fn 'parent-scope #'composite-set-parent-scope)
      '(block if for while do assign plus minus star slash
	logical-or logical-and not-equal arithmatic-negate
	bitwise-or bitwise-and bitwise-xor bitwise-and
	shift-right shift-left unsigned-shift-right
	less greater less-or-equal greater-or-equal equal
	plus-assign minus-assign star-assign slash-assign
	and-assign or-assign xor-assign percent-assign
	shl-assign shr-assign ushr-assign return))

(define (funcall-set-parent-scope ast)
  "Set the parent attributes for a function call"
  (mapc #'(lambda (x)
	    (set-ast-node-attribute! x 'parent ast)
	    (set-ast-parent-scope x))
	(second (ast-node-param-list ast))))

(mapc (make-action-set-fn 'parent-scope #'funcall-set-parent-scope)
      '(array-ref funcall))

(define (fn-set-parent-scope ast)
  "Set the parent attributes for a function definition"
  (let ((fn-code (get-ast-node-attribute ast 'code)))
    (set-ast-node-attribute! fn-code 'parent ast)
    (set-ast-parent-scope fn-code))
  ast)

(set-node-type-action 'fn-decl 'parent-scope #'fn-set-parent-scope)

(define (class-set-parent-scope ast)
  "Set the parent attributes for a class definition"
  (mapc #'(lambda (x)
	    (if (eq (first x) 'fn-decl)
		(progn
		  (set-ast-node-attribute! x 'ref ast)
		  (set-ast-node-attribute! x 'parent ast)
		  (set-ast-parent-scope x))))
	(get-ast-node-attribute ast 'symbols)))

(set-node-type-action 'class-decl 'parent-scope #'class-set-parent-scope)

(define (file-set-parent-scope ast)
  "Set the parent attributes for a file"
  (mapc #'set-ast-parent-scope (get-ast-node-attribute ast 'symbols))
  ast)

(set-node-type-action 'file 'parent-scope #'file-set-parent-scope)

(define (set-ast-parent-scope ast)
  "Set the parent scopes in an ast"
  (if (not (null ast))
      (traverse-ast ast 'parent-scope)))

;;;; Prototype print utility
;
; This silly little utility will take a file ast and print
; out a set of class type signatures in real java syntax

(define (var-print-prototype ast)
  "Print a variable prototype"
  (format t "  ~a;~%"
	  (make-printable-type (get-ast-node-attribute ast 'type)
			       (get-ast-node-attribute ast 'name))))

(mapc (make-action-set-fn 'print-prototype #'var-print-prototype)
      '(fn-decl var-decl))

(define (class-print-prototype ast)
  "Set the parent attributes for a class definition"
  (let ((class-name (get-ast-node-attribute ast 'name))
	(base-class-name (get-ast-node-attribute ast 'base-name)))
    (if base-class-name
	(format t "~%class ~a extends ~a {~%"
		class-name
		base-class-name)
      (format t "class ~a {~%"
	      class-name))
    (mapc #'print-ast-prototype (get-ast-node-attribute ast 'symbols))
    (format t "}~%")))

(set-node-type-action 'class-decl 'print-prototype #'class-print-prototype)

(define (file-print-prototype ast)
  "Set the parent attributes for a file"
  (mapc #'print-ast-prototype (get-ast-node-attribute ast 'symbols))
  ast)

(set-node-type-action 'file 'print-prototype #'file-print-prototype)

(define (print-ast-prototype ast)
  (traverse-ast ast 'print-prototype))

(define (init-sem )
  "Initialize the semantic analyzer"
  (init-type-system))

(define (analyze-file filename)
  "Return a sliced, diced, and typechecked ast for an input file"
  (init-sem)
  (typecheck-ast
   (set-ast-parent-scope
    (normalize-ast (get-file-ast filename)))))
