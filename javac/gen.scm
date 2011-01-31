;;;; gen.lisp - The final code generator  for the CS375 java->classfile
;;;; compiler.
;;;;
;;;; Michael Schaeffer (CS375, Hartmann)

;;;; Modifications
; rev 1 - Initial Release (Final 375 deliverable)

;;;; Misc. header generation code

(define (generate-classfile-prologue )
  (make-bq
   202 254 186 190 ; Magic Number (CAFEBABE)
   3 45))          ; Version Number

;;;; Bytecode assembler
;
; This implements a fairly simple two pass assembler.  This is greatly
; simpified becasue the opcodes used are represented internally by Lisp
; lists.  This is also true because of the regularity of the byte code
; used by java.

;;; Instruction definition code

(define (define-instr node-type length assembler-fn)
  "Set the length of an ast node as an int of a function"
  (setf (get node-type 'instr-length) length)
  (setf (get node-type 'instr-asm) assembler-fn))

(define (get-ast-length node)
  "Retrieve the length of an ast node"
  (get (ast-node-type node) 'instr-length))

(define (assemble-instr node)
  "Retrieve the length of an ast node"
  (funcall (get (ast-node-type node) 'instr-asm) node))

;;; Assembler functions

(define (assemble-mathop ast)
  (let ((type (get-ast-node-attribute ast 'type))
	(op (ast-node-type ast)))
    (make-bq
     (cdr
      (assoc type
	     (cdr
	      (assoc op
		     '((plus .
			     ((byte . 96) (short . 96) (int . 96)
			      (long . 97) (float . 98) (double . 99)))
		       (minus .
			      ((byte . 100) (short . 100) (int . 100)
			       (long . 101) (float . 102) (double . 103)))
		       (star .
			     ((byte . 104) (short . 104) (int . 104)
			      (long . 105) (float . 106) (double . 107)))
		       (slash .
			      ((byte . 108) (short . 108) (int . 108)
			       (long . 109) (float . 110) (double . 111)))
		       (negate .
			       ((byte . 116) (short . 116) (int . 116)
				(long . 117) (float . 118) (double . 119)))
		       (shift-left .
				   ((byte . 120) (short . 120) (int . 120)
				    (long . 121)))
		       (shift-right .
				    ((byte . 122) (short . 122) (int . 122)
				     (long . 123)))
		       (unsigned-shift-right . ((byte . 124) (short . 124)
						(int . 124) (long . 125)))
		       (bitwise-or . ((byte . 128) (short . 128)
				      (int . 128) (long . 129)))
		       (bitwise-and . ((byte . 126) (short . 126)
				       (int . 126) (long . 127)))
		       (bitwise-xor . ((byte . 130) (short . 130)
				       (int . 130) (long . 131)))))))))))

(define (assemble-promotion ast)
  (let ((type-1 (first (ast-node-param-list ast)))
	(type-2 (second (ast-node-param-list ast))))
    (make-bq
     (cdr
      (assoc type-2
	     (cdr
	      (assoc type-1
		     '((byte . ((long . 133) (float . 134) (double . 135)))
		       (short . ((long . 133) (float . 134) (double . 135)))
		       (int . ((long . 133) (float . 134) (double . 135)))
		       (long . ((int . 136) (float . 137) (double . 138)))
		       (float . ((int . 139) (long . 140) (double . 141)))
		       (double . ((int . 142) (long . 143) (float . 144
								  )))))))))))

; TODO: add support for types

(define (assemble-constant ast)
  (let ((constant-target (get-entry-offset (first (ast-node-param-list ast)))))
    (concatenate-bqs
     (make-bq 'const-load)
     (make-bytes-from-short constant-target))))

(define (assemble-incomplete ast)
  (list (ast-node-type ast)))

(define (assemble-jump ast)
  (let ((jump-type (ast-node-type ast))
	(jump-target
	 (get-ast-node-attribute (first (ast-node-param-list ast)) 'offset)))
    (concatenate-bqs
     (make-bq
      (case jump-type
	((goto) 167)
	((if-integer-not) 160)
	((if-integer-equal) 159)
	((if-integer-less) 161)
	((if-integer-less-or-equal) 164)
	((if-integer-greater) 163)
	((if-integer-greater-or-equal) 162)
	((jump-if-not-equal) 154)
	((jump-if-equal) 153)
	((jump-if-less) 155)
	((jump-if-less-or-equal) 158)
	((jump-if-greater) 157)
	((jump-if-greater-or-equal) 156)))
     (make-bytes-from-short jump-target))))

;;; Instruction definitions


(define-instr 'plus 1 #'assemble-mathop)
(define-instr 'minus 1 #'assemble-mathop)
(define-instr 'slash 1 #'assemble-mathop)
(define-instr 'star 1 #'assemble-mathop)
(define-instr 'negate 1 #'assemble-mathop)
(define-instr 'shift-left 1 #'assemble-mathop)
(define-instr 'shift-right 1 #'assemble-mathop)
(define-instr 'unsigned-shift-right 1 #'assemble-mathop)
(define-instr 'bitwise-or 1 #'assemble-mathop)
(define-instr 'bitwise-and 1 #'assemble-mathop)
(define-instr 'bitwise-xor 1 #'assemble-mathop)

(define-instr 'promote 1 #'assemble-promotion)
(define-instr 'coerce 1 #'assemble-promotion)

(define-instr 'store 2 #'assemble-incomplete)
(define-instr 'load 2 #'assemble-incomplete)
(define-instr 'get-static 3 #'assemble-incomplete)
(define-instr 'get-field 3 #'assemble-incomplete)
(define-instr 'put-field 3 #'assemble-incomplete)
(define-instr 'nop 1 #'assemble-incomplete)
(define-instr 'invoke-virtual 3 #'assemble-incomplete)
(define-instr 'invoke-static 3 #'assemble-incomplete)
(define-instr 'return 1 #'assemble-incomplete)
(define-instr 'compare 1 #'assemble-incomplete)
(define-instr 'constant 3 #'assemble-incomplete)

(define-instr 'goto 3 #'assemble-jump)
(define-instr 'if-integer-equal 3 #'assemble-jump)
(define-instr 'if-integer-not-equal 3 #'assemble-jump)
(define-instr 'if-integer-less 3 #'assemble-jump)
(define-instr 'if-integer-greater 3 #'assemble-jump)
(define-instr 'if-integer-less-or-equal 3 #'assemble-jump)
(define-instr 'if-integer-greater-or-equal 3 #'assemble-jump)
(define-instr 'jump-if-less 3 #'assemble-jump)
(define-instr 'jump-if-less-or-equal 3 #'assemble-jump)
(define-instr 'jump-if-greater 3 #'assemble-jump)
(define-instr 'jump-if-greater-or-equal 3 #'assemble-jump)
(define-instr 'jump-if-not-equal 3 #'assemble-jump)

;;; Assembly

(define (generate-bytecode-bytes ast)
  (mapcar #'assemble-instr ast))

;;; Offset assignment

(define (assign-bytecode-offsets ast)
  (let ((offset 0))
    (mapc #'(lambda (x)
	      (set-ast-node-attribute! x 'offset offset)
	      (incf offset (get-ast-length x)))
	  ast)))

;;; Assembler front end

(define (assemble-bytecode ast)
  (generate-bytecode-bytes
   (assign-bytecode-offsets ast)))

;;;; Code to generate the constant pool
;
; Like other parts of this compiler, a fairly specific piece of code
; to generate the constant pool evolved into doing almost everything
; involved in this phase. Such is the power of lisp, I suppose. Anyway,
; basically what this phase does is to turn the ast we've been working on
; since the lex phase inside out and generate a list of class file elements.
; This process starts by taking the symbol table, and 'flattening' it so that
; all symbols and literals defined in the program are in a linear list.  Then,
; pseudo nodes, representing parts of the file that arn't the constant pool
; are added into this list. The list is then traversed, expanding each node
; into any composite nodes.  To help maintain the relationships between list
; elements, parts of the ast are retained as element attributes. Once the
; list is fully expanded, we have a high level, but linear, representation of
; the contents of the class file.  All that remains is to take this list and
; convert it into a sequence of bytes.  This is doable in one pass.

;;; Symbol table linearizer
;
; This code flattens the symbol table, and adds special nodes for other
; parts of the class file

(define (generate-linear-symbol-table ast)
  "Linearize the symbol table, adding pseudo-symbols representing
   parts of the final class file"
  (let ((symbols (get-ast-node-attribute ast 'symbols)))
    (append
     (list ast)
     symbols
     (apply 'append
	    (mapcar #'(lambda (x) (get-ast-node-attribute x 'symbols)) symbols))
     (list
      (make-node 'class-info ast)
      (make-node 'interfaces ast)
      (make-node 'fields ast)
      (make-node 'methods ast)
      (make-node 'attributes ast)))))

;;; Constant pool expansion traversal
;
; This code takes symbol table entries that need more than one constant
; pool entry to represent, and expands them into component parts.

;; Constructors for various types of constant pool nodes

(define (make-type-constant-pool-entry typesig)
  (make-node 'type typesig))

(define (make-name-constant-pool-entry name)
  (let ((entry (make-node 'string-literal)))
    (set-ast-node-attribute! entry 'name name)
    entry))

(define (make-name-and-type-constant-pool-entry name type)
  (make-node 'name-and-type name type))

(define (make-explicit-short-constant-pool-entry short)
  (make-node 'explicit-short short))

(define (make-explicit-reference-constant-pool-entry short)
  (make-node 'explicit-reference short))

(define (make-ref-entry ast n-t-e)
  (make-node 'ref
	     (get-ast-node-attribute (get-ast-node-attribute ast 'parent)
				 'class-entry)
	     n-t-e))

(define (make-field-constant-pool-entry field)
  (let ((field-decl (first (ast-node-param-list field))))
    (list
     (make-node 'field 0
		(get-ast-node-attribute field 'name-entry)
		(get-ast-node-attribute field 'type-entry)
		0))))


(define (make-class-constant-pool-entry name)
  (make-node 'class name))

(define (make-method-constant-pool-entry field)
  (let ((field-decl (first (ast-node-param-list field))))
    (list
     (make-node 'method 0
		(get-ast-node-attribute field 'name-entry)
		(get-ast-node-attribute field 'type-entry)
		field
		1))))

;; Expansion functions - each is responsible for one node type

(define (class-expand-constant-pool-entry ast)
  "Expand a class into a set of constant pool entries"
  (let* ((name (get-ast-node-attribute ast 'name))
	 (base-name (get-ast-node-attribute ast 'base-name))
	 (name-entry (make-name-constant-pool-entry name))
	 (base-name-entry (make-name-constant-pool-entry base-name))
	 (class-entry (make-class-constant-pool-entry name-entry))
	 (base-class-entry (make-class-constant-pool-entry base-name-entry)))
    (set-ast-node-attribute! ast 'class-entry class-entry)
    (if base-name
	(progn
	  (set-ast-node-attribute! ast 'base-class-entry class-entry)
	  (list name-entry class-entry base-name-entry base-class-entry))
      (list name-entry class-entry))))

(set-node-type-action 'class-decl        'expand
		      #'class-expand-constant-pool-entry)

(define (typed-expand-constant-pool-entry ast)
  "Expand a paramater, variable, or function constant pool entry"
  (let* ((type-modifiers (get-ast-node-attribute ast 'type-modifiers))
	 (fn-local (or (member 'local type-modifiers)
		       (member 'paramater type-modifiers)))
	 (name-entry (make-name-constant-pool-entry
		      (get-ast-node-attribute ast 'name)))
	 (type (get-ast-node-attribute ast 'type))
	 (type-name-entry
	  (make-name-constant-pool-entry
	   (make-class-file-type-signature type)))
	 (type-entry (make-type-constant-pool-entry type-name-entry))
	 (name-and-type-entry (make-name-and-type-constant-pool-entry
			       name-entry type-entry))
	 (ref-entry (make-ref-entry ast name-and-type-entry)))
    (set-ast-node-attribute! ast 'name-entry name-entry)
    (set-ast-node-attribute! ast 'type-entry type-entry)
    (if (not fn-local)
	(progn
	  (set-ast-node-attribute! ast 'name-and-type-entry name-and-type-entry)
	  (list name-entry type-name-entry type-entry name-and-type-entry
		ref-entry))
      (list name-entry type-name-entry type-entry))))

(set-node-type-action 'fn-decl           'expand
		      #'typed-expand-constant-pool-entry)
(set-node-type-action 'var-decl          'expand
		      #'typed-expand-constant-pool-entry)
(set-node-type-action 'paramater          'expand
		      #'typed-expand-constant-pool-entry)

(define (interfaces-expand-constant-pool-entry ast)
  "Expand the interfaces constant pool entry into a 0 literal"
  (list
   (make-explicit-short-constant-pool-entry 0)))

(set-node-type-action 'interfaces   'expand
		      #'interfaces-expand-constant-pool-entry)

(define (fields-expand-constant-pool-entry ast)
  "Expand a class into a constant pool list of fields"
  (let ((fields
	 (apply 'append
		(mapcar #'(lambda (x)
			    (if (equal 'var-decl (ast-node-type x))
				(make-field-constant-pool-entry x)))
			(get-ast-node-attribute (first (ast-node-param-list ast))
					    'symbols)))))
    (append (list
	     (make-explicit-short-constant-pool-entry
	      (length fields)))
	    fields)))

(set-node-type-action 'fields   'expand
		      #'fields-expand-constant-pool-entry)

(define (methods-expand-constant-pool-entry ast)
  "Expand a class into a constant pool list of methodss"
  (let ((methods
	 (apply 'append
		(mapcar #'(lambda (x)
			    (if (equal 'fn-decl (ast-node-type x))
				(make-method-constant-pool-entry x)))
			(get-ast-node-attribute (first (ast-node-param-list ast))
					    'symbols)))))
    (append
     (list (make-explicit-short-constant-pool-entry (length methods)))
     methods)))

(set-node-type-action 'methods   'expand
		      #'methods-expand-constant-pool-entry)

(define (attributes-expand-constant-pool-entry ast)
  "Expand the attributes constant pool entry into a 0 literal"
  (list
   (make-explicit-short-constant-pool-entry 0)))

(set-node-type-action 'attributes   'expand
		      #'attributes-expand-constant-pool-entry)

(define (class-info-expand-constant-pool-entry ast)
  "Expand the class info constant pool node into pointers to base class
   and class nodes"
  (list
   (make-explicit-short-constant-pool-entry 0)
   (make-explicit-reference-constant-pool-entry
    (get-ast-node-attribute (first (ast-node-param-list ast)) 'class-entry))
   (make-explicit-reference-constant-pool-entry
    (get-ast-node-attribute (first (ast-node-param-list ast)) 'base-class-entry))))

(set-node-type-action 'class-info   'expand
		      #'class-info-expand-constant-pool-entry)

(define (null-expand-constant-pool-entry ast)
  "Do nothing to a constant pool list"
  (list ast))

(set-node-type-action 'octal-literal     'expand
		      #'null-expand-constant-pool-entry)
(set-node-type-action 'character-literal 'expand
		      #'null-expand-constant-pool-entry)
(set-node-type-action 'integer-literal   'expand
		      #'null-expand-constant-pool-entry)

(define (expand-constant-pool-entry entry)
  "Expand an arbitrary constant pool entry"
  (traverse-ast entry 'expand))

(define (expand-constant-pool-entries linear-symtab)
  "Expand every constant pool entry in a list"
  (apply 'append (mapcar #'expand-constant-pool-entry linear-symtab)))

;;; Offset assignment traversal
;
; This runs down the list and assigns an offset to each node

(define (assign-constant-pool-offsets cpool-ast)
  "Assign sequential offsets to constant pool entries"
  (let ((offset 0))
    (mapc #'(lambda (x)
	      (set-ast-node-attribute! x 'offset (incf offset)))
	  cpool-ast)))

;;; Symbol table assembler
;
; generates byte sequences for each element in the constant pool

(define (field-assemble-constant-pool-entry ast)
  "Generate a byte sequence for a field constant pool entry"
  (let ((params (ast-node-param-list ast)))
    (set-ast-node-attribute! ast 'bytes
			(concatenate-bqs
			 (make-bytes-from-short (first params))
			 (make-bytes-from-short
			  (get-entry-offset (second params)))
			 (make-bytes-from-short
			  (get-entry-offset (third params)))
			 (make-bytes-from-short (fourth params))))))

(set-node-type-action 'field    'assemble
		      #'field-assemble-constant-pool-entry)


(define (method-assemble-constant-pool-entry ast)
  "Generate a byte sequence for a method constant pool entry"
  (let ((params (ast-node-param-list ast)))
    (set-ast-node-attribute! ast 'bytes
			(concatenate-bqs
			 (make-bytes-from-short (first params))
			 (make-bytes-from-short
			  (get-entry-offset (second params)))
			 (make-bytes-from-short
			  (get-entry-offset (third params)))
			 (make-bytes-from-short 1)
			 (assemble-bytecode
			  (get-ast-node-attribute (fourth params) 'bytecode))))))


(set-node-type-action 'method    'assemble
		      #'method-assemble-constant-pool-entry)

(define (x-short-assemble-constant-pool-entry ast)
  "Generate a byte sequence for an explicit short"
  (set-ast-node-attribute! ast 'bytes
		      (make-bytes-from-short
		       (first (ast-node-param-list ast)))))

(set-node-type-action 'explicit-short     'assemble
		      #'x-short-assemble-constant-pool-entry)

(define (x-ref-assemble-constant-pool-entry ast)
  "Generate a byte sequence for an explicit reference"
  (let ((ref (first (ast-node-param-list ast))))
    (set-ast-node-attribute! ast 'bytes
			(make-bytes-from-short
			 (if (null ref) 0
			   (get-ast-node-attribute ref 'offset))))))

(set-node-type-action 'explicit-reference     'assemble
		      #'x-ref-assemble-constant-pool-entry)

(define (integral-assemble-constant-pool-entry ast)
  "Generate a byte sequence for a string constant pool entry"
  (set-ast-node-attribute! ast 'bytes
		      (concatenate-bqs
		       (make-bq 3)
		       (pad-bq
			(make-bytes-from-fixnum
			 (make-fixnum-from-string
			  (get-ast-node-attribute ast 'name)
			  :base (case (ast-node-type ast)
				  ((octal-literal) 8)
				  ((integer-literal) 10)
				  ((hex-literal) 16))))
			4))))

(set-node-type-action 'octal-literal      'assemble
		      #'integral-assemble-constant-pool-entry)
(set-node-type-action 'integer-literal     'assemble
		      #'integral-assemble-constant-pool-entry)
(set-node-type-action 'hex-literal     'assemble
		      #'integral-assemble-constant-pool-entry)

(define (character-assemble-constant-pool-entry ast)
  "Generate a byte sequence for a string constant pool entry"
  (set-ast-node-attribute! ast 'bytes
		      (concatenate-bqs
		       (make-bq 3)
		       (pad-bq
			(make-bytes-from-char
			  (char (get-ast-node-attribute ast 'name) 1))
			4))))

(set-node-type-action 'character-literal    'assemble
		      #'character-assemble-constant-pool-entry)

(define (string-assemble-constant-pool-entry ast)
  "Generate a byte sequence for a string constant pool entry"
  (let ((string-val (get-ast-node-attribute ast 'name)))
    (set-ast-node-attribute! ast 'bytes
			(concatenate-bqs
			 (make-bq 8)
			 (make-bytes-from-short (length string-val))
			 (make-bytes-from-string string-val)))))


(set-node-type-action 'string-literal     'assemble
		      #'string-assemble-constant-pool-entry)


(define (class-assemble-constant-pool-entry ast)
  "Generate a byte sequence for a class constant pool entry"
  (set-ast-node-attribute! ast 'bytes
		      (concatenate-bqs
		       (make-bq 7)
		       (make-bytes-from-short
			(get-entry-offset (first (ast-node-param-list ast)))))))

(set-node-type-action 'class     'assemble
		      #'class-assemble-constant-pool-entry)


(define (get-entry-offset ref)
  (get-ast-node-attribute ref 'offset))

(define (type-assemble-constant-pool-entry ast)
  "Generate a byte sequence for a type constant pool entry"
  (set-ast-node-attribute! ast 'bytes
		      (concatenate-bqs
		       (make-bq 7)
		       (make-bytes-from-short
			(get-entry-offset (first (ast-node-param-list ast)))))))

(set-node-type-action 'type     'assemble
		      #'type-assemble-constant-pool-entry)

(define (n-t-assemble-constant-pool-entry ast)
  "Generate a byte sequence for a class constant pool entry"
  (let ((params (ast-node-param-list ast)))
    (set-ast-node-attribute! ast 'bytes
			(concatenate-bqs
			 (make-bq 12)
			 (make-bytes-from-short
			  (get-entry-offset (first params)))
			 (make-bytes-from-short
			  (get-entry-offset (second params)))))))

(set-node-type-action 'name-and-type     'assemble
		      #'n-t-assemble-constant-pool-entry)

; TODO: fix to patch refs

(define (ref-assemble-constant-pool-entry ast)
  "Generate a byte sequence for a ref constant pool entry"
  (let ((params (ast-node-param-list ast)))
    (set-ast-node-attribute! ast 'bytes
			(concatenate-bqs
			 (make-bq 'ref)
			 (make-bytes-from-short
			  (get-entry-offset (first params)))
			 (make-bytes-from-short
			  (get-entry-offset (second params)))))))

(set-node-type-action 'ref     'assemble
		      #'ref-assemble-constant-pool-entry)

(define (extract-class-bytes ast)
  (mapcar #'(lambda (x) (get-ast-node-attribute x 'bytes))
	  ast))

(define (assemble-constant-pool-entry entry)
  "Expand an arbitrary constant pool entry"
  (traverse-ast entry 'assemble))

(define (assemble-constant-pool constant-pool)
  "Expand every constant pool entry in a list"
  (mapcar #'assemble-constant-pool-entry constant-pool))

(define (flatten xs)
  xs)

(define (generate-class-byte-stream ast)
  (flatten
   (cons
    (get-ast-node-attribute ast 'name)
    (extract-class-bytes
     (assemble-constant-pool
      (assign-constant-pool-offsets
       (expand-constant-pool-entries
	(generate-linear-symbol-table ast))))))))

;;;; The toplevel compilation code for the compiler

;;; Code to extract a list of defined classes

(define (get-class-list ast)
  (get-ast-node-attribute ast 'symbols))

;;; The external compiler interface function

(define (compile-java-file filename)
  (mapcar #'(lambda (x)
	    (format t "Compiling class: ~a~%" (get-ast-node-attribute x 'name))
	    (print (generate-class-byte-stream x)))
	(get-class-list
	 (generate-intermediate-code (analyze-file filename)))))