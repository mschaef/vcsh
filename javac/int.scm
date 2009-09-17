;;;; int.lisp - The code generator  for the CS375 java->classfile
;;;; compiler.
;;;;
;;;; Michael Schaeffer (CS375, Hartmann)

;;;; Modifications
; rev 1 - Initial Release
;
; rev 2 - Final 375 Release
;  * Moved startup load code to jcompile.lisp
;  * Moced test case code to jcompile.lisp
;  * Renamed flatten-ast to generate-intermediate-code
;  * Modified constant load and store op codes to point to const
;  * Switched to ast node representation of instructions

;;;; An error handler for code generation errors
;
; By the time the code gets this far, it should have passed
; muster and be valid, compilable code. Any errors thrown by the
; code generator will be internal errors by default.

(define (int-error error-message &rest paramaters)
  "Function call to issue a syntactic error"
  (apply 'compiler-error
	 (nconc (list 'code-gen 'unknown
		      (string-append "Internal Error: " error-message))
		paramaters)))

;;;; Symbol table consolidation code
;
; The analysis half of the compiler generates an ast with a
; seperate symbol table for each scope, including scopes within
; a function.  What we're doing here is consolodating the symbol
; table so that there is only one symbol table per function. The
; uniqueness of variables will be preserved because each instance
; of a variable name has been assigned a unique number.  The int
; phase can then use these unique numbers to generate variable
; reference code.

(define (toplevel-consolidate-symtabs ast)
  "Concolidate the symbol tables contained in a file or classast"
  (mapcar #'consolidate-symtabs-ast
	  (get-ast-node-attribute ast 'symbols))
  ast)

(set-node-type-action 'file 'consolidate-symtabs
		      #'toplevel-consolidate-symtabs)
(set-node-type-action 'class-decl 'consolidate-symtabs
		      #'toplevel-consolidate-symtabs)


(define (null-consolidate-symtabs ast)
  ast
  )

(mapc (make-action-set-fn 'consolidate-symtabs #'null-consolidate-symtabs)
      '(variable-ref nothing var-decl assign plus minus star slash
	logical-or logical-and not-equal funcall array-ref
	bitwise-or bitwise-and bitwise-xor bitwise-and
	shift-right shift-left unsigned-shift-right
	less greater less-or-equal greater-or-equal equal
	plus-assign minus-assign star-assign slash-assign
	and-assign or-assign xor-assign percent-assign
	shl-assign shr-assign ushr-assign return
	integer-literal octal-literal numeric-literal
	character-literal))

(define (get-symtab ast)
  (get-ast-node-attribute ast 'symbols))

(define (set-symtab ast sym-list)
  (set-ast-node-attribute! ast 'symbols sym-list))

(define (extend-symtab ast sym-list)
  (set-symtab ast (append sym-list (get-symtab ast))))

(define (delete-symtab ast)
  (remove-ast-node-attribute! ast 'symbols))

(define (fn-consolidate-symtabs ast)
  "Normalize a function definition"
  (let ((code-block (get-ast-node-attribute ast 'code)))
    (consolidate-symtabs-ast code-block))
  ast)

(set-node-type-action 'fn-decl 'consolidate-symtabs
		      #'fn-consolidate-symtabs)

(define (compound-consolidate-symtabs ast)
  (mapc #'consolidate-symtabs-ast
	(ast-node-param-list ast))
  (extend-symtab (get-ast-node-attribute ast 'parent)
		 (get-symtab ast))
  (delete-symtab ast)
  ast)

(mapc (make-action-set-fn 'consolidate-symtabs #'compound-consolidate-symtabs)
      '(if for while block))

(define (consolidate-symtabs-ast ast)
  "Normalize an arbitrary ast"
  (if (not (null ast))
      (traverse-ast ast 'consolidate-symtabs)))

;;;; Code generation code
;
; Here is the code generator for the compiler.

(defmacro (make-op opcode . paramaters)
  `(list (make-node ,opcode ,@paramaters)))

(define (file-generate-code ast)
  "Generate code for each class in an ast"
  (mapc #'generate-code-ast
	  (get-ast-node-attribute ast 'symbols))
  ast)

(set-node-type-action 'file 'generate-code-ast #'file-generate-code)

(define (class-generate-code ast)
  "Generate code for each function in a class"
  (mapc #'(lambda (x)
	      (if (eq (ast-node-type x) 'fn-decl)
		  (generate-code-ast x)))
	  (get-ast-node-attribute ast 'symbols))
  ast)

(set-node-type-action 'class-decl 'generate-code-ast
		      #'class-generate-code)

(define (fn-generate-code ast)
  "Generate code for a function"
  (let ((code-block (get-ast-node-attribute ast 'code)))
    (set-ast-node-attribute! ast 'bytecode (generate-code code-block)))
  (remove-ast-node-attribute! ast 'code)
  ast)

(set-node-type-action 'fn-decl 'generate-code-ast
		      #'fn-generate-code)

(define (block-generate-code ast)
  (mapcan #'generate-code (ast-node-param-list ast)))

(set-node-type-action 'block 'generate-code
		      #'block-generate-code)

;;; Code generation for Expressions

;; Assignment

(define (assign-generate-code ast)
  (let ((params-list (ast-node-param-list ast)))
    (append
     (generate-code (second params-list))
     (generate-code (first params-list)))))

(set-node-type-action 'assign 'generate-code
		      #'assign-generate-code)

;; Basic arithmatic operations

(define (arith-binop-generate-code ast)
  (let ((params-list (ast-node-param-list ast)))
    (append
     (generate-code (first params-list))
     (generate-code (second params-list))
     (make-op (ast-node-type ast) (get-ast-node-attribute ast 'type)))))

(mapc (make-action-set-fn 'generate-code #'arith-binop-generate-code)
      '(plus minus star slash percent bitwise-and bitwise-or bitwise-xor
	     shift-right shift-left unsigned-shift-right))

;; Negation

(define (negate-generate-code ast)
  (let ((params-list (ast-node-param-list ast)))
    (append
     (generate-code (first params-list))
     (make-op 'negate (get-ast-node-attribute ast 'type)))))

(mapc (make-action-set-fn 'generate-code #'negate-generate-code)
      '(arithmatic-negate bitwise-negate))

;;; Code generation for literals

(define (literal-generate-code ast)
  (make-op 'constant
	   (get-ast-node-attribute ast 'ref)))

(mapc (make-action-set-fn 'generate-code #'literal-generate-code)
      '(numeric-literal octal-literal character-literal character-literal
	hex-literal integer-literal))

;;; Code generation for variable references

;; Type Promotion

(define (promote-generate-code ast)
  (let* ((source (first (ast-node-param-list ast)))
	 (destination-type (get-ast-node-attribute ast 'type))
	 (source-type (get-ast-node-attribute source 'type)))
    (append
     (generate-code source)
     (make-op 'coerce source-type destination-type))))

(set-node-type-action 'promote 'generate-code
		      #'promote-generate-code)

;; Variable References

(define (varref-generate-code ast)
  (let ((succ (second (ast-node-param-list ast)))
	(ref (get-ast-node-attribute ast 'ref))
	(depth (get-ast-node-attribute ast 'ref-depth))
	(usage (get-ast-node-attribute ast 'usage)))
    (append
     (let ((attrs (get-ast-node-attribute ref 'type-modifiers)))
       (cond ((or (member 'local attrs)
		  (member 'paramater attrs))
	      (case usage
		(al-value (make-op 'store (get-ast-node-attribute ref 'type) ref))
		(r-value (make-op 'load (get-ast-node-attribute ref 'type) ref))
		(else (int-error "Invalid variable-reference"))))
	     (t (let ((static (member 'static (get-ast-node-attribute ref 'type-modifiers))))
		  (append
		   (if (= depth 0)
		       (make-op 'load 'this))
		   (case usage
		     (r-value
		      (make-op (if static 'get-static 'get-field)
			       (get-ast-node-attribute ref 'type) ref))
		     (al-value
		      (make-op (if static 'put-static 'put-field)
			       (get-ast-node-attribute ref 'type) ref))
		     (else (int-error "Invalid variable reference"))))))))
     (if (not (null succ))
	 (generate-code succ)))))

(set-node-type-action 'variable-ref 'generate-code
		      #'varref-generate-code)

(define (funcall-generate-code ast)
  (let ((params-list (ast-node-param-list ast))
	(ref (get-ast-node-attribute ast 'ref)))
    (append
     (if (= 0 (get-ast-node-attribute ast 'ref-depth))
	 (make-op 'load 'this))
     (mapcan #'generate-code (second params-list))
     (make-op
      (if (member 'static (get-ast-node-attribute ref 'type-modifiers))
	  'invoke-static
	'invoke-virtual) ref ))))

(set-node-type-action 'funcall 'generate-code
		      #'funcall-generate-code)

(define (return-generate-code ast)
  (let ((param (first (ast-node-param-list ast))))
    (append
     (generate-code param)
     (make-op 'return (get-ast-node-attribute param 'type)))))

(set-node-type-action 'return 'generate-code
		      #'return-generate-code)

;;; Code generation for logical operations

(define (logical-and-generate-code ast)
  (let* ((params-list (ast-node-param-list ast))
	 (lhs-code (generate-code (first params-list)))
	 (rhs-code (generate-code (second params-list)))
	 (false-result (append
			(make-op 'constant 0)
			(make-op 'nop)))
	 (true-result (append
		       (make-op 'constant 1)
		       (make-op 'goto (second false-result)))))
    (append
     lhs-code
     (make-op 'constant 0)
     (make-op 'if-integer-equal (first false-result))
     rhs-code
     (make-op 'constant 0)
     (make-op 'if-integer-equal (first false-result))
     true-result
     false-result)))

(set-node-type-action 'logical-and 'generate-code #'logical-and-generate-code)

(define (logical-or-generate-code ast)
  (let* ((params-list (ast-node-param-list ast))
	 (lhs-code (generate-code (first params-list)))
	 (rhs-code (generate-code (second params-list)))
	 (true-result (append
		       (make-op 'constant 1)
		       (make-op 'nop)))
	 (false-result (append
			(make-op 'constant 0)
			(make-op 'goto (second true-result)))))
    (append
     lhs-code
     (make-op 'constant 1)
     (make-op 'if-integer-equal (first true-result))
     rhs-code
     (make-op 'constant 1)
     (make-op 'if-integer-equal (first true-result))
     false-result
     true-result)))

(set-node-type-action 'logical-or 'generate-code #'logical-or-generate-code)

(define (logical-negate-generate-code ast)
  (let* ((params-list (ast-node-param-list ast))
	 (op-code (generate-code (first params-list)))
	 (true-result (append
		       (make-op 'constant 1)
		       (make-op 'nop)))
	 (false-result (append
			(make-op 'constant 0)
			(make-op 'goto (second true-result)))))
    (append
     op-code
     (make-op 'constant 1)
     (make-op 'if-integer-equal (first true-result))
     false-result
     true-result)))

(set-node-type-action 'logical-or 'generate-code #'logical-negate-generate-code)


;; Comparison operations

(define (numeric-compare-generate-code ast)
  (let* ((params-list (ast-node-param-list ast))
	 (lhs-code (generate-code (first params-list)))
	 (rhs-code (generate-code (second params-list)))
	 (compare-type (ast-node-type ast))
	 (type (get-ast-node-attribute (first params-list) 'type))
	 (true-result (append
		       (make-op 'constant 1)
		       (make-op 'nop)))
	 (true-entry (first true-result))
	 (false-result (append
			(make-op 'constant 0)
			(make-op 'goto (second true-result))))
	 (compare-op (if (eq 'int type)
			 (case compare-type
			   (less (make-op 'if-integer-less true-entry))
			   (greater (make-op 'if-integer-greater true-entry))
			   (less-or-equal (make-op 'if-integer-less-or-equal true-entry))
			   (greater-or-equal (make-op 'if-integer-greater-or-equal
						      true-entry))
			   (equal (make-op 'if-integer-equal true-entry))
			   (not-equal (make-op 'if-integer-not-equal true-entry))
			   (otherwise (int-error "Invalid comparison class")))
		       (append
			(make-op 'compare type)
			(case compare-type
			  (less (make-op 'jump-if-less true-entry))
			  (greater (make-op 'jump-if-greater true-entry))
			  (less-or-equal (make-op 'jump-if-less-or-equal true-entry))
			  (greater-or-equal (make-op 'jump-if-greater-or-equal true-entry))
			  (equal (make-op 'jump-if-equal true-entry))
			  (not-equal (make-op 'jump-if-not-equal true-entry))
			  (otherwise (int-error "Invalid comparison class")))))))

    (append
     lhs-code
     rhs-code
     compare-op
     false-result
     true-result)))

(mapc (make-action-set-fn 'generate-code #'numeric-compare-generate-code)
      '(less greater less-or-equal greater-or-equal equal not-equal))


;;; Code generation for if statements

(define (if-generate-code ast)
  (let* ((params-list (ast-node-param-list ast))
	 (condition-code (generate-code (first params-list)))
	 (then-code (generate-code (second params-list)))
	 (else-code (if (not (null (third params-list)))
			(generate-code (third params-list))))
	 (end-code (make-op 'nop)))
    (if (null else-code)
	(append
	 condition-code
	 (make-op 'constant 0)
	 (make-op 'if-integer-equal (first end-code))
	 then-code
	 end-code)
      (append
       condition-code
       (make-op 'constant 0)
       (make-op 'if-integer-equal (first else-code))
       then-code
       (make-op 'goto (first end-code))
       else-code
       end-code))))

(set-node-type-action 'if 'generate-code #'if-generate-code)

;;; Code generation for while statements

(define (while-generate-code ast)
  (let* ((params-list (ast-node-param-list ast))
	 (condition-code (generate-code (first params-list)))
	 (block-code (generate-code (second params-list))))
    (append
     (make-op 'goto (first condition-code))
     block-code
     condition-code
     (make-op 'constant 1)
     (make-op 'if-integer-equal (first block-code)))))

(set-node-type-action 'while 'generate-code #'while-generate-code)

;;; Code generation for for statements

(define (for-generate-code ast)
  (let* ((params-list (ast-node-param-list ast))
	 (init-code (generate-code (first params-list)))
	 (condition-code (generate-code (second params-list)))
	 (incr-code (generate-code (third params-list)))
	 (block-code (generate-code (fourth params-list))))
    (append
     init-code
     (make-op 'goto (first condition-code))
     block-code
     incr-code
     condition-code
     (make-op 'constant 1)
     (make-op 'if-integer-equal (first block-code)))))


(set-node-type-action 'for 'generate-code #'for-generate-code)

;;; Code generation for null code fragments

(define (null-generate-code ast)
  '())

(mapc (make-action-set-fn 'generate-code #'null-generate-code)
      '(nothing var-decl ))

(define (generate-code-ast ast)
  "Generate a code ast"
  (traverse-ast ast 'generate-code-ast))

(define (generate-code ast)
  "Generate code for a code ast node"
  (traverse-ast ast 'generate-code))

(define (generate-intermediate-code ast)
  "Given a file's processed ast, generate an intermediate code ast"
  (generate-code-ast (consolidate-symtabs-ast ast)))
