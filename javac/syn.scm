
;;;; syn.lisp - The syntactic analyzer for the CS375 java->classfile
;;;; compiler.
;;;;
;;;; Michael Schaeffer (CS375, Hartmann)

;;;; Modifications
;; rev 1 - Initial Release
;;
;; rev 2 - Improvements for sem release
;;  * Implemented code to support compound names for function return types
;;  * Fixed bug regarding inclusion of line numbers in ast
;;  * Fixed numerous list nesting errors
;;  * Simplified list structure
;;  * Eliminated support for compound statement as expr. term
;;  * Rewrote code for parsing name instances
;;  * Rewrote code for parsing declarations
;;  * Added support for class inheritance (extends only)
;;  * Added support for integer/float tokens
;;
;; rev 3 - Final 375 Release
;;  * Moved ast node code into utility.lisp

;; TODO: look into right assoc. ops (i = i << x<y?1:0; fails)
;; TODO: Extend lex to distinguish between integer/real
;; TODO: Implement post increment/decrement
;; TODO: Implement macro for left assoc. operator parsing
;; TODO: Implement generic 'list' parse routine


;;;; Code for manipulating Abstract Syntax Trees
;;
;; The basic format for an AST node is as follows:
;; (node-type attributes . params)
;;
;; node-type - A symbol defining the syntax element represented
;; attributes - An a-list containing attributes of the node
;; params - A list containing zero or more elements

(define-structure ast-node
  type
  (attributes :default ())
  (params :default ()))

(define (make-node node-type . params)
  "Contruct a new AST node"
  (make-aggregate-node node-type params))

(define (make-aggregate-node node-type params)
  (make-ast-node :type node-type
                 :params params))

;; Accessor functions for AST nodes

(define (get-ast-node-attribute node attribute)
  (aif (assoc attribute (ast-node-attributes node))
       (cdr it)
       #f))

(define (set-ast-node-attribute! node attribute value)
  "Set an attribute in an AST node"
  (let ((assoc-node (assoc attribute (ast-node-attributes node))))
    (if assoc-node
        (set-cdr! assoc-node value)
        (set-ast-node-attributes! node (cons (cons attribute value)
                                             (ast-node-attributes node))))
    node))

(define (remove-ast-node-attribute! node attribute)
  "Remove a node attribute"
  (set-ast-node-attributes! node (remove #L(eq? attribute (car  _))
                                         (ast-node-attributes node))))

(define (attribute-equal? node attribute expected-value)
  "Determine if an attribute has a specific value"
  (equal? (get-node-attribute node attribute) expected-value))


;;; An error handler for syntactic errors

;; Error Call

(define (syn-error error-message error-token . paramaters)
  "Function call to issue a syntactic error"
  (apply compiler-error 'syntax (token-line error-token) error-message))

;;;; Functions for maipulating token lists
;;
;; Note: here we have some uniquely bad coding style.  By using a global
;; variable we make the parser dependant on side-effects, and restrict it
;; to one parse session at a time.  However, seeing as how this maps to the
;; file paradigm used in the lexer, and in other compilers, it seems like
;; it should be a worthwhile tradeoff.
;;
;; One other approach would be to define token stream as a class, and
;; use it like a standard Lisp stream variable.  I feel that this is overkill
;; for thie application

(define *token-list* ()
  "A list containing the tokens contained in the source file")

(define (peek-token)
  "Peek at the next token"
  (first *token-list*))

(define (peek-second-token)
  "Peek at the second token"
  (second *token-list*))

(define (next-token)
  "Advance to the next token in the input stream"
  (set! *token-list* (rest *token-list*))
  (peek-token))

(define (check-token . token-types)
  "Check the next token against specific token types"
  (member (token-class (peek-token)) token-types))

(define (check-this-token token . token-types)
  "Check the next token against specific token types"
  (member (token-class token) token-types))

(define (check-second-token . token-types)
  "Check the second token against specific token types"
  (member (token-class (peek-second-token)) token-types))

(define (expect-token . token-type)
  "Call when the next element of the syntax is fixed"
  (if (apply check-token token-type)
      (next-token)
      (syn-error "Expected one of ~a, found ~a"
                 (peek-token)
                 token-type (ast-node-type (peek-token)))))

;;; Here we go! Actual parsing routines that do somethingt interesting

;; Some basic control structures

(define (parse-if)
  "Parse a java if statement and return an AST node"
  (let (conditional if-clause else-clause)
    (expect-token 'if)
    (expect-token 'open-paren)
    (set! conditional (parse-sentence))
    (expect-token 'close-paren)
    (set! if-clause (parse-complete-statement))
    (set! else-clause
          (if (check-token 'else)
              (begin
               (next-token)
               (parse-complete-statement))))
    (make-node 'if conditional if-clause else-clause)))

(define (parse-while)
  "Parse a java while statement and return an AST node"
  (let (conditional loop-body)
    (expect-token 'while)
    (expect-token 'open-paren)
    (set! conditional (parse-sentence))
    (expect-token 'close-paren)
    (set! loop-body (parse-complete-statement))
    (make-node 'while conditional loop-body)))

(define (parse-for)
  "Parse a java for statement and return an AST node"
  (let (loop-init loop-test loop-incr loop-body)
    (expect-token 'for)
    (expect-token 'open-paren)
    (set! loop-init (parse-sentence))
    (expect-token 'semi)
    (set! loop-test (parse-sentence))
    (expect-token 'semi)
    (set! loop-incr (parse-sentence))
    (expect-token 'close-paren)
    (set! loop-body (parse-complete-statement))
    (make-node 'for loop-init loop-test loop-incr loop-body)))

(define (parse-do)
  "Parse a java do statement and return an AST node"
  (let (conditional loop-body)
    (expect-token 'do)
    (set! loop-body (parse-complete-statement))
    (expect-token 'while)
    (expect-token 'open-paren)
    (set! conditional (parse-sentence))
    (expect-token 'close-paren)
    (make-node 'do loop-body conditional)))

(define (parse-switch)
  "Parse a java switch statement and return an AST node"
  (let (conditional switch-body)
    (expect-token 'switch)
    (expect-token 'open-paren)
    (set! conditional (parse-sentence))
    (expect-token 'close-paren)
    (expect-token 'open-brace)
    (while (check-token 'case 'default)
      (set! switch-body (nconc switch-body (list (parse-case)))))
    (expect-token 'close-brace)
    (make-node 'switch conditional switch-body)))

(define (parse-case)
  "Parse a java case (a part of a switch) statement and return an AST node"
  (let (condition case-body)
    (cond ((check-token 'default)
           (next-token)
           (set! condition 'default))
          ((check-token 'case)
           (next-token)
           (set! condition (parse-atomic-literal)))
          (#t (syn-error "Internal compiler error parsing switch")))
    (expect-token 'colon)
    (while (not (check-token 'case 'default 'close-brace))
      (set! case-body (nconc case-body (list (parse-single-statement))))
                                        ; TODO: fix semi handling here
      (expect-token 'semi))
    (make-aggregate-node 'case (cons condition case-body))))

(define (parse-synchronized)
  "Parse a java synchronized statement and return an AST node"
  (let (lockee synch-body)
    (expect-token 'synchronized)
    (expect-token 'open-paren)
    (set! lockee (parse-sentence))
    (expect-token 'close-paren)
    (set! synch-body (parse-complete-statement))
    (make-node 'synch lockee synch-body)))

;; Parsing code for exceptions blocks

(define (parse-try-block)
  "Parse a java try block and return an AST node"
  (expect-token 'try)
  (make-node 'try (parse-complete-statement)))

(define (parse-catch-block)
  "Parse a java catch block and return an AST node"
  (expect-token 'catch)
  (make-node 'catch
             (parse-typed-paramater-list)
             (parse-complete-statement)))

(define (parse-finally-block)
  "Parse a java finally block and return an AST node"
  (expect-token 'finally)
  (make-node 'finally (parse-complete-statement)))

(define (parse-exception-block)
  "Parse a java try/catch/finally block and return an AST node"
  (let (try-block catch-blocks)
    (set! try-block (parse-try-block))
    (while (check-token 'catch)
      (set! catch-blocks (nconc catch-blocks (list (parse-catch-block)))))
    (if (check-token 'finally)
        (set! catch-blocks (nconc catch-blocks (list (parse-finally-block)))))
    (make-node 'exception-handler try-block catch-blocks)))

;; Parsing code for statements

(define (parse-complete-statement)
  "Parse a complete, terminated java statement"
  (cond ((check-token 'open-brace) (parse-compound-statement))
        ((check-token 'if) (parse-if))
        ((check-token 'for) (parse-for))
        ((check-token 'while)  (parse-while))
        ((check-token 'synchronized)  (parse-synchronized))
        ((check-token 'try) (parse-exception-block))
        (#t (let ((ast (parse-simple-statement)))
             (expect-token 'semi)
             ast))))

(define (parse-compound-statement)
  "Parse a statement block"
  (expect-token 'open-brace)
  (let (block-body)
    (while (not (check-token 'close-brace))
      (set! block-body (nconc block-body (list (parse-complete-statement)))))
    (expect-token 'close-brace)
    (make-aggregate-node 'block block-body)))

(define (parse-break)
  "Parse a java break statement and return an AST node"
  (expect-token 'break)
  (make-node 'break))

(define (parse-return)
  "Parse a java return statement and return an AST node"
  (expect-token 'return)
  (make-node 'return (parse-operation)))

(define (parse-simple-statement)
  "Parse a single-line statement and return an AST node"
  (cond
   ((check-token 'do)     (parse-do))
   ((check-token 'switch) (parse-switch))
   ((check-token 'return) (parse-return))
   ((check-token 'break)  (parse-break))
   ((check-token 'semi)   (make-node 'nothing))
   (#t (parse-sentence))))

;; Parsing code for names

(define (parse-simple-name)
  "Parse a dotted name and return an AST node"
  (let ((name-body (list (parse-id))))
    (while  ; loop until we hit something we don't like
        (cond ((check-token 'dot)
               (next-token)
               (cond ((check-token 'id)
                      (push! (parse-id) name-body))
                     ((check-token 'star)
                      (push! (list (make-node 'arbitrary-id)) name-body))
                     (#t nil))
               (#t nil))))
    (make-aggregate-node 'name name-body)))

(define (parse-name)
  "Parse a dotted name, including aref and p lists and return an AST node"
  (let ((name-body (read-id)))
    (cond ((check-token 'dot)
           (next-token)
           (make-node 'variable-ref name-body (parse-name)))
          ((check-token 'open-bracket)
           (let ((aref-list (parse-aref-list)))
             (if (check-token 'id)
                 (make-node 'array-ref name-body aref-list (parse-name))
                 (make-node 'array-ref name-body aref-list))))
          ((check-token 'open-paren)
           (make-node 'funcall name-body (parse-paramater-list)))
          (#t
           (make-node 'variable-ref name-body)))))

(define (parse-paramater-list)
  "Parse an untyped paramater list"
  (expect-token 'open-paren)
  (let (param-list)
    (while (not (check-token 'close-paren))
      (set! param-list (nconc param-list (list (parse-sentence))))
      (cond ((check-token 'comma) (next-token))
            ((check-token 'close-paren))
            (#t (syn-error "Invalid paramater list" (peek-token)))))
    (expect-token 'close-paren)
    param-list))

(define (read-type-prefix :optional prefix)
  "Parse a java type prefiz and return an AST node"
  (if (check-token 'public 'private 'protected 'static 'final 'synchronized)
      (let ((prefix-val (token-class (peek-token))))
        (next-token)
        (read-type-prefix (cons prefix-val prefix)))
      prefix))

(define (parse-type)
  "Parse a java type and return an AST node"
  (let ((type-prefix (read-type-prefix))
        (type (token-class (peek-token)))
        (type-name (list (token-info (peek-token))))
        array-spec)
    (next-token)
    (while (check-token 'dot)
      (push! (token-info (next-token)) type-name)
      (next-token))
    (make-node 'type type type-name type-prefix (read-adim-spec))))

(define (parse-typed-paramater-list)
  "Parse a java typed paramater list and return an AST node"
  (expect-token 'open-paren)
  (let (param-list)
    (while (not (check-token 'close-paren))
      (set! param-list (nconc param-list
                              (list (make-node 'paramater
                                               (parse-type)
                                               (first (ast-node-params (parse-id)))
                                               (read-adim-spec)))))
      (cond ((check-token 'comma) (next-token))
            ((check-token 'close-paren))
            (#t (syn-error "Invalid paramater list" (peek-token)))))
    (expect-token 'close-paren)
    param-list))

(define (parse-aref-list)
  "Parse a java array reference list and return an AST node"
  (let (aref-list)
    (while (check-token 'open-bracket)
      (expect-token 'open-bracket)
      (push! (parse-sentence) aref-list)
      (expect-token 'close-bracket))
    aref-list))

(define (read-adim-spec :optional (length 0))
  (if (check-token 'open-bracket)
      (begin
       (expect-token 'open-bracket) (expect-token 'close-bracket)
       (read-adim-spec (1+ length)))
      length))

(define (read-id)
  "Read a java identifier and return it"
  (let ((symbol-name (token-info (peek-token))))
    (next-token)
    symbol-name))

(define (parse-id)
  "Parse a java identifier and return an AST node"
  (make-node 'identifier (read-id)))

;; Parsing of literals

(define (parse-atomic-literal)
  "Parse a java literal  and return an AST node"
  (let ((token (peek-token)))
    (if (check-token 'character-literal 'hex-literal
                     'octal-literal 'integer-literal 'float-literal)
        (begin
         (next-token)
         (make-node  (token-class token) (token-info token)))
        (syn-error "Atomic literal expected" token))))

(define (parse-literal)
  "Parse a java literal and return an AST node"
  (cond ((check-token 'string-literal)
         (let ((token (peek-token)))
           (next-token)
           (make-node 'string-literal (token-info token))))
        ((check-token 'boolean 'double 'byte 'int 'short 'char)
         (let ((ast (make-node 'primitive-type (token-class (peek-token)))))
           (next-token)
           ast))
        (#t (parse-atomic-literal))))

;; Here's where we start parsing expressions

(define (parse-factor)
  "Parse an ezpression factor and return an AST node"
  (let (lhs)
    (cond ((check-token 'identifier) (parse-name))
          ((check-token 'open-paren)
           (next-token)
           (set! lhs (parse-sentence))
           (expect-token 'close-paren)
           lhs)
          (#t (parse-literal)))))

(define (parse-unary)
  "Parse a java unary op and return an AST node"
  (if (check-token 'increment 'decrement 'minus 'bang 'tilde)
      (let ((token-op (token-class (peek-token))))
        (next-token)
        (case token-op
          ((increment) (make-node 'pre-increment (parse-sentence)))
          ((decrement) (make-node 'pre-decrement (parse-sentence)))
          ((minus) (make-node 'arithmatic-negate (parse-sentence)))
          ((bang) (make-node 'logical-negate (parse-sentence)))
          ((tilde) (make-node 'logical-negate (parse-sentence)))
          (otherwise (parse-factor))))
      (parse-factor)))

(define (parse-term)
  "Parse a java expression term and return an AST node"
  (let ((lhs (parse-unary)) op rhs)
    (if (check-token 'slash 'star)
        (begin
         (set! op (token-class (peek-token)))
         (next-token)
         (set! rhs (parse-term))
         (make-node op lhs rhs))
        lhs)))

(define (parse-expression)
  "Parse a java expression (plus/minus) and return an AST node"
  (let ((lhs (parse-term)) op rhs)
    (if (check-token 'plus 'minus)
        (begin
         (set! op (token-class (peek-token)))
         (next-token)
         (set! rhs (parse-expression))
         (make-node op lhs rhs))
        lhs)))

(define (parse-shift)
  "Parse a java shift op and return an AST node"
  (let ((lhs (parse-expression)) op rhs)
    (if (check-token 'shift-left 'shift-right 'unsigned-shift-right)
        (begin
         (set! op (token-class (peek-token)))
         (next-token)
         (set! rhs (parse-shift))
         (make-node op lhs rhs))
        lhs)))

(define (parse-comparison)
  "Parse a java comparison op and return an AST node"
  (let ((lhs (parse-shift)) op rhs)
    (if (check-token 'less-or-equal 'greater-or-equal
                     'greater 'less 'instanceof)
        (begin
         (set! op (token-class (peek-token)))
         (next-token)
         (set! rhs (parse-comparison))
         (make-node op lhs rhs))
        lhs)))

(define (parse-equality)
  "Parse a java equality op and return an AST node"
  (let ((lhs (parse-comparison)) op rhs)
    (if (check-token 'equal 'not-equal)
        (begin
         (set! op (token-class (peek-token)))
         (next-token)
         (set! rhs (parse-equality))
         (make-node op lhs rhs))
        lhs)))

(define (parse-bitwise-and)
  "Parse a java logical op and return an AST node"
  (let ((lhs (parse-equality)) rhs)
    (if (check-token 'bitwise-and)
        (begin
         (next-token)
         (set! rhs (parse-bitwise-and))
         (make-node 'bitwise-and lhs rhs))
        lhs)))

(define (parse-bitwise-xor)
  "Parse a java logical op and return an AST node"
  (let ((lhs (parse-bitwise-and)) rhs)
    (if (check-token 'bitwise-xor)
        (begin
         (next-token)
         (set! rhs (parse-bitwise-xor))
         (make-node 'bitwise-xor lhs rhs))
        lhs)))

(define (parse-bitwise-or)
  "Parse a java logical op and return an AST node"
  (let ((lhs (parse-bitwise-xor)) rhs)
    (if (check-token 'bitwise-or)
        (begin
         (next-token)
         (set! rhs (parse-bitwise-or))
         (make-node 'bitwise-or lhs rhs))
        lhs)))

(define (parse-conditional-and)
  "Parse a java logical op and return an AST node"
  (let ((lhs (parse-bitwise-or)) rhs)
    (if (check-token 'logical-and)
        (begin
         (next-token)
         (set! rhs (parse-conditional-and))
         (make-node 'logical-and lhs rhs))
        lhs)))

(define (parse-conditional-or)
  "Parse a java logical op and return an AST node"
  (let ((lhs (parse-conditional-and)) rhs)
    (if (check-token 'logical-or)
        (begin
         (next-token)
         (set! rhs (parse-conditional-or))
         (make-node 'logical-or lhs rhs))
        lhs)))


(define (parse-conditional)
  "Parse a java logical op and return an AST node"
  (let ((condition (parse-conditional-or)) then-case else-case)
    (if (check-token 'question)
        (begin
         (next-token)
         (set! then-case (parse-conditional-or))
         (expect-token 'colon)
         (set! else-case (parse-conditional-or))
         (make-node 'conditional condition then-case else-case))
        condition)))

(define (parse-operation)
  "Parse a java logical op and return an AST node"
  (let ((lhs (parse-conditional)) op rhs)
    (cond
     ((check-token 'plus-assign 'minus-assign 'star-assign 'slash-assign
                   'and-assign 'or-assign 'xor-assign 'percent-assign
                   'shl-assign 'shr-assign 'ushr-assign 'assign)
      (set! op (token-class (peek-token)))
      (next-token)
      (set! rhs (parse-operation))
      (make-node op lhs rhs))
     (#t lhs))))

(define (parse-throws-list)
  "Parse a java function throws list and return an AST node"
  (expect-token 'throws)
  (let (throws-list)
    (while (not (check-token 'open-brace))
      ;; TODO: change to parse a type
      (nconc throws-list (list (parse-id)))
      (cond ((check-token 'comma) (next-token))
            ((check-token 'open-brace))
            (#t (syn-error "Invalid throws list" (peek-token)))))
    (make-aggregate-node 'throws throws-list)))


(define (parse-decl :optional type decl-list)
  "Parse a java declaration and return an AST node"
  (if (null? type)
      (parse-decl (parse-type))
      (if (check-token 'identifier)
          (let ((name (token-info (peek-token))) adim-spec)
            (next-token)
            (set! adim-spec (read-adim-spec))
            (cond ((check-token 'open-paren)  ; Start a function declaration
                   (if (null? decl-list)
                       (make-node 'decl-list
                                  (make-node 'fn-decl type name
                                             (parse-typed-paramater-list)
                                             (if (check-token 'throws)
                                                 (parse-throws-list))
                                             (parse-compound-statement)))
                       (syn-error "Function not allowed in compound decl-list"
                                  (peek-token))))
                  ((check-token 'comma)       ; Continue looking for variable decls
                   (next-token)
                   (parse-decl type (cons
                                     (make-node 'var-decl type name adim-spec)
                                     decl-list)))
                  (#t                          ; Wrap up the declaration
                   (make-aggregate-node 'decl-list
                                        (cons
                                         (make-node 'var-decl type name adim-spec)
                                         decl-list)))))
          (syn-error "Malformed declaration" (peek-token)))))

(define (parse-sentence)
  "Parse a java expression and return an AST node"
  (cond ((or
          (check-token 'boolean 'double 'byte 'int 'short 'char 'void
                       'long 'final 'public 'private 'protected 'static
                       'synchronized)
          (and (check-token 'identifier) (check-second-token 'identifier
                                                             'open-bracket)))
         (parse-decl))
        (#t (parse-operation))))

(define (parse-class)
  "Parse a java class declaration and return an AST node"
  (let ((type-prefix (read-type-prefix)))
    (expect-token 'class)
    (let ((class-name (read-id))
          (base-class (if (check-token 'extends)
                          (begin
                           (next-token)
                           (read-id))))
          class-body)
      (expect-token 'open-brace)
      (while (not (check-token 'close-brace))
        (push! (parse-decl) class-body)
        (expect-token 'semi))
      (expect-token 'close-brace)
      (make-aggregate-node 'class-decl (nconc (list class-name base-class type-prefix)
                                              class-body)))))

(define (parse-import)
  "Parse an import statement"
  (expect-token 'import)
  (make-node 'import (parse-simple-name)))

(define (parse-file :optional class-list)
  "Parse a java source file and return an AST node"
  (let (class-list)
    (while (check-token 'class 'final 'public 'private 'protected 'static)
      (set! class-list (append class-list (list (parse-class)))))
    (make-aggregate-node 'file class-list)))

(define (get-file-ast filename)
  "Parse a java file and return an AST "
  (set! *token-list* (get-file-lexeme-stream filename))
  (parse-file))




