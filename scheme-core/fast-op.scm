;;;; Fast op stuff

(define-structure fast-op-definition
  opcode
  name
  arity)

(define *fast-op-names* (make-hash :eq))
(define *fast-op-opcodes* (make-hash :eq))

(define (extend-fast-op-names! op-name op-code arity)
  (when (hash-has? *fast-op-names* op-name)
    (error "Fast op already defined: ~s" op-name))
  (when (hash-has? *fast-op-opcodes* op-code)
    (error "Fast opcode for ~s already defined: ~s" op-name op-code))
  (let ((defn (make-fast-op-definition :name op-name
                            :opcode op-code
                            :arity arity)))
    (hash-set! *fast-op-names* op-name defn)
    (hash-set! *fast-op-opcodes* op-code defn)))

(defmacro (define-fast-op op-name op-code arity)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (extend-fast-op-names! ,op-name ,op-code ,arity)))


(define-fast-op :literal #.system::FOP_LITERAL 1)
(define-fast-op :global-ref #.system::FOP_GLOBAL_REF 1)
(define-fast-op :global-set! #.system::FOP_GLOBAL_SET 2)
(define-fast-op :local-ref #.system::FOP_LOCAL_REF 1)
(define-fast-op :local-set! #.system::FOP_LOCAL_SET 2)
 
(define-fast-op :apply #.system::FOP_APPLY 2)
 
(define-fast-op :if-true #.system::FOP_IF_TRUE 3)
 
(define-fast-op :and/2 #.system::FOP_AND2 2)
(define-fast-op :or/2 #.system::FOP_OR2 2)
 
(define-fast-op :sequence #.system::FOP_SEQUENCE 2)
 
(define-fast-op :throw #.system::FOP_THROW 2)
 
(define-fast-op :catch #.system::FOP_CATCH 2)
(define-fast-op :with-unwind-fn #.system::FOP_WITH_UNWIND_FN 2)
 
(define-fast-op :close-env #.system::FOP_CLOSE_ENV 2)
 
(define-fast-op :get-env #.system::FOP_GET_ENV 0)
 
(define-fast-op :global-def #.system::FOP_GLOBAL_DEF 3)
 
(define-fast-op :mark-stack #.system::FOP_MARK_STACK 2)

(define-fast-op :get-fsp #.system::FOP_GET_FSP 0)
(define-fast-op :get-hframes #.system::FOP_GET_HFRAMES 0)
(define-fast-op :set-hframes #.system::FOP_SET_HFRAMES 1)

(define (parse-fast-op fast-op :optional (parse-opcode? #t))
  (let ((opcode (%fast-op-opcode fast-op))
        (args (%fast-op-args fast-op)))
    (let ((defn (hash-ref *fast-op-opcodes* opcode #f)))
      (if (and defn parse-opcode?)
          (values (fast-op-definition-name defn)
                  (take args (fast-op-definition-arity defn)))
          (values opcode args)))))

(define (fast-op-args fast-op)
  (mvbind (opcode args) (parse-fast-op fast-op)
     args))

(define (assemble-fast-op op . args)
  (let ((defn (hash-ref *fast-op-names* op #f)))
    (unless defn
      (error "Invalid fast-op to assemble: ~s." op))
    (unless (= (length args) (fast-op-definition-arity defn))
       (error "Expected ~s argument(s) to fast-op ~s, got ~s"  ; REVISIT: pluralization?
              (fast-op-definition-arity defn) op args))
    (apply %fast-op (fast-op-definition-opcode defn) args)))

(define (fast-op? obj)
  (eq? 'fast-op (type-of obj)))
