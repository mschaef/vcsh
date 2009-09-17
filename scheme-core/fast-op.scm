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
  `(extend-fast-op-names! ,op-name ,op-code ,arity))

(define-fast-op :global-ref 16 1)
(define-fast-op :global-set! 17 2)

(define (parse-fast-op fast-op :optional (parse-opcode? #t))
  (let ((opcode (%fast-op-opcode fast-op))
        (args (%fast-op-args fast-op)))
    (let ((defn (hash-ref *fast-op-opcodes* opcode #f)))
      (values (if (and defn parse-opcode?)
                  (fast-op-definition-name defn)
                  opcode)
              (take args (fast-op-definition-arity defn))))))

(define (fast-op-args fast-op)
  (values-bind (parse-fast-op fast-op) (opcode args)
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