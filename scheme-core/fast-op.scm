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

(define-fast-op :literal 8 1)
(define-fast-op :global-ref 16 1)
(define-fast-op :global-set! 17 2)
(define-fast-op :local-ref 18 1)
(define-fast-op :local-set! 19 2)

(define-fast-op :apply 24 2)

(define-fast-op :if-true 32 3)

(define-fast-op :and/2 64 2)
(define-fast-op :or/2 65 2)

(define-fast-op :sequence 96 2)

(define-fast-op :catch-apply0 192 2)
(define-fast-op :throw 193 2)
(define-fast-op :unwind-protect 194 2)

(define-fast-op :close-env 128 2)

(define-fast-op :get-env 224 0)
(define-fast-op :set-genv 225 1)

(define-fast-op :global-def 240 3)

(define-fast-op :mark-stack 248 2)

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
