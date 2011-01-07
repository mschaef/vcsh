;;;; compiler-fasm.scm
;;;;
;;;; The FOP assembler. This maps fast-op assembly trees into closures.

;;;; Fast op stuff

(define-structure fast-op-definition
  opcode
  name
  arity
  args)

(define *fast-op-names* (make-hash :eq))
(define *fast-op-opcodes* (make-hash :eq))

(define (extend-fast-op-names! op-name op-code arity args)
  (when (hash-has? *fast-op-names* op-name)
    (error "Fast op already defined: ~s" op-name))
  (when (hash-has? *fast-op-opcodes* op-code)
    (error "Fast opcode for ~s already defined: ~s" op-name op-code))
  (let ((defn (make-fast-op-definition :name op-name
                                       :opcode op-code
                                       :arity arity
                                       :args args)))
    (hash-set! *fast-op-names* op-name defn)
    (hash-set! *fast-op-opcodes* op-code defn)))

(defmacro (define-fast-op op-name op-code . args)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (extend-fast-op-names! ,op-name ,op-code ,(length args) ',args)))

(define-fast-op :literal        #.system::FOP_LITERAL        :literal                  )
(define-fast-op :global-ref     #.system::FOP_GLOBAL_REF     :symbol                   )
(define-fast-op :global-set!    #.system::FOP_GLOBAL_SET     :symbol :fast-op          )
(define-fast-op :local-ref      #.system::FOP_LOCAL_REF      :symbol                   )
(define-fast-op :local-set!     #.system::FOP_LOCAL_SET      :symbol :fast-op          )
(define-fast-op :apply          #.system::FOP_APPLY          :symbol (:fast-op)        )
(define-fast-op :if-true        #.system::FOP_IF_TRUE        :fast-op :fast-op :fast-op)
(define-fast-op :and/2          #.system::FOP_AND2           :fast-op :fast-op         )
(define-fast-op :or/2           #.system::FOP_OR2            :fast-op :fast-op         )
(define-fast-op :sequence       #.system::FOP_SEQUENCE       :fast-op :fast-op         )
(define-fast-op :throw          #.system::FOP_THROW          :fast-op :fast-op         )
(define-fast-op :catch          #.system::FOP_CATCH          :fast-op :fast-op         )
(define-fast-op :with-unwind-fn #.system::FOP_WITH_UNWIND_FN :fast-op :fast-op         )
(define-fast-op :close-env      #.system::FOP_CLOSE_ENV      :fast-op :literal         )
(define-fast-op :get-env        #.system::FOP_GET_ENV                                  )
(define-fast-op :global-def     #.system::FOP_GLOBAL_DEF     :symbol :literal          )
(define-fast-op :get-fsp        #.system::FOP_GET_FSP                                  )
(define-fast-op :get-hframes    #.system::FOP_GET_HFRAMES                              )
(define-fast-op :set-hframes    #.system::FOP_SET_HFRAMES    :fast-op                  )

(define (parse-fast-op fast-op :optional (parse-opcode? #t))
  (let ((opcode (scheme::%fast-op-opcode fast-op))
        (args (scheme::%fast-op-args fast-op)))
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
    (apply scheme::%fast-op (fast-op-definition-opcode defn) args)))

(define (fast-op? obj)
  (eq? 'fast-op (type-of obj)))

(define (fop-assemble outermost-asm)

  (define (fasm/inner asm)
    (let ((opcode (car asm)))
      (case opcode ; REVISIT: can this be driven off of opcode metadata?
        ((:literal :global-ref :local-ref)
         (assemble-fast-op opcode (cadr asm)))
        ((:global-set! :local-set!)
         (assemble-fast-op opcode (cadr asm) (fasm/inner (caddr asm))))
        ((:global-def)
         (assemble-fast-op :global-def (cadr asm) (caddr asm) (cadddr asm)))
        ((:close-env)
         (assemble-fast-op :close-env
                                   (cons (cadr asm)
                                         (fasm/inner (caddr asm)))
                                   (cadddr asm)))
        ((:apply)
         (apply assemble-fast-op :apply
                (fasm/inner (cadr asm)) (cons (map fasm/inner (cddr asm)))))
        ((:macro)
         (assemble-fast-op :literal
                                   (dbind (opcode macro-fn) asm
                                     (apply scheme::%macro (fasm/outer macro-fn) ()))))
        (#t
         (apply assemble-fast-op opcode
                (map fasm/inner (cdr asm)))))))

  (define (fasm/outer asm)
    (case (car asm)
      ((:close-env)
       (dbind (opcode l-list src p-list) asm
         (scheme::%closure () (cons l-list (fasm/inner src)) p-list)))
      ((:literal)
       (dbind (opcode literal) asm
         literal))

      ((:macro)
       (dbind (opcode macro-fn) asm
           (apply scheme::%macro (fasm/outer macro-fn) ())))

      (#t
       (error "assemble expects to assemble either a literal or a closure: ~s") asm)))


  (unless  (list? outermost-asm)
    (error "assembler imput must be a proper list of fast-op definitions: ~s" outermost-asm))

  (fasm/outer outermost-asm))

