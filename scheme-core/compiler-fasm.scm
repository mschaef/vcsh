
;;;; compiler-fasm.scm -- 
;;;;
;;;; The FOP assembler. This maps fast-op assembly trees into closures.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;;;; Fast op stuff

(define-structure fop-defn
  opcode
  name
  arity
  formals)

(define *fop-name->fop-defn* (make-hash :eq))
(define *fop-opcode->fop-defn* (make-hash :eq))

(define (valid-fop-formals? formals)
  (define (valid-formal-type? type)
    (memq type '(:literal :symbol :fast-op)))
  (define (valid-fop-formal? formal)
    (or (valid-formal-type? formal)
        (and (list? formal)
             (length=1? formal)
             (valid-fop-formal? (car formal)))))
  (if (and (list? formals)
           (<= (length formals) 3)
           (every? valid-fop-formal? formals))
      formals
      #f))

(define (extend-fast-op-names! op-name op-code arity formals)
  (when (hash-has? *fop-name->fop-defn* op-name)
    (error "Fast op already defined: ~s" op-name))
  (when (hash-has? *fop-opcode->fop-defn* op-code)
    (error "Fast opcode for ~s already defined: ~s" op-name op-code))
  (unless (valid-fop-formals? formals)
    (error "Invalid formal argument list for fast op: ~s" formals))
  (let ((defn (make-fop-defn :name op-name
                             :opcode op-code
                             :arity arity
                             :formals formals)))
    (hash-set! *fop-name->fop-defn* op-name defn)
    (hash-set! *fop-opcode->fop-defn* op-code defn)))

(defmacro (define-fast-op op-name op-code . formals)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (extend-fast-op-names! ,op-name ,op-code ,(length formals) ',formals)))

(define-fast-op :literal                #.system::FOP_LITERAL               :literal                  )
(define-fast-op :macro                  -1                                  :fast-op                  )
(define-fast-op :global-ref             #.system::FOP_GLOBAL_REF            :symbol                   )
(define-fast-op :global-set/rv!         #.system::FOP_GLOBAL_SETRV          :symbol                   )
(define-fast-op :local-ref              #.system::FOP_LOCAL_REF             :symbol                   )
(define-fast-op :local-set/rv!          #.system::FOP_LOCAL_SETRV           :symbol                   )
(define-fast-op :apply-global           #.system::FOP_APPLY_GLOBAL          :symbol (:fast-op)        )
(define-fast-op :apply                  #.system::FOP_APPLY                 :fast-op (:fast-op)       )
(define-fast-op :if-true/rv             #.system::FOP_IF_TRUERV             :fast-op :fast-op         )
(define-fast-op :retval                 #.system::FOP_RETVAL                                          )
(define-fast-op :sequence               #.system::FOP_SEQUENCE              :fast-op :fast-op         )
(define-fast-op :throw                  #.system::FOP_THROW                 :fast-op :fast-op         )
(define-fast-op :catch                  #.system::FOP_CATCH                 :fast-op :fast-op         )
(define-fast-op :with-unwind-fn         #.system::FOP_WITH_UNWIND_FN        :fast-op :fast-op         )
(define-fast-op :closure                #.system::FOP_CLOSURE               :literal :fast-op :literal)
(define-fast-op :car                    #.system::FOP_CAR                   :fast-op                  )
(define-fast-op :cdr                    #.system::FOP_CDR                   :fast-op                  )
(define-fast-op :not                    #.system::FOP_NOT                   :fast-op                  )
(define-fast-op :nullp                  #.system::FOP_NULLP                 :fast-op                  )
(define-fast-op :car/rv                 #.system::FOP_CARRV                                           )
(define-fast-op :cdr/rv                 #.system::FOP_CDRRV                                           )
(define-fast-op :not/rv                 #.system::FOP_NOTRV                                           )
(define-fast-op :nullp/rv               #.system::FOP_NULLPRV                                         )
(define-fast-op :eqp                    #.system::FOP_EQP                   :fast-op :fast-op         )
(define-fast-op :get-env                #.system::FOP_GET_ENV                                         )
(define-fast-op :global-def             #.system::FOP_GLOBAL_DEF            :symbol :literal :literal )
(define-fast-op :get-fsp                #.system::FOP_GET_FSP                                         )
(define-fast-op :get-frame              #.system::FOP_GET_FRAME                                       )
(define-fast-op :get-hframes            #.system::FOP_GET_HFRAMES                                     )
(define-fast-op :set-hframes            #.system::FOP_SET_HFRAMES           :fast-op                  )
(define-fast-op :global-preserve-frame  #.system::FOP_GLOBAL_PRESERVE_FRAME :symbol :fast-op          )

(define (parse-fast-op fast-op :optional (parse-opcode? #t))
  (let ((opcode (scheme::%fast-op-opcode fast-op))
        (args (scheme::%fast-op-args fast-op)))
    (let ((defn (hash-ref *fop-opcode->fop-defn* opcode #f)))
      (if (and defn parse-opcode?)
          (values (fop-defn-name defn)
                  (take args (fop-defn-arity defn)))
          (values opcode args)))))

(define (fop-name->formals fop-name)
  (aif (hash-ref *fop-name->fop-defn* fop-name #f)
       (fop-defn-formals it)
       #f))

(define (fast-op-args fast-op)
  (mvbind (opcode args) (parse-fast-op fast-op)
     args))

(define (assemble-fast-op op . args)
  (let ((defn (hash-ref *fop-name->fop-defn* op #f)))
    (unless defn
      (error "Invalid fast-op to assemble: ~s." op))
    (unless (= (length args) (fop-defn-arity defn))
       (error "Expected ~s argument(s) to fast-op ~s, got ~s"  ; REVISIT: pluralization?
              (fop-defn-arity defn) op args))
    (apply scheme::%fast-op (fop-defn-opcode defn) args)))

(define (fast-op? obj)
  (eq? 'fast-op (type-of obj)))

(define (fop-assemble outermost-asm)

 (define (fasm/inner asm)
    (let ((opcode (car asm)))
      (case opcode ; REVISIT: can this be driven off of opcode metadata?
        ((:literal :global-ref :local-ref)
         (assemble-fast-op opcode (cadr asm)))
        ((:global-set/rv! :local-set/rv!)
         (assemble-fast-op opcode (cadr asm)))
        ((:global-def)
         (assemble-fast-op :global-def (cadr asm) (caddr asm) (cadddr asm)))
        ((:closure)
         (assemble-fast-op :closure (cadr asm) (fasm/inner (caddr asm)) (cadddr asm)))
        ((:apply-global)
         (assemble-fast-op :apply-global (cadr asm) (map fasm/inner (caddr asm))))
        ((:apply)
         (assemble-fast-op :apply (fasm/inner (cadr asm)) (map fasm/inner (caddr asm))))
        ((:macro)
         (assemble-fast-op :literal
                                   (dbind (opcode macro-fn) asm
                                     (apply scheme::%macrocons (fasm/outer macro-fn) ()))))
        ((:global-preserve-frame)
         (assemble-fast-op :global-preserve-frame
                           (cadr asm)
                           (fasm/inner (caddr asm))))
        (#t
         (apply assemble-fast-op opcode
                (map fasm/inner (cdr asm)))))))

  (define (fasm/outer asm)
    (case (car asm)
      ((:closure)
       (dbind (opcode l-list src p-list) asm
         (scheme::%closure () (cons l-list (fasm/inner src)) p-list)))
      ((:literal)
       (dbind (opcode literal) asm
         literal))
      ((:macro)
       (dbind (opcode macro-fn) asm
           (apply scheme::%macrocons (fasm/outer macro-fn) ())))

      (#t
       (error "assemble expects to assemble either a literal or a closure: ~s") asm)))


  (unless  (list? outermost-asm)
    (error "assembler imput must be a proper list of fast-op definitions: ~s" outermost-asm))

  (fasm/outer outermost-asm))

