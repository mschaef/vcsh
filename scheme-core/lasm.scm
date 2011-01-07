;;;; lasm.scm
;;;;
;;;; The linear assembler. This maps fast-op assembly trees into linear 

(define-package "lasm"
  (:uses "scheme"
         "compiler"))

(define (literals tasm)
  (set-union/eq
   (let recur ((tasm tasm))
     (let ((opcode (car tasm)))
       (case opcode
         ((:literal :global-ref :local-ref)
          `(,(second tasm)))
         ((:global-set! :local-set!)
          `(,(second tasm) ,@(recur (third tasm))))
         ((:global-def)
          `(,(second tasm) ,@(recur (third tasm))))
         ((:close-env)
          `(,@(second tasm) ,@(recur (caddr tasm))))
         ((:apply)
          `(,@(append-map recur (cdr tasm))))
         ((:macro)
          `(,(recur (second tasm))))
         (#t
          (append-map recur (cdr tasm))))))))

(define (label) (gensym "label"))

(define (lasm outermost-asm)
  (define (lasm/inner asm)
    (let ((opcode (car asm)))
      (case opcode ; REVISIT: can this be driven off of opcode metadata?
        ((:literal :global-ref :local-ref)
         (scheme::assemble-fast-op opcode (cadr asm)))
        ((:global-set! :local-set!)
         (scheme::assemble-fast-op opcode (cadr asm) (lasm/inner (caddr asm))))
        ((:global-def)
         (scheme::assemble-fast-op :global-def (cadr asm) (caddr asm) (cadddr asm)))
        ((:close-env)
         (scheme::assemble-fast-op :close-env
                                   (cons (cadr asm)
                                         (lasm/inner (caddr asm)))
                                   (cadddr asm)))
        ((:apply)
         (apply scheme::assemble-fast-op :apply
                (lasm/inner (cadr asm)) (cons (map lasm/inner (cddr asm)))))
        ((:macro)
         (scheme::assemble-fast-op :literal
                                   (dbind (opcode macro-fn) asm
                                     (apply scheme::%macro (lasm/outer macro-fn) ()))))
        (#t
         (apply scheme::assemble-fast-op opcode
                (map lasm/inner (cdr asm)))))))

  (define (lasm/outer asm)
    (case (car asm)
      ((:close-env)
       (dbind (opcode l-list src p-list) asm
         (scheme::%closure () (cons l-list (lasm/inner src)) p-list)))
      ((:literal)
       (dbind (opcode literal) asm
         literal))

      ((:macro)
       (dbind (opcode macro-fn) asm
           (apply scheme::%macro (lasm/outer macro-fn) ())))

      (#t
       (error "lasm expects to assemble either a literal or a closure: ~s") asm)))


  (unless  (list? outermost-asm)
    (error "lasmr imput must be a proper list of fast-op definitions: ~s" outermost-asm))

  (lasm/outer outermost-asm))

(define (lcompile form)
  (compiler::compile-form->assembly form))