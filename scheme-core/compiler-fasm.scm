;;;; compiler-fasm.scm
;;;;
;;;; The FOP assembler. This maps fast-op assembly trees into closures.

(define (fop-assemble outermost-asm)

  (define (fasm/inner asm)
    (let ((opcode (car asm)))
      (case opcode ; REVISIT: can this be driven off of opcode metadata?
        ((:literal :global-ref :local-ref)
         (scheme::assemble-fast-op opcode (cadr asm)))
        ((:global-set! :local-set!)
         (scheme::assemble-fast-op opcode (cadr asm) (fasm/inner (caddr asm))))
        ((:global-def)
         (scheme::assemble-fast-op :global-def (cadr asm) (caddr asm) (cadddr asm)))
        ((:close-env)
         (scheme::assemble-fast-op :close-env
                                   (cons (cadr asm)
                                         (fasm/inner (caddr asm)))
                                   (cadddr asm)))
        ((:apply)
         (apply scheme::assemble-fast-op :apply
                (fasm/inner (cadr asm)) (cons (map fasm/inner (cddr asm)))))
        ((:macro)
         (scheme::assemble-fast-op :literal
                                   (dbind (opcode macro-fn) asm
                                     (apply scheme::%macro (fasm/outer macro-fn) ()))))
        (#t
         (apply scheme::assemble-fast-op opcode
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

