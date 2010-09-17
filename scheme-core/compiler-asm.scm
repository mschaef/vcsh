;;;; compiler-asm.scm
;;;;
;;;; The assembler. This maps fast-op assembly trees into closures.

(define (assemble outermost-asm)

  (define (assemble/inner asm)
    (let ((opcode (car asm)))
      (case opcode ; REVISIT: can this be driven off of opcode metadata?
        ((:literal :global-ref :local-ref)
         (scheme::assemble-fast-op opcode (cadr asm)))
        ((:global-set! :local-set!)
         (scheme::assemble-fast-op opcode (cadr asm) (assemble/inner (caddr asm))))
        ((:global-def)
         (scheme::assemble-fast-op :global-def (cadr asm) (caddr asm) (cadddr asm)))
        ((:close-env)
         (scheme::assemble-fast-op :close-env
                                   (cons (cadr asm)
                                         (assemble/inner (caddr asm)))
                                   (cadddr asm)))
        ((:apply)
         (apply scheme::assemble-fast-op :apply
                (assemble/inner (cadr asm)) (cons (map assemble/inner (cddr asm)))))
        ((:macro)
         (error "Macro definition ~s in unexpected place within ~s."
                asm outermost-asm))
        (#t
         (apply scheme::assemble-fast-op opcode
                (map assemble/inner (cdr asm)))))))

  (define (assemble/outer asm)
    (case (car asm)
      ((:close-env)
       (dbind (opcode l-list src p-list) asm
         (scheme::%closure ()
                           (cons l-list (assemble/inner src))
                           p-list)))
      ((:literal)
       (dbind (opcode literal) asm
         literal))

      ((:macro)
       (dbind (opcode macro-fn) asm
           (apply scheme::%macro (assemble/outer macro-fn) ())))

      (#t
       (error "assemble expects to assemble either a literal or a closure: ~s") asm)))


  (unless  (list? outermost-asm)
    (error "assembler imput must be a proper list of fast-op definitions: ~s" outermost-asm))

  (assemble/outer outermost-asm))

