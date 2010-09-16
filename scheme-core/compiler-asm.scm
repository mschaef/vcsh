;;;; compiler-asm.scm
;;;;
;;;; The assembler. This maps fast-op assembly trees into closures.

(define (assemble outer-form)
  (define (asm form)
    (let ((opcode (car form)))
      (case opcode ; REVISIT: can this be driven off of opcode metadata?
        ((:literal :global-ref :local-ref)
         (scheme::assemble-fast-op opcode (cadr form)))
        ((:global-set! :local-set!)
         (scheme::assemble-fast-op opcode (cadr form) (asm (caddr form))))
        ((:global-def)
         (scheme::assemble-fast-op :global-def (cadr form) (caddr form) (cadddr form)))
        ((:close-env)
         (scheme::assemble-fast-op :close-env
                                   (cons (cadr form)
                                         (asm (caddr form)))
                                   (cadddr form)))
        ((:apply)
         (apply scheme::assemble-fast-op :apply
                (asm (cadr form)) (cons (map asm (cddr form)))))
        ((:macro)
         (error "Macro definition ~s in unexpected place within ~s."
                form outer-form))
        (#t
         (apply scheme::assemble-fast-op opcode
                (map asm (cdr form)))))))

  (unless  (list? outer-form)
    (error "assembler imput must be a proper list of fast-op definitions: ~s" outer-form))

  (case (car outer-form)
    ((:close-env)
     (dbind (opcode l-list src p-list) outer-form
       (scheme::%closure ()
                         (cons l-list (asm src))
                         p-list)))
    ((:literal)
     (dbind (opcode literal) outer-form
       literal))
    (#t
     (error "assemble expects to assemble either a literal or a closure: ~s") outer-form)))

