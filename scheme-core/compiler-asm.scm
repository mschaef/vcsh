;;;; compiler-asm.scm
;;;;
;;;; The assembler. This maps fast-op assembly trees into closures.

(define (assemble-closure outer-form)
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

  (unless (and (list? outer-form)
               (eq? (car outer-form) :close-env))
    (error "Expected a closure definition to assemble: ~s" outer-form))

  (dbind (opcode l-list src p-list) outer-form
    (scheme::%closure ()
                      (cons l-list (asm src))
                      p-list)))

