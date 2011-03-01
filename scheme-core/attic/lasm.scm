
;;;; lasm.scm --
;;;;
;;;; The linear assembler. This maps fast-op assembly trees into linear 
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

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
  (let ((literals (literals outermost-asm)))
    (define (literal-index value)
      (list-index #L(eq? value _) literals))
    

    (define (lasm/inner asm)
      (let ((opcode (car asm)))
        (case opcode
          ((:literal :global-ref :local-ref)
           `(,(car opcode) ,(literal-index (second opcode))))
          ((:global-set! :local-set!)
           `(,(car opcode) ,(literal-index (second opcode)) ,(literal-index (third opcode))))

          ((:close-env)
           (scheme::assemble-fast-op :close-env
                                     (cons (cadr asm)
                                           (lasm/inner (caddr asm)))
                                     (cadddr asm)))
          ((:apply)
           `(,@(map lasm/inner (cddr asm)) ,(lasm/inner (cadr asm)) ,((car asm))))
          ((:macro)
           (scheme::assemble-fast-op :literal
                                     (dbind (opcode macro-fn) asm
                                       (apply scheme::%macro (lasm/outer macro-fn) ()))))
          (#t
           (apply scheme::assemble-fast-op opcode
                  (map lasm/inner (cdr asm))))))))

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