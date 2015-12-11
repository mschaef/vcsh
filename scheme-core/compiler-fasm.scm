
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

(define *fop-name->fop-defn* (make-identity-hash))
(define *fop-opcode->fop-defn* (make-identity-hash))

(define (valid-fop-formals? formals)
  (define (valid-formal-type? type)
    (memq type '(:literal :symbol :fast-op :fast-ops)))
  (if (and (list? formals)
           (<= (length formals) 3)
           (every? valid-formal-type? formals))
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

(define-fast-op :literal                #.system::FOP_LITERAL               :literal           )
(define-fast-op :global-ref             #.system::FOP_GLOBAL_REF            :symbol            )
(define-fast-op :local-ref              #.system::FOP_LOCAL_REF             :symbol            )
(define-fast-op :global-set!            #.system::FOP_GLOBAL_SET            :symbol            )
(define-fast-op :local-set!             #.system::FOP_LOCAL_SET             :symbol            )
(define-fast-op :apply-global           #.system::FOP_APPLY_GLOBAL          :symbol :fast-ops  )
(define-fast-op :apply                  #.system::FOP_APPLY                 :fast-op :fast-ops )
(define-fast-op :if-true                #.system::FOP_IF_TRUE               :fast-op :fast-op  )
(define-fast-op :retval                 #.system::FOP_RETVAL                                   )
(define-fast-op :sequence               #.system::FOP_SEQUENCE              :fast-op :fast-op  )
(define-fast-op :closure                #.system::FOP_CLOSURE               :literal :fast-op  )
(define-fast-op :eqp                    #.system::FOP_EQP                   :fast-op :fast-op  )
(define-fast-op :car                    #.system::FOP_CAR                                      )
(define-fast-op :cdr                    #.system::FOP_CDR                                      )
(define-fast-op :not                    #.system::FOP_NOT                                      )
(define-fast-op :nullp                  #.system::FOP_NULLP                                    )
(define-fast-op :throw                  #.system::FOP_THROW                 :fast-op :fast-op  )
(define-fast-op :catch                  #.system::FOP_CATCH                 :fast-op :fast-op  )
(define-fast-op :with-unwind-fn         #.system::FOP_WITH_UNWIND_FN        :fast-op :fast-op  )
(define-fast-op :get-env                #.system::FOP_GET_ENV                                  )
(define-fast-op :global-def             #.system::FOP_GLOBAL_DEF            :symbol :literal   )
(define-fast-op :get-fsp                #.system::FOP_GET_FSP                                  )
(define-fast-op :get-frame              #.system::FOP_GET_FRAME                                )
(define-fast-op :get-hframes            #.system::FOP_GET_HFRAMES                              )
(define-fast-op :set-hframes            #.system::FOP_SET_HFRAMES           :fast-op           )
(define-fast-op :global-preserve-frame  #.system::FOP_GLOBAL_PRESERVE_FRAME :symbol :fast-op   )

(define (parse-fast-op fast-op)
  (let ((opcode (scheme::%fast-op-opcode fast-op))
        (args (scheme::%fast-op-args fast-op)))
    (let ((defn (hash-ref *fop-opcode->fop-defn* opcode #f)))
      (values opcode
              (if defn
                  (fop-defn-name defn)
                  #f)
              (if defn
                  (take args (fop-defn-arity defn))
                  args)))))

(define (fop-name->formals fop-name)
  (aif (hash-ref *fop-name->fop-defn* fop-name #f)
       (fop-defn-formals it)
       #f))

(define (fast-op-args fast-op)
  (mvbind (opcode op-name args) (parse-fast-op fast-op)
     args))

(define (fast-op? obj)
  (eq? 'fast-op (type-of obj)))

(define (lookup-fast-op op)
    (let ((defn (hash-ref *fop-name->fop-defn* op #f)))
      (unless defn
        (error "Invalid fast-op: ~s." op))
      (values (fop-defn-opcode defn)
              (fop-defn-formals defn))))

(forward fasm)

(define (fasm asm)
  (dbind (op . actuals) asm
    (mvbind (opcode formals) (lookup-fast-op op)
      (unless (same-length? actuals formals)
        (error "Improper number of arguments while assembling ~s" asm))
      (apply scheme::%fast-op
             opcode
             (map (lambda (formal actual)
                    (case formal
                      ((:literal)  actual)
                      ((:fast-op)  (fasm actual))
                      ((:fast-ops) (map fasm actual))
                      ((:symbol)   (runtime-check symbol? actual))
                      (#t
                       (error "Invalid fast-op formal argument type: ~s" formal))))
                  formals
                  actuals)))))

(define (fop-assemble outermost-asm)
  (runtime-check list? outermost-asm "Malformed FOP assembly syntax.")

  (unless (eq? (car outermost-asm) :closure)
    (error "assemble expects to assemble either a closure: ~s" outermost-asm))

  (dbind (opcode (l-list . p-list) src) outermost-asm
    (scheme::%closure () (cons l-list (fasm src)) p-list)))

(define (cpass/fasm fasm)
  (fop-assemble fasm))
