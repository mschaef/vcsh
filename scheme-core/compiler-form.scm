;;;; compiler-form.scm
;;;;
;;;; The form compiler entry points.

(define (form->fop-assembly form :optional (at-toplevel? #f))
  (expanded-form-meaning
   (expand-form form at-toplevel?) () at-toplevel?))

(define (compile-form form :optional (at-toplevel? #f))
  "Accept a <form> and compile it into the corresponding closure or literal. <form>
   must be a form that either explicitly defines a closure or a literal."
  (fop-assemble
   (optimize-fop-assembly
    (form->fop-assembly form at-toplevel?))))

(define (toplevel-form->thunk form)
  "Accept a <form> and compile it into a closure that can be invoked to produce the
  effect of evaluating that form at toplevel."
  (compile-form `(compiler::%toplevel-lambda ,form) #t))