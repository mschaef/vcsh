;;;; compiler-form.scm
;;;;
;;;; The form compiler entry points.

(define (compile-form form :optional (genv #f) (at-toplevel? #f))
  "Accept a <form> and compile it into the corresponding closure or literal. <form>
   must be a form that either explicitly defines a closure or a literal."
  (assemble
   (expanded-form-meaning
    (expand-form form genv at-toplevel?) () at-toplevel?)))

(define (toplevel-form->thunk form :optional (genv #f))
  "Accept a <form> and compile it into a closure that can be invoked to produce the
  effect of evaluating that form at toplevel."
  (compile-form `(compiler::%toplevel-lambda ,form) genv #t))