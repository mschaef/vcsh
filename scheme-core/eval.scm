;;; Eval.scm
;;;
;;; The evaluator.

(define (eval form :optional (lenv ()))
  ;; TODO: Add support to the inspector for passing in lenvs when this
  ;; gets enabled
  (unless (null? lenv)
    (error "non-null lenvs are not currently supported with compiler evaluation. form: ~s env: ~s" form lenv))
  (catch 'end-eval
    (handler-bind ((compiler::compile-error
                    (lambda (context-form fatal? message details)
                      (compiler::compiler-message context-form :error message details)
                      (error "Error while compiling form: ~s" form)))
                   (compiler::compile-warning
                    (lambda (context-form message args)
                      (compiler::compiler-message context-form :warning message args))))
      (let ((form-fn (compiler::toplevel-form->thunk form)))
        (form-fn)))))
