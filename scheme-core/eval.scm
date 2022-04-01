
;;;; eval.scm --
;;;;
;;;; The entry point to the evaluator.
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (eval form :optional (lenv ()))
  ;; REVISIT: Add support to the inspector for passing in lenvs when this gets enabled
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
      (let ((form-fn (compiler::compile form)))
        (form-fn)))))
