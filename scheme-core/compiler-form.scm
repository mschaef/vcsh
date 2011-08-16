
;;;; compiler-form.scm --
;;;;
;;;; The form compiler entry points.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (compile-form form)
  "Accept a <form> and compile it into the corresponding closure or literal. <form>
   must be a form that either explicitly defines a closure or a literal."
  (fop-assemble
   (optimize-fop-assembly
    (expanded-form-meaning
     (expand-form form #t)
     ()))))

(define (compile form)
  "Accept a <form> and compile it into a closure that can be invoked to produce the
  effect of evaluating that form at toplevel."
  (compile-form `(compiler::%toplevel-lambda ,form)))
