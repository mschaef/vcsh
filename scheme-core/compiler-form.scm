
;;;; compiler-form.scm --
;;;;
;;;; The form compiler entry points.
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (compile-to-fasm form)
 (cpass/fasm-optimize
  (cpass/meaning
   (cpass/expand form))))

(define (compile form)
  "Accept a <form> and compile it into a closure that can be invoked to produce the
  effect of evaluating that form at toplevel."
  (cpass/fasm
   (compile-to-fasm `(compiler::%toplevel-lambda ,form))))

(define (compile-form form)
  "Accept a <form> and translate it into its compiled representation."
  ((compile form)))
