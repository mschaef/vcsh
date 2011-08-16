
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

(define (compile form)
  "Accept a <form> and compile it into a closure that can be invoked to produce the
  effect of evaluating that form at toplevel."
  (cpass/fasm
   (cpass/fasm-optimize
    (cpass/meaning
     (cpass/expand  `(compiler::%toplevel-lambda ,form))))))
