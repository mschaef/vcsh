;;;; jcompile.lisp - The front end for the CS375 java->classfile
;;;; compiler.
;;;;
;;;; Michael Schaeffer (CS375, Hartmann)

;;;; Modifications
; rev 1 - Initial Release (Final 375 Deliverable)

;;;; Load startup files

(enlarge-heap 16)

(define-package "javac"
  (:uses "scheme")
  (:includes "utility.scm"
             "lex.scm"
             "syn.scm"
         ;;"sem.lisp"
         ;;"int.lisp"
         ;;"gen.lisp"
         ))
