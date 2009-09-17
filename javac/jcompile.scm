;;;; jcompile.lisp - The front end for the CS375 java->classfile
;;;; compiler.
;;;;
;;;; Michael Schaeffer (CS375, Hartmann)

;;;; Modifications
; rev 1 - Initial Release (Final 375 Deliverable)

;;;; Load startup files

(load "utility.lisp")
(load "lex.lisp")
(load "syn.lisp")
(load "sem.lisp")
(load "int.lisp")
(load "gen.lisp")

;;;; Automatic test case code

(defun test-file (filename)
  (format t "~%Testing file ~A:~%------------------------------~%" filename)
  (system (string-append "cat " filename))
  (compile-java-file filename)
  (format t "~%------------------------------~%~%"))

; There is only one failure test case shown, because failures issue
; an official Lisp exception, and drop out to a debug prompt.  It isn't
; too easy to get around this non-interactively.

(defun do-tests ()
  (mapc #'test-file '("a.java" "b.java" "c.java")))
