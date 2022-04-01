
;;;; macro.scm --
;;;;
;;;; Support for Common Lisp style non-hygenic macros.
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(defmacro (defmacro lambda-list . macro-body) ;; REVISIT: Macros with :optional arguments
  (runtime-check valid-lambda-list? lambda-list)
  (dbind (macro-name . macro-formals) lambda-list
    (mvbind (doc-string macro-body) (accept-documentable-block macro-body)
      `(define ,macro-name
         (%macrocons (%lambda (,@(if doc-string
                                     `((documentation . ,(normalize-whitespace doc-string)))
                                     ())
                               (macro-name . ,macro-name)
                               (macro-formals . ,macro-formals))
                         (form env)
                       (dbind ,macro-formals (cdr form)
                         ,@macro-body)))))))
