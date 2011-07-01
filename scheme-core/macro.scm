
;;;; macro.scm --
;;;;
;;;; Support for Common Lisp style non-hygenic macros.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(defmacro (defmacro lambda-list . macro-body) ;; REVISIT: Macros with :optional arguments
  (check valid-lambda-list? lambda-list)
  (let ((macro-body macro-body) ; shadowed the argument to give us something settable.
        (macro-name (car lambda-list))
        (macro-formals (cdr lambda-list))
        (macro-documentation #f))
    (when (and (string? (car macro-body)) (> (length macro-body) 1))
      (set! macro-documentation (car macro-body))
      (set! macro-body (cdr macro-body)))
    `(define ,macro-name
       (%macrocons (%lambda (,@(if macro-documentation
                                   `((documentation . ,(normalize-whitespace macro-documentation)))
                                   ())
                             (macro-name . ,macro-name)
                             (macro-formals . ,macro-formals))
                       (form env)
                     (dbind ,macro-formals (cdr form)
                       ,@macro-body))))))