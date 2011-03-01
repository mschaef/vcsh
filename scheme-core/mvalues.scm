
;;;; mvalues.scm --
;;;;
;;;; Multiple value support.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (values . v)
  (if (= (length v) 1)
      (first v)
      (apply %values v)))

(define (call-with-values proc vals)
  (apply proc (%values->list vals)))

(defmacro (mvbind vars form . body)
  `(call-with-values (lambda ,vars ,@body) ,form))
