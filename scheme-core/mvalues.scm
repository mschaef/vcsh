
;;;; mvalues.scm --
;;;;
;;;; Multiple value support.
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
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
