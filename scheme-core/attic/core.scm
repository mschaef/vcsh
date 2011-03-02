
;;;; core.scm --
;;;;
;;;; Core forms defined.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(%early-define (error message . args)
  (%panic (string-append "Early error: " message)))

(%early-defmacro (when condition . forms)
  `(if ,condition (begin ,@forms)))

(%early-defmacro (unless condition . forms)
  `(when (not ,condition) ,@forms))

(%early-defmacro (assert condition)
  `(unless ,condition (error "Assertation Failure: ~a" ',condition)))

(%early-define (atom? x)
  (not (pair? x)))

(%early-define (throw tag :optional value)
  (%%throw tag value))

(%early-define (unwind-protect thunk after)
  (unless (procedure? thunk)
    (error "Bad thunk in unwind-protect: ~s") thunk)
  (unless (procedure? after)
    (error "Bad after thunk in unwind-protect: ~s") after)
  (%%with-unwind-fn after (thunk)))

(%early-defmacro (catch tag-form . body)
  `(%%catch ,tag-form (begin ,@body)))

(%early-defmacro (block . forms)
  `(catch '%return-target ,@forms))

(%early-defmacro (return value)
  `(throw '%return-target ,value))

(%early-defmacro (catch-all . forms)
  `(catch () ,@forms))



