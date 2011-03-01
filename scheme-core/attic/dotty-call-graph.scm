
;;;; dotty-call-graph.scm --
;;;;
;;;; Code to write a call graph out in dotty syntax.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define-package "dotty-call-graph"
  (:uses "scheme"
         "call-graph")
  (:exports "dotty-call-graph"))


(define (write-dotty-call-graph cg :optional (op (current-output-port)))
  (define (format-name name)
    (format #f "\"~a::~a\"" (package-name (symbol-package name)) (symbol-name name)))
  (format op "digraph G {\n")
  (dolist (caller-info cg)
    (let ((caller (car caller-info)))
      (dolist (callee (cdr caller-info))
        (format op "   ~a->~a;\n" (format-name caller) (format-name callee))))
    (format op "\n"))
  (format op "}\n"))

;; (define (package-call-graph package)
;;   (let ((cg (call-graph::package-call-graph package)))
;;     ))

