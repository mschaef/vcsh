
;;;; foplog.scm --
;;;;
;;;; Scheme level support for the VM's optional FOPLOG facility.
;;;; This module provides tools for generating distributions of FOPs
;;;; encountered during execution, as well as FOP sequences.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define-package "foplog"
  (:uses "scheme")
  (:exports "fop-histogram"
            "fop-seq-histogram"
            "foplog-for"))

(define %foplog-reset #.(scheme::%subr-by-name "%foplog-reset" #L0(error "foplog support not found")))
(define %foplog-enable #.(scheme::%subr-by-name "%foplog-enable" #L0(error "foplog support not found")))
(define %foplog-snapshot #.(scheme::%subr-by-name "%foplog-snapshot" #L0(error "foplog support not found")))

(define (opcode->name op)
  (if (not (null? op))
      (mvbind (opcode args) (compiler::parse-fast-op op) opcode)
      #f))

(define (foplog->opcodes log)
  (map opcode->name (vector->list log)))

(define (foplog)
  (map opcode->name (vector->list (%foplog-snapshot))))

(define (foplog-seqs log length)
  (map-pair #L(take-up-to _ length) log))

(define (hist log)
  (let ((counts (make-hash :equal)))
    (dolist (entry log)
      (hash-set! counts entry (+ 1 (hash-ref counts entry 0))))
    (qsort (hash->a-list counts) > cdr)))

(define (fop-histogram :optional (log (%foplog-snapshot)))
  (hist (foplog->opcodes log)))

(define (fop-seq-histogram :optional (log (%foplog-snapshot)) (length 2))
  (hist (foplog-seqs (foplog->opcodes log) length)))

(defmacro (foplog-for expr)
  `(begin
     (when (%foplog-enable #f)
       (error "foplog already enabled."))
     (%foplog-reset)
     (%foplog-enable #t)
     ,expr
     (%foplog-enable #f)
     (%foplog-snapshot)))