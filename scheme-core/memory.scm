
;;;; memory.scm --
;;;;
;;;; Memory management support implemented in Scheme.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define *heap-enlarge-threshold* 0.25)

(define (maybe-enlarge-heap cells-freed)
  (dbind #(heap-segments heap-segment-cells) (gc-info)
    (unless (> cells-freed
               (* heap-segments heap-segment-cells *heap-enlarge-threshold*))
      (%request-heap-size (+ 1 heap-segments)))))

(define (trap-after-gc trapno fsp cells-freed)
  (maybe-enlarge-heap cells-freed))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_AFTER_GC trap-after-gc))
