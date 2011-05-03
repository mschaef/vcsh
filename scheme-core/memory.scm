
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

(define *gc-target-free-cell-factor* 0.9)

(define (maybe-enlarge-heap cells-freed)
  (dbind #(heap-segments heap-segment-cells max-heap-segments) (gc-info)
    (let ((target-free-cells (* heap-segments heap-segment-cells *gc-target-free-cell-factor*)))
      (unless (>= cells-freed target-free-cells)
        (let ((heap-segments-needed (ceiling (/ (- target-free-cells cells-freed)
                                                 heap-segment-cells))))
          (%request-heap-size (min max-heap-segments
                                   (+ heap-segments-needed heap-segments))))))))

(define (trap-after-gc trapno frp cells-freed)
  (maybe-enlarge-heap cells-freed))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_AFTER_GC trap-after-gc))
