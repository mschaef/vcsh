;;;; fasl-compiler0.scm
;;;;
;;;; Bootstrap file for the scc0 fasl-compiler. Because scc0 does not
;;;; run with a conventional s-core startup that calls (run), this file
;;;; is necessary to kickstart the compiler. It it loaded as the last
;;;; bootstrap file in a scansh0 to be used as a compiler.

(define *args* (cdr *args0*))
(define *args0* (quote ()))

(eval-when (:load-toplevel)
   (process-arguments #f)
   (run))
