;;;; invoke-repl.scm
;;;;
;;;; Bootstrap file for the scc0 version of vcsh0. Because scc0 does not
;;;; run with a conventional s-core startup that calls (run), this file
;;;; is necessary to kickstart the REPL. It it loaded as the last bootstrap
;;;; file in a scansh0 to be used interactively.


(eval-when (:load-toplevel)
  (catch-all
    (scheme:repl)))
