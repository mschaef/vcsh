;;;; compiler.scm
;;;;
;;;; The main compiler file.

;;; TODO: Alter compiler to store a metadata with compiled functions
;;;   - Version of host VM/scheme image
;;;   - Date/time compiled
;;;   - Mapping from fast-ops back to source forms
;;;   - Original source
;;;   - (A lot of these should be optional in various ways.)

(define-package "compiler"
  (:uses "scheme")
  (:includes "compiler-tools.scm"
             "compiler-asm.scm"
             "compiler-expand.scm"
             "compiler-meaning.scm"
             "compiler-form.scm"
             "compiler-file.scm"
             )
  (:exports "compile-file"
            "*verbose*"
            "*output-file-name*"
            "*debug*"
            "*show-meanings*"
            "*show-expansions*"
            "*show-actions*"
            "*files-to-compile*"
            "*initial-package*"
            "*cross-compile*"
            ))

