
;;;; compiler.scm --
;;;;
;;;; The main compiler file.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;;; REVISIT: Alter compiler to store a metadata with compiled functions
;;;   - Version of host VM/scheme image
;;;   - Date/time compiled
;;;   - Mapping from fast-ops back to source forms
;;;   - Original source
;;;   - (A lot of these should be optional in various ways.)

(define-package "compiler"
  (:uses "scheme")
  (:includes "compiler-tools.scm"
             "compiler-fasm.scm"
             "compiler-expand.scm"
             "compiler-meaning.scm"
             "compiler-foptimize.scm"
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
            "*disable-load-unit-boundaries*"
            "compile-form"
            "compile"
            ))
