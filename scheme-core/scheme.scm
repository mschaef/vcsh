
;;;; scheme.scm --
;;;;
;;;; The main file defining the standard scheme image.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;;;; Compile with: <vcsh> -c --load-file:setup-image-compile.scm scheme.scm

;; Capture some relevent attriutes of the image build.

(define *scheme-build-date* #.(date->string (current-date) "~b ~d ~Y ~H:~M:~S"))

(define *scheme-build-vm* #.(system-info :vm-build-id))

(define *scheme-build-image* #.(system-info :scheme-build-id))

(define *scheme-build-version* "Scheme 0.50")

;;; Load definitions for subrs and VM-specific constants

(include "../vm/scan-constants.scm")
(include "subrs.scm")

;;; Set up the default package structure


(define *package-list* (if (%symbol-globally-bound? '*package-list*)
                           *package-list* 
                           (cons (%control-field system::VMCTRL_PACKAGE_SYSTEM)
                                 (cons (%control-field system::VMCTRL_PACKAGE_SCHEME)
                                       (cons (%control-field system::VMCTRL_PACKAGE_KEYWORD))))))

(define *package* (if (%symbol-globally-bound? '*package*)
                      *package*
                      (%control-field system::VMCTRL_PACKAGE_SCHEME)))

(%set-package-use-list! (%control-field system::VMCTRL_PACKAGE_SCHEME)
                        (cons (%control-field system::VMCTRL_PACKAGE_SYSTEM)))

;;; Now, we're ready to start processing official 'load units'

(compiler::%%begin-load-unit-boundaries "scheme")

;;; A scheme build includes the following files for basic functionality.

(include "mvalues.scm")
(include "list.scm")
(include "control-flow.scm")
(include "procedure.scm")
(include "properties.scm")
(include "character.scm")
(include "hash.scm")
(include "sets.scm")
(include "number.scm")
(include "string.scm")
(include "macro.scm")
(include "text-dictionary.scm")
(include "package.scm")
(include "hooks.scm")
(include "class-graph.scm")
(include "structure.scm")
(include "iterate.scm")
(include "error-handling.scm")
(include "io.scm")
(include "generic-functions.scm")
(include "EQUAL.scm")
(include "memoize.scm")
(include "match.scm")
(include "quasiquote.scm")
(include "reader.scm")
(include "printer.scm")
(include "system.scm")
(include "fast-write.scm")
(include "fasl-stream.scm")
(include "date-time.scm")
(include "exports.scm")
(include "memory.scm")
(include "compiler.scm")
(include "eval.scm")

(include "deferred-execution.scm")
(include "list-statistics.scm")
(include "repl.scm")
(include "tools.scm")
(include "inspect.scm")
(include "main.scm")
