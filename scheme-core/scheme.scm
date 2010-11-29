;;;; scheme.scm
;;;;
;;;; The main file defining the standard scheme image.
;;;;
;;;; Compile with: <vcsh> -c scheme.scm --cross-compile --initial-package:scheme --output:scheme.scf --no-load-unit-boundaries

;; Capture some relevent attriutes of the image build.

(define *scheme-build-date* #.(date->string (current-date) "~b ~d ~Y ~H:~M:~S"))

(define *scheme-build-vm* #.(system-info :vm-build-id))

(define *scheme-build-image* #.(system-info :scheme-build-id))

(define *scheme-build-version* "Scheme 0.50")

;;; Load definitions for subrs and VM-specific constants

(include "../vm/constants.scm")
(include "subrs.scm")

;;; Set up the default package structure

(define *package* (%control-field system::VMCTRL_PACKAGE_SCHEME))

(define *package-list* (cons (%control-field system::VMCTRL_PACKAGE_SYSTEM)
                             (cons (%control-field system::VMCTRL_PACKAGE_SCHEME)
                                   (cons (%control-field system::VMCTRL_PACKAGE_KEYWORD)))))

(define (%current-package-list) *package-list*)

(define (%set-current-package-list! packages)
  (%set-fasl-package-list! packages)
  (set! *package-list* packages)
  packages)

(%set-package-use-list! (%control-field system::VMCTRL_PACKAGE_SCHEME)
                        (cons (%control-field system::VMCTRL_PACKAGE_SYSTEM)))

;;; Now, we're ready to start processing official 'load units'

(compiler::%%begin-load-unit-boundaries "scheme")

;;; A scheme build includes the following files for basic functionality.

(include "character.scm")
(include "class-graph.scm")
(include "control-flow.scm")
(include "higher-order.scm")
(include "EQUAL.scm")
(include "error-handling.scm")
(include "hash.scm")
(include "io.scm")
(include "list.scm")
(include "memoize.scm")
(include "number.scm")
(include "package.scm")
(include "procedure.scm")
(include "printer.scm")
(include "quasiquote.scm")
(include "string.scm")
(include "system.scm")
(include "structure.scm")
(include "macro.scm")
(include "iterate.scm")
(include "reader.scm")
(include "fasl-write.scm")
(include "fast-op.scm")
(include "date-time.scm")
(include "exports.scm")
(include "compiler.scm")
(include "deferred-execution.scm")
(include "repl.scm")
(include "tools.scm")
(include "inspect.scm")
(include "main.scm")
