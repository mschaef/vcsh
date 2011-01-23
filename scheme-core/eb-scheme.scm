;;;; scheme.scm
;;;;
;;;; The main file defining the standard scheme image.
;;;;
;;;; Compile with: <vcsh> -c --load-file:setup-image-compile.scm scheme.scm

;; Capture some relevent attriutes of the image build.

(%define *scheme-build-date* #.(host-scheme::date->string (host-scheme::current-date) "~b ~d ~Y ~H:~M:~S"))

(%define *scheme-build-vm* #.(host-scheme::system-info :vm-build-id))

(%define *scheme-build-image* #.(host-scheme::system-info :scheme-build-id))

(%define *scheme-build-version* "Scheme 0.50")

;;; Load definitions for subrs and VM-specific constants

(host-compiler::include "../vm/constants.scm")
(host-compiler::include "subrs.scm")

;;; Set up the default package structure

(%define *package-list* (if (%symbol-globally-bound? '*package-list*)
                           *package-list* 
                           (cons (%control-field system::VMCTRL_PACKAGE_SYSTEM)
                                 (cons (%control-field system::VMCTRL_PACKAGE_SCHEME)
                                       (cons (%control-field system::VMCTRL_PACKAGE_KEYWORD))))))

(%define *package* (if (%symbol-globally-bound? '*package*)
                      *package*
                      (%control-field system::VMCTRL_PACKAGE_SCHEME)))

(%set-package-use-list! (%control-field system::VMCTRL_PACKAGE_SCHEME)
                        (cons (%control-field system::VMCTRL_PACKAGE_SYSTEM)))

;;; At this point we have enough defined that we can bring in a few bootstrap forms
;;; that'll help us compile the rest of the image.
(host-scheme::eval-when (:compile-toplevel)
  (host-scheme::load "image-bootstrap.scm"))

;; Point the package protection functions to the right *package*. (see compiler-file.scm for more info)
(%early-define (system::LOAD-TIME-GET-PACKAGE) scheme::*package*)
(%early-define (system::LOAD-TIME-SET-PACKAGE! package) (set! *package* scheme::package))

;;; Now, we're ready to start processing official 'load units'

(host-compiler::%%begin-load-unit-boundaries "scheme")

;;; A scheme build includes the following files for basic functionality.

(host-compiler::include "core.scm")

;; (include "character.scm")
;; (include "class-graph.scm")
;; (include "instance.scm")
;; (include "generic-functions.scm")
;; (include "control-flow.scm")
;; (include "EQUAL.scm")
;; (include "error-handling.scm")
;; (include "hash.scm")
;; (include "sets.scm")
;; (include "io.scm")
;; (include "list.scm")
;; (include "memoize.scm")
;; (include "number.scm")
;; (include "list-statistics.scm")
;; (include "hooks.scm")
;; (include "package.scm")
;; (include "procedure.scm")
;; (include "mvalues.scm")
;; (include "printer.scm")
;; (include "quasiquote.scm")
;; (include "string.scm")
;; (include "text-dictionary.scm")
;; (include "system.scm")
;; (include "structure.scm")
;; (include "macro.scm")
;; (include "iterate.scm")
;; (include "reader.scm")
;; (include "fasl-write.scm")
;; (include "date-time.scm")
;; (include "exports.scm")

;; (include "compiler.scm")
;; (include "eval.scm")

;; (include "deferred-execution.scm")
;; (include "repl.scm")
;; (include "tools.scm")
;; (include "inspect.scm")
;; (include "main.scm")
