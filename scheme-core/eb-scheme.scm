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

;; (host-compiler::include "list.scm")
;; (host-compiler::include "control-flow.scm")
;; (host-compiler::include "procedure.scm")
;; (host-compiler::include "character.scm")
;; (host-compiler::include "hash.scm")
;; (host-compiler::include "sets.scm")
;; (host-compiler::include "number.scm")
;; (host-compiler::include "string.scm")
;; (host-compiler::include "macro.scm")
;; (host-compiler::include "structure.scm")
;; (host-compiler::include "text-dictionary.scm")
;; (host-compiler::include "class-graph.scm")
;; (host-compiler::include "instance.scm")
;; (host-compiler::include "error-handling.scm")
;; (host-compiler::include "io.scm")
;; (host-compiler::include "generic-functions.scm")
;; (host-compiler::include "EQUAL.scm")
;; (host-compiler::include "memoize.scm")
;; (host-compiler::include "match.scm")
;; (host-compiler::include "hooks.scm")
;; (host-compiler::include "package.scm")
;; (host-compiler::include "printer.scm")
;; (host-compiler::include "system.scm")
;; (host-compiler::include "iterate.scm")
;; (host-compiler::include "reader.scm")
;; (host-compiler::include "quasiquote.scm")
;; (host-compiler::include "fasl-write.scm")
;; (host-compiler::include "date-time.scm")
;; (host-compiler::include "exports.scm")
;; (host-compiler::include "compiler.scm")
;; (host-compiler::include "eval.scm")

;; (host-compiler::include "deferred-execution.scm")
;; (host-compiler::include "list-statistics.scm")
;; (host-compiler::include "repl.scm")
;; (host-compiler::include "tools.scm")
;; (host-compiler::include "inspect.scm")
;; (host-compiler::include "main.scm")
