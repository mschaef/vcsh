;;;; vcinit.scm
;;;; Mike Schaeffer
;;
;; vCalc Initialization code

 
(define-package "vcalc"
  (:uses "scheme")
  (:includes "subrs.scm"
             "utils.scm"
             "standard-windows.scm"
             "colors.scm"
             "formatted-text.scm"
             "core.scm"
             "printer.scm"
             "postfix.scm"
             "keymap.scm"
             "vcalc-commands.scm"
             "default-keymap.scm"
             "vcalc-window.scm"
             )
  (:exports "command-modes"
            "define-vcalc-command"
            "defconfig"
            "load-time-define"
            "vc-object->f-text"
            "write-vc-object"
            "describe-object"
            "*angle-mode*"
            "*default-base*"
            "*interest-accrual-mode*"
            "interactively-evaluate-objects"
            "*constant-library*"
            "*last-stack*"
            "*redo-stack*"
            "*last-stack-limit*"
            "*last-arguments*"
            "*stack*"
            "*current-window*"
            "*registers*"
            "apply-to-stack"
            "string->vc-object"
            "vc-object->string"
            "edit-text"
            "check-text"
            "keymap-bind-sequence!"
            "*global-keymap*"
            "*busy-keymap*"
            "keymap-describe-sequence!"
            "update"
            "write-to-string"
            "stack-empty?"
            "stack-depth"
            "stack-top"
            "stack-push"
            "stack-pop"
            "stack-ensure-arguments"
            "*number-precision*"
            "*number-format-mode*"
            "*seperator-mode*"
            "objects->postfix-program"
            "*recording-macro*"
            "*current-macro-seq*"
            "vc-error"
            "*register-watch-list*"
            "toggle-key-help"
            "last-key-number"
            "yes-or-no?"
            ))

(define *debug* #f)

;;;; Bootstrap process

(define (setup-user-package)
  (when (find-package "vcalc-user")
    (delete-package! "vcalc-user"))
  (let ((p (make-package! "vcalc-user")))
    (use-package! "vcalc" p)
    (use-package! "vcalc-commands" p)
    (install-config-variables! p)
    (reset-config-variables!)))

(define (vcalc-boot)
  (init-busy-keymap)
  (init-global-keymap)
  (setup-user-package)
  (init-vcalc-stack-window)
  (maybe-load-persistant-state)
  (ensure-visible-stack-window)
  )

;;; Initialize I/O to point to the correct places

(set-current-input-port *console-input-port*)
(set-current-error-port *console-error-port*)
(set-current-debug-port *console-error-port*)
(set-current-output-port *console-output-port*)

(define (run)
  ;(set-current-error-port scheme::*console-error-port*)
  ; (set-current-output-port scheme::*console-output-port*)
  (in-package! "vcalc-user")
  (vcalc-boot)
  (repl))