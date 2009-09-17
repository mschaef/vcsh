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
  (:exports "define-vcalc-command"
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
            "apply-to-stack"
            "string->vc-object"
            "edit-text"
            "check-text"
            "keymap-bind-sequence!"
            "*global-keymap*"
            "*busy-keymap*"
            "keymap-describe-sequence!"
            "update"
            "write-to-string"
            "stack-empty?"
            "*number-precision*"
            "*number-format-mode*"
            "*seperator-mode*"
            "objects->postfix-program"
            ))
  
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
  )

;;; Initialize I/O to point to the correct places

(set-current-input-port *console-input-port*)
(set-current-error-port *console-error-port*)
(set-current-debug-port *console-error-port*)
(set-current-output-port *console-output-port*)

(define (run)
  ;(set-current-error-port scheme::*console-error-port*)
  ; (set-current-output-port scheme::*console-output-port*)
  ;(vcalc-boot)
  (in-package! "vcalc")
  (vcalc-boot)
  (repl))