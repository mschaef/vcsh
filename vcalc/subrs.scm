;;;; subrs.scm
;;;; Mike Schaeffer
;;
;; vCalc subr definitions.

(defmacro (load-time-define symbol binding)
  (unless (symbol? symbol)
    (error "Load time definitions only support simple symbols for names: ~s" symbol))
  `(begin
     (define ,symbol :load-time)
     (eval-when (:load-toplevel)
      (set! ,symbol ,binding))))


(load-time-define show-console (scheme::%subr-by-name "show-console"))
(load-time-define hide-console (scheme::%subr-by-name "hide-console"))
(load-time-define get-run-mode (scheme::%subr-by-name "get-run-mode"))
(load-time-define do-register (scheme::%subr-by-name "do-register"))
(load-time-define about-box (scheme::%subr-by-name "about-box"))
(load-time-define set-application-busy! (scheme::%subr-by-name "set-application-busy!"))
(load-time-define pump-messages (scheme::%subr-by-name "pump-messages"))

(load-time-define scheme::%set-timer-event-time (scheme::%subr-by-name "%set-timer-event-time"))

(load-time-define make-image (scheme::%subr-by-name "make-image"))
(load-time-define measure-image (scheme::%subr-by-name "measure-image"))

(load-time-define image-open! (scheme::%subr-by-name "image-open!"))
(load-time-define image-close! (scheme::%subr-by-name "image-close!"))

(load-time-define draw-line (scheme::%subr-by-name "draw-line"))
(load-time-define draw-polyline (scheme::%subr-by-name "draw-polyline"))
(load-time-define draw-rectangle (scheme::%subr-by-name "draw-rectangle"))
(load-time-define fill-rectangle (scheme::%subr-by-name "fill-rectangle"))
(load-time-define draw-gradient (scheme::%subr-by-name "draw-gradient"))
(load-time-define draw-ellipse (scheme::%subr-by-name "draw-ellipse"))
(load-time-define fill-ellipse (scheme::%subr-by-name "fill-ellipse"))
(load-time-define draw-point (scheme::%subr-by-name "draw-point"))
(load-time-define draw-image (scheme::%subr-by-name "draw-image"))
(load-time-define copy-image (scheme::%subr-by-name "copy-image"))
(load-time-define subimage (scheme::%subr-by-name "subimage"))
(load-time-define draw-text (scheme::%subr-by-name "draw-text"))
(load-time-define measure-text (scheme::%subr-by-name "measure-text"))

(load-time-define set-drawing-origin! (scheme::%subr-by-name "set-drawing-origin!"))
(load-time-define update-drawing-origin! (scheme::%subr-by-name "update-drawing-origin!"))
(load-time-define get-drawing-origin (scheme::%subr-by-name "get-drawing-origin"))

(load-time-define set-foreground-color! (scheme::%subr-by-name "set-foreground-color!"))
(load-time-define set-background-color! (scheme::%subr-by-name "set-background-color!"))
(load-time-define set-font! (scheme::%subr-by-name "set-font!"))
(load-time-define point-scale (scheme::%subr-by-name "point-scale"))

(load-time-define draw-linear-text (scheme::%subr-by-name "draw-linear-text"))
(load-time-define measure-linear-text (scheme::%subr-by-name "measure-linear-text"))

(load-time-define font (scheme::%subr-by-name "font"))

(load-time-define rgb-color (scheme::%subr-by-name "rgb-color"))
(load-time-define color-r (scheme::%subr-by-name "color-r"))
(load-time-define color-g (scheme::%subr-by-name "color-g"))
(load-time-define color-b (scheme::%subr-by-name "color-b"))

(load-time-define beep (scheme::%subr-by-name "beep"))
(load-time-define exit-application (scheme::%subr-by-name "exit-application"))
(load-time-define system-string (scheme::%subr-by-name "system-string"))
(load-time-define %choose-file (scheme::%subr-by-name "%choose-file"))
(load-time-define yes-or-no? (scheme::%subr-by-name "yes-or-no?"))
(load-time-define message (scheme::%subr-by-name "message"))
(load-time-define get-clipboard-formats (scheme::%subr-by-name "get-clipboard-formats"))
(load-time-define set-clipboard-data (scheme::%subr-by-name "set-clipboard-data"))
(load-time-define get-clipboard-data (scheme::%subr-by-name "get-clipboard-data"))
(load-time-define get-user-file-path (scheme::%subr-by-name "get-user-file-path"))
(load-time-define %make-window (scheme::%subr-by-name "%make-window"))
(load-time-define show-window (scheme::%subr-by-name "show-window"))
(load-time-define hide-window (scheme::%subr-by-name "hide-window"))
(load-time-define window-size (scheme::%subr-by-name "window-size"))
(load-time-define close-window (scheme::%subr-by-name "close-window"))
(load-time-define update-window (scheme::%subr-by-name "update-window"))
(load-time-define flush-window (scheme::%subr-by-name "flush-window"))
(load-time-define open-editor (scheme::%subr-by-name "open-editor"))
(load-time-define close-editor (scheme::%subr-by-name "close-editor"))
(load-time-define window-placement (scheme::%subr-by-name "window-placement"))
(load-time-define set-window-placement! (scheme::%subr-by-name "set-window-placement!"))
(load-time-define set-status-text! (scheme::%subr-by-name "set-status-text!"))
(load-time-define parent-repositioned (scheme::%subr-by-name "parent-repositioned"))
(load-time-define key-name->key-id (scheme::%subr-by-name "key-name->key-id"))
(load-time-define key-id->key-name (scheme::%subr-by-name "key-id->key-name"))
(load-time-define key-id->natural-number (scheme::%subr-by-name "key-id->natural-number"))
(load-time-define key-id->character (scheme::%subr-by-name "key-id->character"))
(load-time-define show-config-dialog (scheme::%subr-by-name "show-config-dialog"))
(load-time-define show-tip (scheme::%subr-by-name "show-tip"))
(load-time-define choose (scheme::%subr-by-name "choose"))

(load-time-define edit-text (scheme::%subr-by-name "edit-text"))
(load-time-define show-config-dialog (scheme::%subr-by-name "show-config-dialog"))
