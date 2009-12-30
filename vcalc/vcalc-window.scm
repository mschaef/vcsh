;;;; vcalc-window.scm
;;;; Mike Schaeffer
;;
;; This is the main vCalc Window.

(define *busy-keymap* #f)
(define *global-keymap* #f)

(define *vcalc-state-file* "vcalc-savestate.vcx")


(define (save-persistant-state)
  (save-state-to-file (get-user-file-path *vcalc-state-file*)))

(define (maybe-load-persistant-state)
  (catch 'abort-load
    (handler-bind ((read-error (lambda args
				 (info "Persistant state load aborted!!!")
				 (throw 'abort-load))))
      (when (file-exists? (get-user-file-path *vcalc-state-file*))
	(load-state-from-file (get-user-file-path *vcalc-state-file*))))))

(define (do-quit)
  (exit-application))


(define (choose-file parent-window loading? specs)
  "Display a file chooser for <parent-window>. If <loading?> is #t, the
   window is configured for loading a file, otherwise for saving. <specs>
   Is an a-list of acceptable file types and their descriptions (<desc> . <extention>), 
   #t is permissible in this list to allow all files."
  (define (w32-file-dialog-filter specs)
    (let ((write-buffer (open-output-string))
	  (default-filename #f))
      (define (next-spec remaining need-seperator?)
	(cond ((null? remaining)
	       (cons default-filename (string-append (get-output-string write-buffer) "||")))
	      (#t
	       (when need-seperator? (format write-buffer "|"))
	       (cond ((and (boolean? (car remaining)) (car remaining))
			(unless default-filename
			  (set! default-filename "*.*"))
		      (format write-buffer "All Files (*.*)|*.*")
		      (next-spec (cdr remaining) #t))
		     ((and (pair? (car remaining)) (string? (caar remaining)) (string? (cdar remaining)))
		      (let ((spec (car remaining)))
			(unless default-filename
			  (set! default-filename (cdr spec)))
			(format write-buffer "~a (~a)|~a" (car spec) (cdr spec) (cdr spec))
			(next-spec (cdr remaining) #t)))
		     (#t
		      (error "Invalid dialog filter specification" (car remaining)))))))
      (next-spec specs #f)))

  (let* ((default-fn/spec-string (w32-file-dialog-filter specs))
	 (spec-string (cdr default-fn/spec-string))
	 (filename (car default-fn/spec-string))
	 (extension (filename-extension filename)))
    (format #t "default-fn/spec-string =~a\n" default-fn/spec-string (w32-file-dialog-filter specs))

    (%choose-file @(parent-window native-peer) 
                  loading? spec-string extension filename)))

(define *current-window* #f)

(define-proto (<keyhelp-drawer> <drawer>)
  )

(define-proto (<vcalc-window> <lisp-window>)
  'window-type :vcalc-window
  )
	 
(defconfig *tip-of-the-day-status* '(#t . 0))

(define (show-tip-of-the-day parent)
  (set! *tip-of-the-day-status*
	(show-tip @(parent native-peer) *tip-of-the-day-status*)))

(define (maybe-show-tip-of-the-day)
  (when (not (car *tip-of-the-day-status*))
    (show-tip-of-the-day)))

(defmesg <vcalc-window> (cmd-window-close arg)
  (save-persistant-state)
  (do-quit)
  #t)

(define (load-file-cmd filename)
  (let ((type (string-downcase (filename-extension filename))))
    (cond ((equal? type "vcx") (load-state-from-file filename))
	  ((equal? type "csv") (stack-push (read-csv-file filename)))
	  (#t (vc-error "I don't know how to handle this kind of file.")))))

(defmesg <vcalc-window> (cmd-quit arg)
  (save-persistant-state)
  (do-quit))

(defmesg <vcalc-window> (cmd-file-dropped filename)
  (load-file-cmd filename)
  [self update])

(defmesg <vcalc-window> (cmd-show-options arg)
  (do-config w))

(defmesg <vcalc-window> (cmd-register-vcalc arg)
  (do-register @native-peer))

(defmesg <vcalc-window> (cmd-about-vcalc arg)
  (about-box @native-peer))

(defmesg <vcalc-window> (cmd-load-state arg)
  (awhen (choose-file self #t '(("vCalc Save Files" . "*.vcx") 
                                ("Comma Seperated Data Files" . "*.csv") 
                                #t))
    (load-file-cmd it)
    [self update]))


(defmesg <vcalc-window> (cmd-save-state arg)
  (save-persistant-state))

(defmesg <vcalc-window> (cmd-save-state-as arg)
  (awhen (choose-file self #f '(("vCalc Save Files" . "*.vcx") #t))
    (save-state-to-file it #f)
    [self update]))

(defmesg <vcalc-window> (cmd-toggle-keyboard-commands arg)
  (toggle-key-help))

(define-vcalc-command (toggle-key-help)
  "TODO: NEEDS DOCS"
  (let ((keyhelp-drawer @(*current-window* keyhelp-drawer)))
    (let ((key-help-placement [keyhelp-drawer placement]))
      (if (vector-ref key-help-placement 1)
          [keyhelp-drawer show]
          [keyhelp-drawer hide]))
    (values)))

(defmesg <vcalc-window> (cmd-showtip arg)
  (show-tip-of-the-day self))

(defmesg <vcalc-window> (cmd-cut arg)
  (apply-to-stack stack-cut)
  [self update])

(defmesg <vcalc-window> (cmd-copy arg)
  (apply-to-stack stack-copy)
  [self update])

(defmesg <vcalc-window> (cmd-paste arg)
  (apply-to-stack stack-paste)
  [self update])

(defmesg <vcalc-window> (cmd-laststack arg)
  (apply-to-stack last-stack)
  [self update])

(defmesg <vcalc-window> (cmd-redostack arg)
  (apply-to-stack redo-stack)
  [self update])

(defmesg <vcalc-window> (on-keypress key-info)
  (format #t "; keypress=~s\n" key-info)
  (push-key! key-info)
  (let ((binding (current-key-binding)))
    (cond ((not binding)
	   (beep)
	   (reset-current-key-sequence!))
	  ((keymap? binding)
	   )
	  (#t
	   (catch-all
	    (reset-current-key-sequence!)
            (dynamic-let ((*current-window* self))
              (interactively-evaluate-objects binding))))))
  [self update]
  [@keyhelp-drawer update])

(define (init-vcalc-stack-window)
  (let* ((w [<vcalc-window> create])
         (d [<keyhelp-drawer> create 'parent w]))
    (slot-set! w 'keyhelp-drawer d)
    [w show]
    [d show]
    w))

(define (make-keyspec-ftext keyspec binding)
  `("Arial" (18 (:bold ,keyspec) " - " ,binding)))


(define (draw-keyboard-help window-w window-h)
  (let ((sequence-prefix '())
	(y 0))

    ; The rest of the key help pane is occupied by a list of the currently
    ; available key bindings. These are printed in black on yellow text.
    (set-color! *color-keyhelp-background*)
    (draw-gradient (point 0 0) (point window-w window-h)
		   (list *color-keyhelp-g/from* *color-keyhelp-g/to* #f))

    (dolist (key-info (current-key-sequence))
      (set! sequence-prefix (append! sequence-prefix (list key-info)))
      (let* ((key-id key-info)
	     (key-binding  (keymap-sequence-binding *global-keymap* sequence-prefix))
	     (key-ftext (make-keyspec-ftext (describe-keyspec-or-id key-id)
					    (describe-object key-binding)))
	     (ys (cadr (measure-formatted-text key-ftext))))
	
	(set-color! *color-dark-blue*)
	(fill-rectangle (point 0 y) (point window-w (+ y ys)))

	(set-foreground-color! *color-yellow*)
	(draw-anchored-text (point 10 (+ y (/ ys 2))) :w key-ftext)
	(incr! y ys)))
  
    
    (set-foreground-color! *color-keyhelp-text*)
    
    (let ((current-keymap (current-key-binding)))

      (when (keymap? current-keymap)
	; Formatted text descriptions of each key binding are cached in the keymaps.
	(let ((descriptions (keymap-user-cache current-keymap)))
	  (when (null? descriptions)
	    (set! descriptions 
		  (keymap-map
		   (lambda (key-id binding binding-description)
		     (if binding-description
			 (cons (if (symbol? binding) 
				   (disabled-premium-binding? binding) 
				   #f)
			       (make-keyspec-ftext (describe-keyspec-or-id key-id)
						   (cond ((boolean? binding-description)
							  (describe-object binding))
							 ((string? binding-description)
							  binding-description)
							 (#t
							  (error "Bad binding description, ~a for binding ~a."
								 binding-description
								 binding)))))
			 '()))		   current-keymap))
	    (set-keymap-user-cache! current-keymap descriptions))

	  (dolist (binding-desc descriptions)
	    (unless (null? binding-desc)
	      (let ((ys (cadr (measure-formatted-text (cdr binding-desc)))))
		  
		(when (car binding-desc)
		  (set-color! *gray-80*)
		  (fill-rectangle (point 0 y) (point window-w (+ y ys)))
		  (set-background-color! *color-keyhelp-background*)
		  (set-foreground-color! *color-dark-yellow*))
		
		(draw-anchored-text (point 10 (+ y (/ ys 2))) :w (cdr binding-desc))
		(incr! y ys)))))))))

(defmesg <keyhelp-drawer> (on-update)
  (with-window-image self
     (draw-keyboard-help @width @height))
  [self flush])

(defconfig *eval-time-display* #f)
(defconfig *eval-trace* #f)

(defconfig *register-watch-list* '())

(defmesg <vcalc-window> (on-update)
  (let ((w @native-peer))
    (set-status-text! w 1 (angle-mode-text))
    (set-status-text! w 2 (default-base-text))
    (set-status-text! w 3 (macro-recorder-text))
  
    (when *eval-time-display*
      (set-status-text! w 4 *last-eval-time*)))

  (with-window-image self
    (let ((window-w @width)
	  (window-h @height))

      (define (draw-register-watches y registers)
	(cond ((null? registers)
	       (set-color! *color-watch-highlight*)
	       (draw-line (point 0 y) (point window-w y)))
	      (#t
	       (let* ((reg (car registers))
		      (ftext (list #f
				   (format #f "r[~a]=" (inexact->exact reg))
				   (vc-object->f-text (vector-ref *registers* reg))))
		      (ys (cadr (measure-formatted-text ftext))))
		 
		 (set-color! *color-watch-background*)
		 (fill-rectangle (point 0 y) (point window-w (+ y ys)))
		 (set-color! *color-black*)
		 (draw-anchored-text (point 10 y) :nw ftext)
		 (draw-register-watches (+ y ys) (cdr registers))))))

      (define (draw-stack y stack light-line?)
	(unless (null? stack)
	  (let* ((elem (car stack))
		 (ftext (vc-object->f-text elem))
		 (ys (cadr (measure-formatted-text ftext))))


	    (if (< y (- ys)) (throw 'box-full 0))

	    (set-color! (if light-line? *color-stack-light* *color-stack-dark*))
	    (fill-rectangle (point 0 (- y ys)) (point window-w y))

	    (set-foreground-color! *color-black*)
	    (draw-anchored-text (point (- window-w 10) (- y ys)) :ne ftext)
	    (draw-stack (- y ys) (cdr stack) (not light-line?)))))

      (set-color! *color-black*)
      (draw-gradient (point 0 0) (point window-w window-h)
		     (list *color-background-g/from* *color-background-g/to* #f))
      (catch 'box-full
        (draw-stack window-h *stack* #f))

      (draw-register-watches 0 *register-watch-list*)

      [self flush])))

(define (read-multiple string reader)
  (let ((ip (open-input-string string)))
    (let recur ((next (reader ip)))
      (if (eof-object? next)
          ()
          (cons next (recur (reader ip)))))))

(defmesg <vcalc-window> (commit-editor-text)
  (let ((editor-text (close-editor @native-peer)))
    (with-stack-transaction #t
       (apply interactively-evaluate-objects (read-multiple editor-text vc-read)))
    [self update]))

(defmesg <vcalc-window> (begin-editor-with letter)
  (open-editor @native-peer (character->string letter)))

(defmesg <vcalc-window> (on-edit-keypress key-id)
  (let ((key-desc (last (key-id->key-name key-id))))
    (case key-desc
      ((:key-return)
       [self commit-editor-text]
       #f)
      ((:key-escape)
       (close-editor @native-peer)
       #f)
      ((not (char-edit-pred letter))
       [self commit-editor-text]
       [self on-keypress #f key-id]
       #f)
      (#t
       (if (char-edit-pred (key-char key-id))
           #t
           (begin 
             (commit-editor-text)
             [self on-keypress #f key-id]
             #f))))))


(define char-edit-pred
  (lambda (ch)
    (or (and (char-ci>=? ch #\0)
	     (char-ci<=? ch #\9))
	(and (char-ci>=? ch #\a)
	     (char-ci<=? ch #\f))
	(char-ci=? ch #\b)
	(char-ci=? ch #\x)
	(char-ci=? ch #\o)
	(char-ci=? ch #\.)
	(char-ci=? ch #\bs))))

(define-vcalc-command (begin-editor-with-last-keystroke) 
  "Begins an in window editor containing the character generated by the
   last keystroke."
  (command-modes :no-stack-transaction :not-recordable)
  [*current-window*  begin-editor-with (key-char (last-keystroke))]
  (values))

(define (vc-read/interactive string)
  (handler-bind ((read-error (lambda (error-type port location)
			       (message (format #f "~s @ ~s" error-type location) "Invalid Object Format!"))))
    (vc-read string)))

(define (check-text text reformat?)
  (catch 'bad-check
    (handler-bind ((read-error (lambda args (throw 'bad-check #f))))
      (let ((object (vc-read/interactive text)))
	(if reformat?
	    (vc-object->string object)
	    text)))))
