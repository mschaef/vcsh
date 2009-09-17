;;;; keymap.scm
;;;; Mike Schaeffer
;;
;; vCalc keymaps. These are used to bind sequences of keystrokes to 
;; commands.

       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Keymap
;;;
;;; A keymap is a key->action binding mechanism for sequences of
;;; keystrokes.
;;;
;;; A key sequence specification is a list of key specifications

(define (key-char key)
  "Returns the character produced by key <key>."
  (cond ((or (symbol? key) (list? key)) 
	 (key-char (key-name->key-id key)))
	((or (exact? key) (char? key))
	 (key-id->character key))
	(#t
	 (error "Invalid key: ~a" key))))
	 
(define (key=? key-1 key-2)
  "Equivalence test for keys. Two keys are considered to be the same
   if they 1) generate the same character or 2) are the same physical
   key."
  (cond ((or (char? key-1) (char? key-2))
	 (eq? (key-char key-1) (key-char key-2)))
	((and (exact? key-1) (exact? key-2))
	 (= key-1 key-2))
	((or (symbol? key-1) (list? key-1)) (key=? (key-name->key-id key-1) key-2))
	((or (symbol? key-2) (list? key-2)) (key=? key-1 (key-name->key-id key-2)))
	((not (exact? key-1)) (error "Invalid key: ~a" key-1))
	((not (exact? key-2)) (error "Invalid key: ~a" key-2))
	(#t (= key-1 key-2))))


(define-structure keymap
  (name       :default #f)
  (bindings   :default '())
  (user-cache :default #f))

(define-method (describe-object (self keymap))
  (list :bold (keymap-name self)))

(define (keymap-ref keymap key-name)
  "Looks for the key <key-name> in the keymap <keymap>. Returns the (<binding> . <description>)
   if a binding exists, #f otherwise."
  (aif (find (lambda (map-entry) (key=? key-name (car map-entry)))
	     (keymap-bindings keymap))
    (cdr it)
    #f))

(define (keymap-set! keymap key-name binding binding-description)
  "Sets the binding to a keymap from the key <keyspec> to the object <binding>, with the
   description <binding-description>. A <binding-description> of #t uses the object's
   description as the binding's description, #f suppresses the binding from display,
   and a string is the description. Returns the keymap node for the binding."
  (keymap-clear-cache! keymap)
  (aif (keymap-ref keymap key-name)
    (begin
      (set-car! it binding)
      (set-cdr! it binding-description)
      it)

    (let ((new-node (cons binding binding-description)))
      (set-keymap-bindings! keymap (alist-cons (key-name->key-id key-name) 
                                               new-node
                                               (keymap-bindings keymap)))
      new-node)))

(define (keymap-clear-cache! keymap)
  (set-keymap-user-cache! keymap ()))

(define (all-bound-commands :optional (keymap *global-keymap*))
  "Computes the set of all commands bound through <keymap>, including
   any sub-keymaps."
  (cond ((keymap? keymap)
	 (append-map (lambda (binding-record)
		(all-bound-commands (cadr binding-record)))
	      (keymap-bindings keymap)))
	((symbol? keymap)
	 (cons keymap))
	(#t
	 #f)))

(define (all-bound-non-gf-commands)
  "Displays a list of all bound commands that are not generic functions."
  (table (list->set (filter (lambda (c) 
			      (not (generic-function? (symbol-value c)))) 
			    (all-bound-commands *global-keymap*)))))
		  

(define (ensure-key-name key-name-or-id)
  "Coerces <key-name-or-id> into the list key-name format used by keymap clients."
  (cond ((exact? key-name-or-id)
	 (key-id->key-name key-name-or-id))
	((atom? key-name-or-id)
	 (list key-name-or-id))
	(#t
	 (key-name-or-id))))


(define (keymap-sequence-node keymap key-sequence create-path?)
  "Looks for the sequence node in <keymap> for the sequence <key-sequence>.
   If the node does not exist, returns #f unless <create-path?> is true.
   If <create-path?> is true, all intermediate keymaps are created and
   a final binding is created between the key sequence and #f.  Descriptions
   are set to #f."
  (define (next-key current-keymap-1 remaining-sequence)
    (let ((keymap-entry (keymap-ref current-keymap-1 (car remaining-sequence))))
      (cond ((null? (cdr remaining-sequence))
	     (if (and (not keymap-entry) create-path?)
		 (keymap-set! current-keymap-1 (car remaining-sequence) #f #f)
		 keymap-entry))
	    (keymap-entry
	     (unless (keymap? (car keymap-entry))
	       (if (and create-path? (not (car keymap-entry)))
		   (set-car! keymap-entry (make-keymap :name "")) ; !!!!!
		   (error "This key sequence, ~a, requires redefining a keymap entry already bound to ~a." 
			  key-sequence (car keymap-entry))))
	     (next-key (car keymap-entry) (cdr remaining-sequence)))
	    (#t
	     (if create-path?
		 (let ((new-keymap (make-keymap :name #f)))
		   (keymap-set! current-keymap-1(car remaining-sequence) new-keymap #f)
		   (next-key new-keymap (cdr remaining-sequence)))
		 #f)))))
  (next-key keymap key-sequence))

(define (keymap-describe-sequence! keymap key-sequence description)
  (let ((sequence-node (keymap-sequence-node keymap key-sequence #t)))
    (set-cdr! sequence-node description)
    (when (not (car sequence-node))
      (set-car! sequence-node (make-keymap :name description)))))

(define (keymap-bind-sequence! keymap key-sequence binding . maybe-description)
  (let* ((description (if (null? maybe-description) #t (car maybe-description)))
	 (node (keymap-sequence-node keymap key-sequence #t)))
    (set-car! node binding)
    (set-cdr! node description)))

(define (keymap-sequence-binding keymap key-sequence)
  (aif (keymap-sequence-node keymap key-sequence #f)
    (car it)
    #f))

(define (keymap-sequence-description keymap key-sequence)
  (aif (keymap-sequence-node keymap key-sequence #f)
    (cdr it)
    #f))

(define (keymap-map fn keymap)
  "Map over all the sub bindings within a keymap"
  (map (lambda (binding-info)
	    (fn (car binding-info)    ; key-id
		(cadr binding-info)   ; binding
		(cddr binding-info))) ; binding-public?
	  (keymap-bindings keymap)))


(define (build-keymap name . bindings)
  "Build a new keymap named <name>. Bindings are specified as key/binding
   pairs in subsequent argments to build-keymap. Bindings can be a atom,
   in which case they are the binding, a list beginning with a string,
   in which case they are sub-keymaps, or a list with the binding and
   the documentation string."
  (define (add-remaining new-keymap remaining)
    (cond ((null? remaining)
	   new-keymap)
	  ((< (length remaining) 2)
	   (error "Invalid bindings list ~a, wrong length." bindings))
	  (#t
	   (let ((binding-key (car remaining))
		 (binding-object (cadr remaining)))
	     (cond ((atom? binding-object)
		    (keymap-set! new-keymap binding-key binding-object #t))
		   ((string? (car binding-object))
		    (keymap-set! new-keymap binding-key (apply build-keymap binding-object) (car binding-object)))
		   (#t
		    (keymap-set! new-keymap binding-key (car binding-object) (cadr binding-object)))))
	   (add-remaining new-keymap (cddr remaining)))))
  (check-class build-keymap name string)
  (add-remaining (make-keymap :name name) bindings))

;;; The current key sequence

(define *current-key-sequence* ())
(define *last-key-sequence* ())

(define (push-key! key-info)
  (set! *current-key-sequence*
	(append *current-key-sequence* (list key-info))))

(define (reset-current-key-sequence!)
  (set! *last-key-sequence* *current-key-sequence*)
  (set! *current-key-sequence* ()))

(define (current-key-sequence)
  *current-key-sequence*)

(define (current-key-binding)
  (if (null? *current-key-sequence*)
      *global-keymap*
      (keymap-sequence-binding *global-keymap*  *current-key-sequence*)))
  
(define (last-keystroke)
  (last *last-key-sequence*))

(define (last-key-number)
  (if (null? (last-keystroke))
      -1
      (key-id->natural-number (last-keystroke))))

(define *keyspec-strings*
  '((:shift . "Shift-")           (:control . "Control-")   
    (:key-back . "Backspace")     (:key-return . "Return")
    (:key-tab . "Tab")            (:key-f1 . "F1")
    (:key-f2 . "F2")              (:key-f3 . "F3")
    (:key-f4 . "F4")              (:key-f5 . "F5")
    (:key-f6 . "F6")              (:key-f7 . "F7")
    (:key-f8 . "F8")              (:key-f9 . "F9")
    (:key-f10 . "F10")            (:key-f11 . "F11")
    (:key-f11 . "F11")            (:key-f12 . "F12")
    (:key-menu .  "Alt")          (:key-pause . "Pause")
    (:key-capital . "Caps-Lock")  (:key-escape . "Escape")
    (:key-prior . "PgUp")         (:key-next . "PgDn")
    (:key-home . "Home")          (:key-end . "End")
    (:key-up . "Up")              (:key-down . "Down")
    (:key-left . "Left")          (:key-right . "Right")
    (:key-space . "Space")))

(define (describe-keyspec-or-id keyspec-or-id)
  (apply string-append (append! (list "[ ")
				(map (lambda (x)
					  (aif (assoc x *keyspec-strings*)
					       (cdr it)
					       (display-to-string x)))
					(ensure-key-name keyspec-or-id))
				(list " ]"))))

