;;;; core.scm
;;;; Mike Schaeffer
;;
;; This is the core of vCalc. It contains the implementation of the user stack,
;; the 'constant memory', and basic command processing.

;;; The vcalc error mechamism

;; Vcalc has a standard error handling protocol. This allows
;; vcalc runtime errors to be distinguished from Lisp errors,
;; which probably represent errors in vcalc itself.
(define *vcalc-error-object* #f)

(define (vc-error msg . args)
  (set! *vcalc-error-object* (cons msg args))
  (message (apply format (cons #f *vcalc-error-object*)) "Error!")
  (throw 'vcalc-error))

(define scheme::*reader-quotes-literal-lists* ()) ; TODO: figure out what this does, and how it should really be defined in the core

(define (vc-read ip)
  "The vcalc reader: reads one object from <ip> with the correct reader
   settings. <ip> can also be a string, in which case open-input-string
   is used to open a string input port."
  (if (string? ip)
      (vc-read (open-input-string ip))
      (dynamic-let ((scheme::*reader-defaults-to-flonum* #t)
                    (scheme::*reader-quotes-literal-lists* #t))
        (read ip))))



;; (defmacro (with-application-busy . code)
;;   (with-gensyms (previous-busy-sym block-complete-sym)
;;     `(let ((,previous-busy-sym :deferred)
;; 	   (,block-complete-sym #f))
;;        (dynamic-let ((*global-keymap* *busy-keymap*))
;; 	 (unwind-protect
;; 	  (lambda () 
;; 	    (in 0.2 (lambda () 
;; 		      (unless ,block-complete-sym
;; 			(update-window *keyhelp-window*)
;; 			(set! ,previous-busy-sym (set-application-busy #t)))))
;; 	    (begin-1 
;; 	     (begin ,@code)
;; 	     (set! ,block-complete-sym #t)))
;; 	  (lambda () 
;; 	    (unless (eq? ,previous-busy-sym :deferred)
;; 	      (set-application-busy ,previous-busy-sym))))))))

(defmacro (with-application-busy . code) `(begin ,@code))

;;; Commands and command registration.

(define (command-mode? command-fn mode)
  (if (memq mode (command-modes command-fn))
      #t
      #f))

(define (disabled-premium-binding? b)
  (and (not (get-run-mode))
       (procedure? (symbol-value b)) 
       (command-mode? (symbol-value b) :premium)))

(define (recordable? o) ; REVISIT: can be more encapsulated?
  (if (symbol? o)
      (not (command-mode? (symbol-value o) :not-recordable))
      #t))

(define *vcalc-commands* '())

(define (register-vcalc-command! command-name)
  (unless (member command-name *vcalc-commands*)
    (push! command-name *vcalc-commands*)))

(defmacro (define-vcalc-command lambda-list documentation-string . code)
  (unless (and (list? lambda-list) (symbol? (car lambda-list)))
    (error "vCalc command definitions must begin with a lambda list with a name in the first position: ~a" lambda-list))

  (unless (string? documentation-string)
    (error "vCalc command definitions must have a documentation-string: ~a" (car lambda-list)))

  (let ((code code)
        (modes ()))

    (when (aand (pair? code)
                (pair? (car code))
                (eq? (car it) 'command-modes))
      (set! modes (cdar code))
      (set! code (cdr code)))

    `(begin
       (register-vcalc-command! ',(car lambda-list))

       (define-generic-function ,lambda-list
         ,documentation-string
       
         ,@(if (null? code)
               `((vc-error "The command ~a can only be applied to one of the following:\n\n~a"
                           ',(car lambda-list)
                           (with-output-to-string
                             (dolist (signature (generic-function-signatures ,(car lambda-list)))
                               (format #t "* ~a\n" (signature-list->english signature))))))
               code))

       (set-property! ,(car lambda-list) 'command-modes ',modes))))

;;; Constant memory - vCalc remembers user state on shutdown and restores
;;; it on startup.

(define *config-variables* '())

(define (register-config-variable! name init-fn)
  (check symbol? name)
  (check procedure? init-fn)
  (unless (assq name *config-variables*)
    (push! (cons name init-fn) *config-variables*)))

(defmacro (defconfig name init-form)
  `(begin
     (define ,name ,init-form)
     (eval-when (:load-toplevel)
       (register-config-variable! ',name (lambda () ,init-form)))))

(define (install-config-variables! package)
  (check package? package)
  (dolist (vardef *config-variables*)
    (let ((var (car vardef))
          (init-fn (cdr vardef)))
      (set-symbol-value! var (init-fn))
      (import! var package))))

(define (reset-config-variables!)
  (dolist (vardef *config-variables*)
    (let ((var (car vardef))
          (init-fn (cdr vardef)))
      (set-symbol-value! var (init-fn)))))


(define (write-config-variables port)
  (dolist (varname (local-package-variables (find-package "vcalc-user")))
    (let ((val (symbol-value varname)))
      (write `(set! ,varname 
		    ,(if (pair? val)
			 (list 'quote val)
			 val))
		    port)
    (newline port)
    (newline port))))

(define (write-save-state port)
  (dynamic-let ((*print-shared-structure* #f)
                (*print-depth* #f)
                (*print-length* #f)
                (*print-readably* #t)
                (*print-addresses* #f)
                (*print-packages-always* #t))
    (format #t ";;; vCalc 1.1 Save File\n;;; Saved On ~a\n\n" (current-date))
    (write-config-variables port)))

(define (save-state-to-file filename)
  (let ((fileport (open-output-file filename)))
    (write-save-state fileport)
    (close-port fileport)))

;;; Forms to be executed at file load time
(defconfig *load-forms* '())

;;; TODO: does load-state-from-file need better error handling?
(define (load-state-from-file filename)
  (catch-all
   (load filename)
   (eval (cons 'begin *load-forms*))))


;;; The main operand stack.

(defconfig *stack* '())

(define (stack-empty?) (null? *stack*))
(define (stack-depth) (length  *stack*))

(define (stack-top)
  (when (stack-empty?)
    (error "The stack is currently empty."))
  (car *stack*))

(define (stack-push value)
   (set! *stack* (cons value *stack*)))
   
(define (stack-pop)
   (if (null? *stack*)
       #f
     (begin
       (let ((value (car *stack*)))
         (set! *stack* (cdr *stack*))
         value))))

(define (stack-ensure-arguments number)
  (if (< (length *stack*) number)
    (throw 'invalid-number-of-arguments)))
           
(define (command-stack-defmesg <vcalc-window> arit argy fn)
  (define (lambda-list-mandatory-args lambda-list count)
    (if (not (atom? lambda-list))
        (lambda-list-mandatory-args (cdr lambda-list) (+ 1 count))
        count))
  (aif (scheme::procedure-decl? fn 'stack-arity)
       (car it)
       (lambda-list-mandatory-args (car (scheme::%closure-code fn)) 0)))

(define (command-modes fn)
  (get-property fn 'command-modes ()))

(define (command-stack-arity fn)
  (values-bind (procedure-arity fn) (arity rest?)
    arity))

(define *last-arguments* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;; last-stack support
;;;
;;; This code supports a simple transaction model for stack 
;;; operations. Any block of code surrounded by a 
;;; with-stack-transaction is treated as a transaction against
;;; the stack. On entrance to the outermost transaction, a copy
;;; of the stack is pushed onto *undo-stack*.
;;;
;;; Invoking (last-stack) pushes the current stack onto *redo-stack*
;;; and pops a new stack off of *last-stack*. The redo stack is
;;; flushed whenever a new stack is pushed on *last-stack*

(defconfig *last-stack* '())
(defconfig *redo-stack* '())
(defconfig *last-stack-limit* 10)

(define *stack-transaction-level* 0)

(define (update-last-stack)
   (set! *last-stack* 
      (take-up-to (cons *stack* *last-stack*) *last-stack-limit*))
   (set! *redo-stack* '()))
    

(defmacro (with-stack-transaction last-stack? . forms)
   `(let ((old-st-level *stack-transaction-level*))
      (unwind-protect
        (lambda ()
	  (when ,last-stack?
	     (if (= *stack-transaction-level* 0)
		 (update-last-stack))
	     (set! *stack-transaction-level* (+ *stack-transaction-level* 1)))
          ,@forms)
        (lambda ()
          (set! *stack-transaction-level* old-st-level)))))


(define (apply-to-stack obj)
  "Apply an object to the stack.  For most objects, this pushes the object onto the
   stack. For procedures, this applies the function to the topmost n values on
   the stack."
  (cond ((procedure? obj)
	 (let ((count (command-stack-arity obj)))

	   (when (< (length *stack*) count)
	     (vc-error "Invalid number of arguments"))

	   (let ((remaining-stack (drop *stack* count))
		 (fixed-arguments (reverse (take *stack* count))))

	     (with-stack-transaction (not (command-mode? obj :no-stack-transaction))
				     (set! *stack* remaining-stack)
				     (values-bind (apply obj (append fixed-arguments remaining-stack)) retval
				       (cond ((null? retval)
					      )
					     ((pair? retval)
					      (for-each stack-push retval))
					     (#t
					      (stack-push retval)))
				       (when (not (command-mode? obj :no-last-arguments))
					 (set! *last-arguments* fixed-arguments))
				       retval)))))
	(#t
	 (stack-push obj)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard macros and object evaluation


(define *last-eval-time* "")

(define (string->program string)
  (objects->postfix-program (read-multiple string vc-read)))

(define (evaluate-object o)
  ((objects->postfix-program (list o))))


(define *recording-macro* #f)
(define *current-macro-seq* '())

(define (interactively-evaluate-objects . os)
  (when *recording-macro*
    (set! *current-macro-seq* (append! *current-macro-seq* (filter recordable? os))))
  (with-application-busy
   (let ((results (scheme::%time-apply0 (objects->postfix-program os))))
     (dynamic-let ((*flonum-print-precision* 4))
       (set! *last-eval-time* (format #f "~a ms." (* 1000.0 (vector-ref results 1)))))
     (vector-ref results 0))))




(defconfig *angle-mode* :degrees)
(defconfig *default-base* :decimal)
 
(define (default-base-text)
  (case *default-base*
	((:decimal)     "DEC" )
	((:hexadecimal) "HEX")
	((:octal)       "OCT" )
	((:binary)      "BIN" )
	(#t                                "???" )))

(define (default-base-char)
  (case *default-base*
    ((:decimal)     "d" )
    ((:hexadecimal) "h")
    ((:octal)       "o" )
    ((:binary)      "b" )
    (#t                                "?" )))

(define (default-base)
  (case *default-base*
    ((:decimal)     10 )
    ((:hexadecimal) 16 )
    ((:octal)       8  )
    ((:binary)      2  )
    (#t                                10 )))


(define (angle-mode-text)
  (case *angle-mode*
    ((:degrees)  "DEG" )
    ((:gradians) "GRAD")
    ((:radians)  "RAD" )
    (#t                           "???" )))

(defconfig *interest-accrual-mode* :begin)

(define (interest-accrual-text)
  (case *interest-accrual-mode* 
    ((:begin)     "BEGIN" )
    ((:end  )     "END"   )
    (#t                                       "???"   )))

;; (define (macro-recorder-text)
;;   (cond ((not *recording-macro*) "")
;; 	(#t "REC")))


(define (macro-recorder-text)
  "")