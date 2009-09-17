


    

(define-vcalc-command (import-csv-file)
  "Imports a CSV file and pushes the contents on the top of the stack."
  (awhen (choose-file *current-window* #t '(("Comma Seperated Data Files" . "*.csv") #t))
    (list (read-csv-file it))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
; Basic primitives
          

(define (string-accept-to-null string)
  "Returns the leftmost characters of <string>, up to but
   not including the first #\nul. If there is no #\nul, 
   this function returns all of <string>"
  (aif (string-first-character string "\0") 
       (substring string 0 it)
       string))

(define (copy-object-to-clipboard obj)
  (dynamic-let ((*seperator-mode* :none))
    (cond ((number? obj)
	   (set-clipboard-data :CF_TEXT (vc-object->f-text obj)
			       "s-expression" (write-to-string obj)))
	  ((list? obj)
	   (set-clipboard-data "Csv" (list->csv-string obj)
			       "s-expression" (write-to-string obj)))
	  (#t
	   (set-clipboard-data "s-expression" (write-to-string obj))))))
			   


(define (get-object-from-clipboard)
  (aif (get-clipboard-data "s-expression" "Csv" :CF_TEXT)
    (let ((format (car it))
	  (data (string-accept-to-null (cdr it))))
      (cond ((equal? format "s-expression")
	     (vc-read data))
	    ((equal? format "Csv")
	     (csv-string->list data))
	    ((equal? format :CF_TEXT)
	     (vc-read data))
	    (#t (values))))
    (values)))

(define-vcalc-command (stack-cut x)
  "Cuts the top object from the stack and places it on the clipboard."
  (copy-object-to-clipboard x)
  (values))

(define-vcalc-command (stack-copy x)
  "Copies the top object from the stack to the clipboard."
  (copy-object-to-clipboard x)
  (values x))

(define-vcalc-command (stack-paste)
  "Pastes the object from the clipaboard onto the stack."
  (get-object-from-clipboard))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Online help logic (tip of the day and the keyboard map
;;; window)


(define (show-console-help-text)
  (display ";;; Welcome to the vCalc console.\n")
  (display ";;;\n")
  (display ";;; This window lets you evaluate arbitrary Scheme expressions.\n")
  (display ";;; Enter the expression you wish to evaluate and press <control+Enter>\n")
  (display ";;;\n")
  (display ";;; Sample Expressions:\n")  
  (display ";;;    Hide the console window: (hide-console)\n")
  (display ";;;    Online help: (apropos \"map\")\n")
  (display ";;;    Add numbers: (+ 2 3)\n")
  (display ";;;\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu operations

(define (do-config w)
  (define (get-current-config-vector)
    (vector *angle-mode*
	    *seperator-mode*
	    *number-format-mode*
	    *number-precision*
	    *interest-accrual-mode*
	    *default-base*))
  (define (set-current-config-vector vec)
    (awhen (vector-ref vec 0)
      (set! *angle-mode* it))
    (awhen (vector-ref vec 1)
      (set! *seperator-mode* it))
    (awhen (vector-ref vec 2)
      (set! *number-format-mode* it))
    (awhen (vector-ref vec 3)
      (set! *number-precision* it))
    (awhen (vector-ref vec 4)
      (set! *interest-accrual-mode* it))
    (awhen (vector-ref vec 5)
      (set! *default-base* it))
      (update-window *current-window*))
  (let ((current-configuration (get-current-config-vector)))
    (set-current-config-vector (aif (show-config-dialog w
							current-configuration 
							set-current-config-vector)
				    it
				    current-configuration))))

    










   
 


(define (vcalc-boot)
  (init-busy-keymap)
  (init-global-keymap)
  (init-vcalc-stack-window)
  (maybe-load-persistant-state)
  (show-window *stack-window*)
  (update-window *keyhelp-window*)
  (show-console-help-text)
  (maybe-show-tip-of-the-day))

(set! *flonum-print-precision* 16) 

;(define exit exit-application)

(define (run)
  ;(set-current-error-port scheme::*console-error-port*)
  ; (set-current-output-port scheme::*console-output-port*)
  ;(vcalc-boot)
  (repl))