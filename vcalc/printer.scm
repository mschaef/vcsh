;;;; printer.scm
;;;; Mike Schaeffer
;;
;; The vCalc object printer.


(define-generic-function (write-vc-object o port)
  (write o port))

(define-generic-function (vc-object->f-text o)
  (write-to-string o))

(define (vc-object->string o)
  (let ((op (open-output-string)))
    (write-vc-object o op)
    (get-output-string op)))

(define-generic-function (describe-object o)
  "")

(define (string->vc-object string)
  (eval (vc-read string)))

(define (string->vc-objects string)
  (read-multiple string string->vc-object))

;; fixnum message handlers

(define-method (write-vc-object (o fixnum) port)
  (format port "#~a~a" (default-base-char) (number->string o (default-base))))

(define (fixnum->string fix)
  (string-append "#"
		 (default-base-char)
		 (number->string fix (default-base))))

(define-method (vc-object->f-text (o fixnum))
  (fixnum->string o))

;; flonum message handlers

(define-method (write-vc-object (flo flonum) port)
  (if (real? flo)
      (format port "~a" (inexact->display-string flo
						 *number-precision*
						 (eq? *number-format-mode* :scientific)
						 *seperator-mode*))
      (format port "~a~a~ai"     
	      (inexact->display-string (real-part flo)
				       *number-precision*
				       (eq? *number-format-mode* :scientific)
				       *seperator-mode*)
	      (if (< (imag-part flo) 0.0) "" "+")
	      (inexact->display-string (imag-part flo)
				       *number-precision*
				       (eq? *number-format-mode* :scientific)
				       *seperator-mode*))))

(define-method (vc-object->f-text (o flonum))
  (vc-object->string o))

;; symbol message handlers

(define-method (describe-object (concrete symbol))
  (if (disabled-premium-binding? concrete)
      (list :italic (symbol-name concrete))
      (symbol-name concrete)))

;; list message handlers

(defconfig *list-display-limit* 5)

(define-method (vc-object->f-text (concrete cons))
  (let ((l (length concrete)))
    (if (> l *list-display-limit*)
	`(#f "( " 
	     ,@(append-map! (lambda (x) (list (vc-object->f-text x) " ")) (take concrete *list-display-limit*)) 
	     (:italic ,(format #f "... n=~a" (- l *list-display-limit*)))
	     " )")
	`(#f "( " 
	     ,@(append-map! (lambda (x) (list (vc-object->f-text x) " ")) concrete)
	     ")"))))

(define-method (write-vc-object (o cons) port)
  (display "( " port)
  (dolist (item o)
    (write-vc-object item port)
    (display " " port))
  (display ")" port))

(define-method (vc-object->f-text (str string))
  `(#f "\"" ,str "\""))

(define-method (write-vc-object (o string) port)
  (format port "~s" o))


(define-method (write-vc-object (o closure) port)
  (define (postfix-write lambda-list postfix-code has-local-environment?)
    (define (nested-definition? x) (and (list? x) (= (length x) 4)))
    (if has-local-environment?
	(display "#{{ " port)
	(display "#{ " port))
    (dolist (arg lambda-list)
      (format port "~s " arg))
    (display "| " port)
    (dolist (oper postfix-code)
      (if (nested-definition? oper)
	  (postfix-write (second oper) (fourth oper) #f)
	  (write-vc-object oper port))
      (display " " port))
    (if has-local-environment?
	(display "}}" port)
	(display "}" port)))
  (if (postfix-program-object? o)
      (postfix-write (scheme::%procedure-property o 'lambda-list)
		     (scheme::%procedure-property o 'postfix)
		     (not (null? (scheme::%closure-env o))))
      (call-next-method)))

(define-method (vc-object->f-text (p closure))
  (if (postfix-program-object? p)
      (vc-object->string p)
      (call-next-method)))
  

