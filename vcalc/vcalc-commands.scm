;;;; vcalc-commands.scm
;;;; Mike Schaeffer
;;
;; vCalc commands.

(define-package "vcalc-commands"
  (:uses "vcalc"
         "scheme")
  (:includes "tagged-object.scm"
             "constants.scm")
  (:shadows "+" "-" "*" "/" ">" ">=" "<" "<=" "=" "abs"
            "sin" "cos" "tan" "asin" "acos" "atan"
            "log" "exp" "expt" "log10" "exp10" "sqrt"
            "random" "set-random-seed!" "quotient" "remainder"
            "floor" "ceiling" "truncate" "round" "bitwise-and" 
            "bitwise-or" "bitwise-xor" "bitwise-not"
            "bitwise-shift-left" "bitwise-shift-right"
            "bitwise-arithmatic-shift-right" "bitwise-rotate-left"
            "bitwise-rotate-right" "make-rectangular" "make-polar" 
            "real-part" "imag-part" "magnitude" "angle"
            "->ieee-754-bits" "ieee-754-bits->"
            "first" "rest" "last" "butlast")
  (:exports "+" "-" "*" "/" "chs" "abs" "round" "truncate" "floor" "ceiling"
            ">" ">=" "<" "<=" "=" "prng" "prng-seed" "sin" "cos" "tan" "asin"
            "acos" "atan" "atan2" "sqrt" "expt" "log10" "exp10" "log" "exp"
            "erf" "normal-dist-prob" "normal-dist" "gamma" "factorial"
            "pi-constant" "e-constant" "phi-constant" "percent" 
            "percent-delta" "percent-t" "bitwise-and" "bitwise-or"
            "bitwise-xor" "bitwise-not" "bitwise-shl" "bitwise-shr"
            "constant-library" "apply-to-stack-repeatedly" "last-arguments"
            "last-stack" "redo-stack" "stack-clear" "stack-drop"
            "stack-swap" "stack-dup" "stack-dup2" "srot" "srotd"
            "stack-dropn" "begin-macro " "end-macro " "export-csv-file data"
            "toggle-key-help" "begin-editor-with-last-keystroke " "edit-object"
            "enter-object" "interactive-break"))



(define-vcalc-command (apply-to-stack-repeatedly o c)
  "Evaluate the object <o>, <c> times."
  (unless (real? c)
    (vc-error "Number expected: ~a" c))
  (let ((count (if (exact? c) c (inexact->exact c))))
    (repeat count (apply-to-stack o)))
  (values))

(define-vcalc-command (last-arguments) 
  "Pushes the arguments to the last command on the stack."
  (command-modes :no-last-arguments)
  (apply values *last-arguments*))


(define-vcalc-command (begin-macro) 
  "Begin recording a keystroke macro."
  (command-modes :not-recordable)
  (when *recording-macro*
    (vc-error "You are already recording a macro."))
  (set! *recording-macro* #t)
  (set! *current-macro-seq* '())
  (values))
 
(define-vcalc-command (end-macro) 
  "End recording a keystroke macro."
  (command-modes :not-recordable)
  (unless *recording-macro*
    (vc-error "You are not recording a macro."))
  (let ((toplevel-macro (objects->postfix-program *current-macro-seq*)))
    (set! *current-macro-seq* ())
    (set! *recording-macro* #f)    
    (values toplevel-macro)))

;;
;; Basic Arithmetic
;;

(define-vcalc-command (+ x y)
  "Adds <x> and <y>.")

(define-method (+ (x number) (y number))
  (scheme::+ x y))

(define-method (+ (x cons) (y cons))
  (append x y))

(define-vcalc-command (- x y)
  "Subtracts <y> from <x>")

(define-method (- (x number) (y number))
  (scheme::- x y))

(define-vcalc-command (* x y)
  "Multiplies <x> and <y>.")
  
(define-method (* (x number) (y number))
  (scheme::* x y))

(define-vcalc-command (/ x y)
  "Divides <x> and <y>.")
  
(define-method (/ (x number) (y number))
  (scheme::/ x y))

(define-vcalc-command (chs x)
  "Changes the sign of <x>.")

(define-method (chs (x number))
  (scheme::- x))

(define-vcalc-command (abs x)
  "Computes the absolute value of <x>.")

(define-method (abs (x number))
  (scheme::abs x))

;;
;; Number parts
;;

(define-vcalc-command (round x)
  "Rounds <x> to the nearest whole number. Numbers with fractional parts
   > 0.5 are rounded up, others are rounded down.")

(define-method (round (x number))
  (scheme::round x))


(define-vcalc-command (truncate x)
  "Truncates <x>. The fractional part of the number is set to 0")

(define-method (truncate (x number))
  (scheme::truncate x))

(define-vcalc-command (floor x)
  "Computes the next lower whole number.")

(define-method (floor (x number))
  (scheme::floor x))

(define-vcalc-command (ceiling x)
  "Computes the next higher whole number.")

(define-method (ceiling (x number))
  (scheme::ceiling x))

;; 
;; Comparisons
;;

(define-vcalc-command (> x y)
  "Retrns true if <x> > <y>")

(define-method (> (x complex) (y complex))
  (> (magnitude x) (magnitude y)))

(define-method (> (x number) (y number))
  (scheme::> x y))

(define-vcalc-command (>= x y)
  "Retrns true if <x> >= <y>")

(define-method (>= (x complex) (y complex))
  (>= (magnitude x) (magnitude y)))

(define-method (>= (x number) (y number))
  (scheme::>= x y))

(define-vcalc-command (< x y)
  "Retrns true if <x> < <y>")

(define-method (< (x complex) (y complex))
  (< (magnitude x) (magnitude y)))

(define-method (< (x number) (y number))
  (scheme::< x y))

(define-vcalc-command (<= x y)
  "Retrns true if <x> <= <y>")

(define-method (<= (x complex) (y complex))
  (<= (magnitude x) (magnitude y)))

(define-method (<= (x number) (y number))
  (scheme::<= x y))


(define-vcalc-command (= x y)
  "Retrns true if <x> = <y>")

(define-method (= (x complex) (y complex))
  (= (magnitude x) (magnitude y)))

(define-method (= (x number) (y number))
  (scheme::= x y))

;;;; PRNG

(define-vcalc-command (prng)
  "Computes a pseudo-random number in the range [0,1)."
  (scheme::random))

(define-vcalc-command (prng-seed x)
  "Sets the seed of the pseudo random number generator.")

(define-method (prng-seed (x fixnum))
  (scheme::set-random-seed! x))

;;;; Trig

(define-vcalc-command (sin x)
  "Computes the sine of <x>.")

(define-method (sin (x flonum))
  (scheme::sin (to-radians x)))

(define-vcalc-command (cos x)
  "Computes the cosine of <x>.")

(define-method (cos (x flonum))
  (scheme::cos (to-radians x)))

(define-vcalc-command (tan x)
  "Computes the tangent of <x>.")

(define-method (tan (x flonum))
  (scheme::tan (to-radians x)))

(define-vcalc-command (asin x)
  "Computes the arc-sine of <x>.")

(define-method (asin (x flonum))
  (to-user-angle ( scheme::asin x)))

(define-vcalc-command (acos x)
  "Computes the arc-cosine of <x>.")

(define-method (acos (x flonum))
  (to-user-angle (scheme::acos x)))

(define-vcalc-command (atan x)
  "Computes the arc-tangent of <x>.")

(define-method (atan (x flonum))
  (to-user-angle (scheme::atan x)))

(define-vcalc-command (atan2 x y)
  "Computes the arc-tangent of <x>/<y>.")

(define-method (atan2 (x flonum) (y flonum))
  (to-user-angle (scheme::atan2 x y)))

;;;;; Logs and exponentiation

(define-vcalc-command (sqrt x)
  "Computes the square root of <x>.")

(define-method (sqrt (x number))
  (scheme::sqrt x))

(define-vcalc-command (expt x y)
  "Raises <x> to the <y> power.")

(define-method (expt (x flonum) (y flonum))
  (scheme::expt x y))

(define-vcalc-command (log10 x)
  "Computes the base-10 log of <x>.")

(define-method (log10 (x number))
  (scheme::log10 x))

(define-vcalc-command (exp10 x)
  "Raises 10 to the power of <x>.")

(define-method (exp10 (x number))
  (scheme::exp10 x))

(define-vcalc-command (log x)
  "Computes the base-e log of <x>.")

(define-method (log (x number))
  (scheme::log x))

(define-vcalc-command (exp x)
  "Raises e to the power of <x>.")

(define-method (exp (x number))
  (scheme::exp x))

(define *epsilon* 0.0000000001)
(define *sqrt-2pi* 2.506628274631002)
(define *bail-after* 100)
(define *bail-after-n2* 12)

(define (trap-integrate fn lo hi n)
  " (x -> f(x)), lo, hi, n -> x : Integrate the function fn over the interval [lo, hi]. This function uses the trapeziodal method with n steps."
  (define step-size (/ (- hi lo) n))

  (define (next-step i sum)
    (let ((step-val (fn (+ lo (* step-size i)))))
      (cond ((= i n)
	     (* (+ sum (* step-val 0.5)) step-size))
	    (#t
	     (next-step (+ i 1) (+ sum step-val))))))

  (next-step 1 (* 0.5 (fn lo))))

(define (integrate fn lo hi)
  " ( x -> f(x) ), lo, hi -> x : Integrate the function fn over the interval [lo, hi]. This function repeatedly integrated with the trapeziodal algorithm, doubling the number of integration steps until two results are returned within *epsilon*"
  (define (next-refinement i steps last-value)
    (let ((current-value (trap-integrate fn lo hi steps)))
      (cond ((> i *bail-after-n2*)
	     #f)
	    ((or (not last-value)
		  (> (abs (- last-value current-value)) *epsilon*))
	     (next-refinement (+ i 1) (* steps 2) current-value))
	    (#t
	     current-value))))
  ;; The first iteration starts with 32 integration steps. This lowers
  ;; performance integrating linear functions, but reduces the
  ;; liklihood of early false positives from the refinement stop
  ;; condition.
  (next-refinement 0 32 #f))

(define-vcalc-command (erf z) 
  "Computes the error function.")

(define-method (erf (z complex))
  (vc-error "Erf undefined for complex numbers" z))
		    
(define-method (erf (z number))
  (cond ((< z 0)
	 (chs (erf (cjs z))))
	(#t
	 (* (/ 2 (sqrt *pi*))
	    (integrate (lambda (t) (antilog-e (chs (expt t 2))))
		       0 
		       z)))))

(define-vcalc-command (normal-dist-prob x mean variance)
  "Computes the normal probability distribution")

(define-method (normal-dist-prob (x number) (mean number) (variance number))
  (/ (antilog-e (/ (chs (expt (- x mean) 2))
		   (* 2 (expt variance 2))))
     *sqrt-2pi*))


(define-vcalc-command (normal-dist x mean variance)
  "Computes the normal distribution")

(define-method (normal-dist (x number) (mean number) (variance number))
  (* 0.5
     (+ 1 (erf (/ (- x mean)
		  (* variance (sqrt 2.0)))))))

(define-vcalc-command (gamma x)
  "Computes the gamma function of number.")

(define-method (gamma (x flonum))
  (scheme::* (scheme::/ (scheme::+ (scheme::* 75122.6331530 (scheme::expt x 0))
				   (scheme::* 80916.6278952 (scheme::expt x 1))
				   (scheme::* 36308.2951477 (scheme::expt x 2))
				   (scheme::* 8687.24529705 (scheme::expt x 3))
				   (scheme::* 1168.92649479 (scheme::expt x 4))
				   (scheme::* 83.8676043424 (scheme::expt x 5))
				   (scheme::* 2.50662827511 (scheme::expt x 6)))
			(scheme::* (scheme::+ x 0)
				   (scheme::+ x 1)
				   (scheme::+ x 2)
				   (scheme::+ x 3)
				   (scheme::+ x 4)
				   (scheme::+ x 5)
				   (scheme::+ x 6)))
	     (scheme::expt (scheme::+ x 5.5)
			   (scheme::+ x 0.5))
	     (scheme::exp (scheme::- (scheme::+ x 5.5)))))
  


(define-vcalc-command (factorial x)
  "Computes the factorial of a number."
  (command-modes :premium))

(define-method (factorial (x fixnum))
  (define (loop x accum)
    (if (< x 1) accum (loop (- x 1) (* x accum))))
  (when (< x 0)
    (vc-error "Invalid argument to factorial!" x))
  (loop x 1))
  
(define-method (factorial (x flonum))
  (if (> x 0.0)
      (let ((fact 1.0))
	(while (> x 1.0)
	  (set! fact (* fact x))
	  (set! x (- x 1.0)))
	(if (> x 1.0)
	    (set! fact (* fact (gamma x))))
	fact)
      (gamma (+ x 1))))

(define-vcalc-command (pi-constant) "Pushes the value of pi on the stack." 
  *pi*)

(define-vcalc-command (e-constant) "Pushes the value of e on the stack."  
  2.71828182845905)

(define-vcalc-command (phi-constant) "Pushes the value of phi, the golden ratio on the stack."
  1.61803398874989484820)

(define-vcalc-command (percent x y)
  "Computes <y> percent of <x>.")

(define-method (percent (x number) (y number))
  (/ (* x y) 100.0))

(define-vcalc-command (percent-delta x y)
  "Returns the percentage change between <x> and <y>.")

(define-method (percent-delta (x number) (y number))
  (* 100.0 (/ (- y x) x)))

(define-vcalc-command (percent-t x y) ;; percentage of total
  "Returns the percentage of <x> represented by <y>.")

(define-method (percent-t (x number) (y number))
  (* 100.0 (/ y x)))

(define-vcalc-command (bitwise-and x y)
  "Computes the bitwise and of <x> and <y>.")

(define-method (bitwise-and (x fixnum) (y fixnum))
  (scheme::bitwise-and x y))  


(define-vcalc-command (bitwise-or x y)
  "Computes the bitwise or of <x>and <y>.")

(define-method (bitwise-or (x fixnum) (y fixnum))
  (scheme::bitwise-or x y))  

(define-vcalc-command (bitwise-xor x y)
  "Computes the bitwise xor of <x> and <y>.")

(define-method (bitwise-xor (x fixnum) (y fixnum))
  (scheme::bitwise-xor x y))  

(define-vcalc-command (bitwise-not x)
  "Computes the bitwise inverse of <x>.")

(define-method (bitwise-not (x fixnum))
  (scheme::bitwise-not x))

(define-vcalc-command (bitwise-shl x y)
  "Computes <x> shifted bitwise <y> bits to the left.")

(define-method (bitwise-shl (x fixnum) (y fixnum))
  (scheme::bitwise-shift-left x y))

(define-vcalc-command (bitwise-shr x y)
  "Computes <x> shifted bitwise <y> bits to the right.")

(define-method (bitwise-shr (x fixnum) (y fixnum))
  (scheme::bitwise-shift-right x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;; Registers
;;;
;;; Registers are a set of n memory locations into which objects
;;; can be temporarily stored
(define *max-register* 100)

(defconfig *registers* (make-vector *max-register* 0))

(define-vcalc-command (parse-register-number n)
  "Parses the register number <n>.")

(define-method (parse-register-number (r number))
  (when (or (scheme::< r 0)
	    (scheme::>= r *max-register*))
    (vc-error "Invalid register ~a, register numbers must be in the range [~a, ~a]"
	      r 0 (scheme::- *max-register* 1)))
  (scheme::inexact->exact r))


(define-vcalc-command (rstore value s)
  "Stores the valuue on the top of the stack into the specified register."
  (vector-set! *registers* (parse-register-number s) value)
  (values))

(define-vcalc-command (rstore-with-last-keystroke)
  "Invokes rstore, using the key number of the last keystroke."
  (command-modes :not-recordable)
  (interactively-evaluate-objects (+ 1 (last-key-number)) 'rstore)
  (values))

(define-vcalc-command (rrecall s)
  "Recalls the value in the specified register. The value in the register
   is not evaluated.")

(define-method (rrecall (s number))
  (vector-ref *registers* (parse-register-number s)))

(define-vcalc-command (rrecall-with-last-keystroke)
  "Invokes rrecall, using the key number of the last keystroke."
  (command-modes :not-recordable)
  (interactively-evaluate-objects (+ 1 (last-key-number)) 'rrecall)
  (values))

(define-vcalc-command (rapply s)
  "Evaluates the value in the specified register.")

(define-method (rapply (s number))
  (apply-to-stack (rrecall s))
  (values))

(define-vcalc-command (rapply-with-last-keystroke)
  "Invokes rapply, using the key number of the last keystroke."
  (command-modes :not-recordable)
  (interactively-evaluate-objects (+ 1 (last-key-number)) 'rapply)
  (values))

(define-vcalc-command (rexch value s)
  "Exchanges the value on the top of the stack with the value in register s."
  (let ((slot (parse-register-number s)))
    (let ((temp (vector-ref *registers* slot)))
      (vector-set! *registers* slot value)
      temp)))

;
; Register watch is a watch facility that displays register values
; on the primary window as they are updated
;

(defconfig *register-watch-list* '())    

(define-vcalc-command (register-watch s)
  "Adds a watch on the specified register to the watch window.")

(define-method (register-watch (s number))
  (let ((slot (parse-register-number s)))
    (unless (member slot *register-watch-list*)
      (set! *register-watch-list* (qsort (cons slot *register-watch-list*) <))
      (when *current-window*
        [*current-window* update])))
  (values))

(define-vcalc-command (register-unwatch s)
  "Removes a watch on the specified register from the watch window.")

(define-method (register-unwatch (s number))
  (let ((slot (parse-register-number s)))
    (set! *register-watch-list* (delete slot *register-watch-list*))
    [*current-window* update])
  (values))
  
(define-vcalc-command (register-unwatch-all)
  "Removes all register watches from the watch window."
  (set! *register-watch-list* '())  
  (values))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Operational modes


(define-vcalc-command (set-decimal-mode)      
  "Sets the default display base to decimal, base-10" 
  (set! *default-base* :decimal)      (values))

(define-vcalc-command (set-hexadecimal-mode)  
  "Sets the default display base to hexadecimal, base-16" 
  (set! *default-base* :hexadecimal)  (values))

(define-vcalc-command (set-octal-mode)        
  "Sets the default display base to octal, base-8" 
  (set! *default-base* :octal)        (values))

(define-vcalc-command (set-binary-mode)       
  "Sets the default display base to binary, base-2" 
  (set! *default-base* :binary)       (values))

(define (to-radians angle)
  (case *angle-mode*
    ((:degrees)
     (* angle (/ *pi* 180)))
    ((:gradians)
     (* angle (/ *pi* 200)))
    ((:radians)
     angle)))

(define (to-user-angle angle)
  (case *angle-mode*
    ((:degrees)
     (/ angle (/ *pi* 180)))
    ((:gradians)
     (/ angle (/ *pi* 200)))
    ((:radians)
     angle)))

(define-vcalc-command (set-degree-mode)  
  "Sets the default angle representation to degrees."
  (set! *angle-mode* :degrees)    
  (values))

(define-vcalc-command (set-radian-mode)  
  "Sets the default angle representation to radians."
  (set! *angle-mode* :radians)    
  (values))

(define-vcalc-command (set-gradian-mode) 
  "Sets the default angle representation to gradians."
  (set! *angle-mode* :gradians)   
  (values))


(defconfig *seperator-mode* :us)

(define-vcalc-command (set-no-seperator)
  "Sets the display of numbers to not use a thousands seperator and use
   a period (.) as a decimal seperator."
  (set! *seperator-mode* :none) 
  (values))

(define-vcalc-command (set-comma-seperator)   
  "Sets the display of numbers to use a comma (,) for a thousands seperator
   and use a period (.) as a decimal seperator."
  (set! *seperator-mode* :us)   
  (values))

(define-vcalc-command (set-period-seperator)  
  "Sets the display of numbers to use a period (.) for a thousands seperator
   and use a comma (,) as a decimal seperator."
  (set! *seperator-mode* :euro) 
  (values))

(defconfig *number-precision* 4)
(defconfig *number-format-mode* :fixed)

(define (range-check-width w)
  (cond ((> w 16) 16)
	((< w 0)  0)
	(#t w)))

(define-vcalc-command (set-scientific-mode s) 
  "Configures vCalc to display numbers in scientific notation.
  The display precision of the mantissa is set to <s>.")

(define-method (set-scientific-mode (s number))
  (set! *number-precision* (range-check-width (inexact->exact s)))
  (set! *number-format-mode* :scientific)
  (values))

(define-vcalc-command (set-fixed-mode s) 
  "Configures vCalc to display numbers in fixed precision.
  The display precision of the mantissa is set to <s>.")

(define-method (set-fixed-mode (s number))
  (set! *number-precision* (range-check-width (inexact->exact s)))
  (set! *number-format-mode* :fixed)
  (values))

; !! We could use standard/engineering modes


(define-vcalc-command (set-beginning)
  "Sets vCalc to accrue interest at the beginning of terms."
  (set! *interest-accrual-mode* :begin)
  (values))  

(define-vcalc-command (set-ending)
  "Sets vCalc to accrue interest at the beginning of terms."
  (set! *interest-accrual-mode* :end)
  (values))


(define *console-visible* #f)

(define-vcalc-command (toggle-console)
  "Toggles the display of the vCalc Lisp console."
  (if *console-visible*
      (hide-console)
      (show-console))
  (set! *console-visible* (not *console-visible*))
  (values))

;;;; List based statistics

;; For the sake of usability, all the dataset commands use a default
;; dataset stored in the top of the register file. These functions
;; help manage that dataset. This usability advice is courtesy of Donny.

(define (set-current-dataset! dataset)
  (rstore dataset (scheme::- *max-register* 1)))

(define (list-or-null? x) (or (list? x) (null? x)))

(define (current-dataset)
  (let ((dataset (rrecall (scheme::- *max-register* 1))))
    (unless (list-or-null? dataset)
      (vc-error "The current dataset is not a valid list."))
    dataset))

(define (always-clear-data)
  (set-current-dataset! '()))

(define-vcalc-command (clear-data)
  "Clears the current dataset, prompts the user to verify."
  (command-modes :premium)
  (when (yes-or-no? "Are you sure you want to clear the current dataset?")
    (always-clear-data))
  (values))

(always-clear-data)   ; Need to initialize the first instance of the dataset

(define-vcalc-command (push-data x)
  "Adds the object <x> to the current data set.")

(define-method (push-data (x number))
  (set-current-dataset! (cons x (current-dataset)))
  (values))

;;; Interface functions (called by the user)

(define-vcalc-command (data-tot)
  "Compute the total sum of the items in the current dataset."
  (list-sum (current-dataset)))

(define-vcalc-command (data-atot)
  "Compute the cumulative statistics totals for the currrent dataset."
  (let ((sums (list-stat-sums (current-dataset))))
    (values (tag-object (list-ref sums 0) "count")
	    (tag-object (list-ref sums 1) "x-sum")
	    (tag-object (list-ref sums 2) "x2-sum")
	    (tag-object (list-ref sums 3) "y-sum")
	    (tag-object (list-ref sums 4) "y2-sum")
	    (tag-object (list-ref sums 5) "xy-sum"))))


(define-vcalc-command (data-avg)
  "Computes the mean of the current dataset."
  (list-mean (current-dataset)))

(define-vcalc-command (data-sdev)
  "Computes the standard deviation of the current dataset."
  (list-sdev (current-dataset)))

(define-vcalc-command (data-lr)
  "Computes the slope and intercept of the best fit line through the
   current dataset."
  (vector->list (list-lr (current-dataset))))

(define-vcalc-command (data-pcov)
  "Computes the population covariance of the current dataset."
  (list-cov (current-dataset) #t))

(define-vcalc-command (data-cov)
  "Computes the covariance of the current dataset."
  (list-cov (current-dataset) #f))

(define-vcalc-command (data-corr)
  "Computes the correlation coefficient of the current dataset."
  (list-corr (current-dataset)))

(define-vcalc-command (data-watch)
  "Establishes a watch on the current dataset."
  (register-watch (- *max-register* 1)))

(define-vcalc-command (data-predx slope intercept y)
  "Given a line specified by <slope> and <intercept>, and a value for
   <y>, return the x coordinate of the corresponding point on the
   line. This version of the function leaves <slope> and <intercept>
   on the stack to allow it to be more easily invoked more than once.")

(define-method (data-predx (slope flonum) (intercept flonum) (y flonum))
  (values slope
	  intercept
	  (/ (- y intercept) slope)))


(define-vcalc-command (data-predy slope intercept x)
  "Given a line specified by <slope> and <intercept>, and a value for
   <x>, return the y coordinate of the corresponding point on the
   line. This version of the function leaves <slope> and <intercept>
   on the stack to allow it to be more easily invoked more than once.")

(define-method (data-predy (slope flonum) (intercept flonum) (x flonum))
  (values slope
	  intercept
	  (+ intercept (* slope x))))

(define-vcalc-command (data-predx-consuming slope intercept y)
  "Given a line specified by <slope> and <intercept>, and a value for
   <y>, return the x coordinate of the corresponding point on the
   line.")

(define-method (data-predx-consuming (slope flonum) (intercept flonum) (y flonum))
  (/ (- y intercept) slope))


(define-vcalc-command (data-predy-consuming slope intercept x)
  "Given a line specified by <slope> and <intercept>, and a value for
   <x>, return the y coordinate of the corresponding point on the
   line.")

(define-method (data-predy-consuming (slope flonum) (intercept flonum) (x flonum))
  (+ intercept (* slope x)))



(define-vcalc-command (data-log-x dataset)
  "Maps log over the x coordinates in a list of complex numbers.")

(define-method (data-log-x (dataset cons))
  (map (lambda (pt)
	 (make-rectangular (log (real-part pt)) (imag-part pt)))
       dataset))

(define-vcalc-command (data-log-y dataset)
  "Maps log over the y coordinates in a list of complex numbers.")

(define-method (data-log-y (dataset cons))
  (map (lambda (pt)
	 (make-rectangular (real-part pt) (log (imag-part pt))))
       dataset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User evaluation functions

(define-vcalc-command (evaluate o)
  "Evaluate object <o>."
  (evaluate-object o)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stack marker, for denoting the beginning of sequences of data

(define-structure stack-marker)

(define-method (vc-object->f-text (o stack-marker))
    (list :bold "("))

(define-method (write-vc-object (o stack-marker) port)
  (display "(" port))


(define-vcalc-command (begin-list)
  "Pushes a list marker onto the stack."
  (make-instance stack-marker))

(define-vcalc-command (end-list)
  "Create a list with all the elements from the top of the stack 
   to the stack list marker. If there is no stack list marker, the
   entire stack contents are used."
  (define (next-element current-list)
    (cond ((stack-empty? *stack*)
	   current-list)
	  ((stack-marker? (stack-top))
	   (stack-pop)
	   current-list)
	  (#t
	   (next-element (cons (stack-pop) current-list)))))
  (next-element '()))

(define-vcalc-command (list-> xs)
  "Breaks list <xs> into its component parts and in integer inndicating the
   number of list elements.")

(define-method (list-> (xs cons))
  (apply values (append xs (cons (length xs)))))

(define-vcalc-command (->list count)
  "Builds a list from the top <count> stack elements.")

(define-method (->list (count number))
  (stack-ensure-arguments count) ;; !!!!!!!!!! this does not seem to work
  (let ((new-list (reverse! (take *stack* count))))
    (stack-dropn count)
    new-list))

(define-vcalc-command (first xs)
  "Returns the first element of the list <xs>.")

(define-method (first (xs cons))
  (car xs))

(define-vcalc-command (rest xs)
  "Returns a new list with every elemebt but the first.")

(define-method (rest (xs cons))
  (cdr xs))

(load-time-define butfirst rest)

(define-vcalc-command (last xs)
  "Returns the last element of <xs>.")

(define-method (last (xs cons))
  (scheme:last xs))

(define-vcalc-command (butlast xs)
  "Returns a list containing every element of <xs> but the last.")

(define-method (butlast (xs cons))
  (scheme:butlast xs))

(define-vcalc-command (union xs ys)
  "Computes the set union of the lists <xs> and <ys>. The resultant
   list contains each element of <xs> and <ys> once and only once.")

(define-method (union (xs cons) (ys cons))
  (set-union xs ys))

(define-vcalc-command (diff xs ys)
  "Computes the set difference of the lists <xs> and <ys>.")

(define-method (diff (xs cons) (ys cons))
  (set-diff xs ys))

(define-vcalc-command (intersect xs ys)
  "Computes the set intersection of the lists <xs> and <ys>.")

(define-method (intersect (xs cons) (ys cons))
  (set-isect xs ys))

(define-vcalc-command (*i x)
 "Multiplies <x> by i.")

(define-method (*i (x number))
  (* 1i x))

(define-vcalc-command (*i/+ x y)
  "Multiplies <x> by i and adds <y>.")

(define-method (*i/+ (x number) (y number))
  (+ x (* 1i y)))

(define-vcalc-command (make-rectangular re im)
  "Makes a complex number with real part <re> and imaginary part <im>.")

(define-method (make-rectangular (re flonum) (im flonum))
  (scheme:make-rectangular re im))

(define-vcalc-command (make-polar r theta)
  "Makes a polar complex number with r <r> and theta <theta>.")

(define-method (make-polar (r flonum) (theta flonum))
  (scheme::make-polar r (to-radians theta)))

(define-vcalc-command (real-part x)
  "Returns the real valued part of the number <x>.")

(define-method (real-part (x number))
  (scheme::real-part x))

(define-vcalc-command (imag-part x)
  "Returns the imaginary valued part of the number <x>.")

(define-method (imag-part (x number))
  (scheme::imag-part x))

(define-vcalc-command (angle x)
  "Returns the angle of the complex valued number <x>.")

(define-method (angle (x number))
  (to-user-angle (scheme::angle x)))


(define-vcalc-command (magnitude x)
  "Returns the magnitude of the complex valued number <x>.")

(define-method (magnitude (x number))
  (scheme::magnitude x))

(define-vcalc-command (->rectangular x)
  "Breaks the complex valued number <x> into its real and imaginary components.")

(define-method (->rectangular (x number))
  (values (real-part x) (imag-part x)))

(define-vcalc-command (->polar x)
  "Breaks the complex valued number <x> into its r and theta components.")

(define-method (->polar (x number))
  (values (magnitude x) (angle x)))

(define-vcalc-command (->ieee-754-bits x)
  "Returns a 64-bit integer containing the IEEE-754 double precision representation of <x>.")

(define-method (->ieee-754-bits (x flonum))
  (scheme::->ieee-754-bits x))

(define-vcalc-command (ieee-754-bits-> x)
  "Given a 64-bit integer containing the IEEE-754 double precision encoding
   of a number, return the number as a float.")

(define-method (ieee-754-bits-> (x fixnum))
  (scheme::ieee-754-bits-> x))

(define-vcalc-command (constant-library)
  "Prompts the user to select a constant from the library to be pushed on 
   the stack."
  (command-modes :not-recordable)
  (awhen (choose *current-window* *constant-library* "Constant Library" "Pick a constant")
    (interactively-evaluate-objects (cdr it)))
  (values))


(define-vcalc-command (last-stack) 
  "Resets the stack to its state at the beginning of the last command."
  (command-modes :no-stack-transaction)
  (when (not (null? *last-stack*))
    (set! *redo-stack* (cons *stack* *redo-stack*))
    (set! *stack* (car *last-stack*))
    (set! *last-stack* (cdr *last-stack*)))
  (values))

(define-vcalc-command (redo-stack) 
  "Restores the stack after an invocation of last-stack."
  (command-modes :no-stack-transaction)
  (when (not (null? *redo-stack*))
    (set! *last-stack* (cons *stack* *last-stack*))
    (set! *stack* (car *redo-stack*))
    (set! *redo-stack* (cdr *redo-stack*)))
  (values))

(define-vcalc-command (stack-clear) 
  "Clears the stack."
  (set! *stack* '())
  (values))

(define-vcalc-command (stack-drop x) 
  "Drops the first item off the stack."
  (values))

(define-vcalc-command (stack-swap x y) 
  "Swaps the first two items on the stack."
  (values y x))

(define-vcalc-command (stack-dup x) 
  "Duplicates the top item on the stack."
  (values x x))

(define-vcalc-command (stack-dup2 x y) 
  "Duplicates the top two items on the stack."
  (values x y x y))
	
(define-vcalc-command (srot x y z) 
  "Rotates the top two items on the stack, x y z -> y z x"
  (values y z x))

(define-vcalc-command (srotd x y z) 
  "Rotates downward the top two items on the stack, x y z -> z x y"
  (values z x y))

(define-vcalc-command (stack-dropn n) 
  "Drops the first <n> items from the stack.")

(define-method (stack-dropn (n number))
  (set! *stack* (drop *stack* n))
  (values))

(define-vcalc-command (export-csv-file data)
  "Exports the data on the top of the stack to a CSV file."
  (awhen (choose-file *current-window* #f '(("Comma Seperated Data Files" . "*.csv") #t))
    (with-port p (open-output-file it)
      (write-csv data p))))

(define-vcalc-command (edit-object x)
  "Opens object <x> in a text editor."
  (aif (edit-text *stack-window* (vc-object->string x) check-text)
       (string->vc-object it)
       x))

(define-vcalc-command (enter-object)
  "Opens a text editor for entry of a new object."
  (aif (edit-text *stack-window* "" check-text)
       (string->vc-object it)
       (values)))

(define-vcalc-command (interactive-break)
  "Stops exection of the current interactive command."
  (signal 'user-break)
  (values))
