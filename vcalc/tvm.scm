(define (f1 x) (- (* x x) 10))

(define (f2 x) (- (sin x) (expt (/ x 2) 2)))

(define (firr x)
  (- (+ (/ 2.00 (expt (+ 1 x) 1))
	(/ 2.10 (expt (+ 1 x) 2))
	(/ 2.25 (expt (+ 1 x) 3))
	(/ 60.0 (expt (+ 1 x) 3)))
     50))

(define (f-nasty-irr x)
  (+ -180000
     (/ 100000 (expt (+ 1 x) 1))
     (/ 100000 (expt (+ 1 x) 2))
     (/ 100000 (expt (+ 1 x) 3))
     (/ 100000 (expt (+ 1 x) 4))
     (/ 100000 (expt (+ 1 x) 5))
     (/ -100000 (expt (+ 1 x) 6))
     (/ -100000 (expt (+ 1 x) 7))
     (/ -100000 (expt (+ 1 x) 8))
     (/ -100000 (expt (+ 1 x) 9))
     (/ -100000 (expt (+ 1 x) 10))
     (/ 200000 (expt (+ 1 x) 20))))

(define *bracket-expand-factor* 0.5)


(define (bisect-bracket-nr fn lo hi limit)
  (let ((f-l (fn lo))
	(f-h (fn hi)))
    (display (string-append "bracketing [" (number->string lo) "," 
			    (number->string hi) "]\n"))
    (cond ((= limit 0)
	   #f)
	  ((< (* f-l f-h) 0.0)
	   (cons lo hi))
	  ((< (abs f-l) (abs f-h))
	   (bisect-bracket-nr fn 
			      (- lo (* *bracket-expand-factor* (- hi lo))) hi
			      (- limit 1)))
	  (#t
	   (bisect-bracket-nr fn 
			      lo (+ hi (* *bracket-expand-factor* (- hi lo)))
			      (- limit 1))))))


(define (bisect-bracket fn lo hi limit)
  (define (bisect-bracket-1 fn lo hi lo? limit)
    (let ((f-l (fn lo))
	  (f-h (fn hi)))
      (display (string-append "bracketing [" (number->string lo) "," 
			      (number->string hi) "]\n"))
      (cond ((= limit 0)
	     #f)
	    ((< (* f-l f-h) 0.0)
	     (cons lo hi))
	    (lo?
	     (bisect-bracket-1 fn 
			       (- lo (* *bracket-expand-factor* (- hi lo))) hi
			       #f
			       (- limit 1)))
	    (#t
	     (bisect-bracket-1 fn 
			       lo (+ hi (* *bracket-expand-factor* (- hi lo)))
			       #t
			       (- limit 1))))))
  (bisect-bracket-1 fn lo hi #f limit))
	

(define (bisect-find-root fn lo hi epsilon limit)
  (let ((middle (/ (+ lo hi) 2)))
    (let ((f-l (fn lo))
	  (f-m (fn middle))
	  (f-h (fn hi)))
      (display (string-append "solving [" (number->string lo) "," 
			      (number->string hi) "]\n"))
      (cond ((< (abs f-m) epsilon)
	     middle)
	    ((= limit 0)
	     #f)
	    ((> (* f-l f-h) 0)
	     #f)
	    ((and (< 0 f-l) (< 0 f-h))
	     #f)
	    ((eq? (> 0 f-l) (> 0 f-m))
	     (bisect-find-root fn middle hi epsilon (- limit 1)))
	    ((eq? (> 0 f-h) (> 0 f-m))
	     (bisect-find-root fn lo middle epsilon (- limit 1)))))))
	    
(define (bisect-solve fn guess)
  (let ((initial-bracket (bisect-bracket fn guess (+ guess (/ guess 10)) 10)))
    (when (not initial-bracket)
      (vc-error "Couldn't bracket a root!"))
    (bisect-find-root fn (car initial-bracket) (cdr initial-bracket) 0.0001 20)))

(define (secant-find-root f xn-1 xn epsilon limit)
  (let ((fn-1 (f xn-1))
	(fn (f xn)))
    (display (string-append "solving (secant) [" (number->string xn-1) "," 
			    (number->string xn) "] = ["
			    (inexact->display-string fn-1 16 #t :us )
			    ","
			    (inexact->display-string fn 16 #t :us )
			    "]\n"))

    (cond ((< (abs fn) epsilon)
	   xn)
	  ((= limit 0)
	   #f)
	  (#t
	   (secant-find-root f
			     xn
			     (- xn
				(* fn
				   (/ (- xn xn-1)
				      (- fn fn-1))))
			     epsilon
			     (- limit 1))))))



(define (tvm pv fv i pmt n begin?)
  (+ pv
     (* pmt 
	(+ 1 (* i (if begin? 1 0)))
	(/ (- 1 (expt (+ 1 i) (- n)))
	   i))
     (* fv
	(expt (+ 1 i) (- n)))))

(define (tvm pv fv i pmt n begin?)
  (+ (* pv
	i)
     (* pmt 
	(+ 1 (* i (if begin? 1 0)))
	(- 1 (expt (+ 1 i) (- n))))
     (* fv
	i
	(expt (+ 1 i) (- n)))))

(define (tvm-for-pv fv i pmt n begin?)
  (secant-find-root (lambda (pv) (tvm pv fv i pmt n begin?))
		    (+ fv (* pmt n))
		    0
		    0.000001 
		    20))

(define (tvm-for-fv pv i pmt n begin?)
  (secant-find-root (lambda (fv) (tvm pv fv i pmt n begin?))
		    (- pv (* pmt n))
		    0
		    0.000001 
		    20))

(define (tvm-for-i pv fv pmt n begin?)
  (secant-find-root (lambda (i) (tvm pv fv i pmt n begin?))
		    0.02
		    0.01
		    0.000001 
		    20))
     
(define (tvm-for-pmt pv fv i n begin?)
  (secant-find-root (lambda (pmt) (tvm pv fv i pmt n begin?))
		    (/ (- pv fv) n)
		    (* 2 (/ (- pv fv) n))
		    0.000001 
		    20))

(define (tvm-for-n pv fv i pmt begin?)
  (secant-find-root (lambda (n) (tvm pv fv i pmt n begin?))
		    (/ (- fv pv) pmt)
		    (* 2 (/ (- fv pv) pmt))
		    0.0000000000001 
		    20))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (unique-cons x xs)
  "Prepend an item x onto a list xs, only if xs does not already contain x"
  (if (member x xs)
      xs
      (cons x xs)))
(define (unique xs)
  "Return a list containing all of the elements in xs, but with each item only represented once"
  (define (remaining xs uniques)
    (if (null? xs)
 uniques
 (remaining (cdr xs) (unique-cons (car xs) uniques))))
  (remaining xs '()))
(define (union . sets)
  "Compute the union of sets."
  (unique (apply append sets)))
      
(define (expr-vars expr)
  "Given an expression, return the set of argument variables."
  (cond ((symbol? expr)
  (list expr))
 ((pair? expr)
  (apply union (mapcar expr-vars (cdr expr))))
 (#t '())))
(define (expr-solver-lambda expr solving-for)
  "Given an expression, return a 1-argument lambda that evaluates the expression with respect to solving-for. All other variables in the expression must be bound to values, an error will be signaled."
  (let ((vars (expr-vars expr)))
    (unless (member solving-for vars)
     (error "Expression missing variable" (cons expr solving-for)))
    (unless (every? (mapcar symbol-bound? 
       (filter (lambda (var)
          (not (eq? var solving-for)))
        vars)))
     (error "Not all free variables are bound"))
    (eval `(lambda (,solving-for) ,expr))))
(set! tvm '(+ (* pv i) 
       (* pmt (+ 1 (* i (if begin? 1 0))) 
   (- 1 (expt (+ 1 i) (- n)))) 
       (* fv i (expt (+ 1 i) (- n)))))
(set! x2 '(- s (* x x)))
(define (solver-progress solver-name x1 x2 y1 y2)
  (display "s(")
  (display solver-name)
  (display ")[")
  (display x1)
  (display ", ")
  (display x2)
  (display "]=[")
  (display (inexact->display-string y1 16 #t :us))
  (display ", ")
  (display (inexact->display-string y2 16 #t :us))
  (display "]\n"))
  
(define *solver-epsilon* 0.00001)
(define *solver-iter-limit* 100) 
(define  (bisect-find-root fn lo hi) 
  (define (iter lo hi count)
    (let ((middle (/ (+ lo hi) 2))) 
      (let ((f-l (fn lo)) 
     (f-m (fn middle)) 
     (f-h (fn hi))) 
 (solver-progress "bisect" lo hi f-l f-h)
 (cond ((< (abs f-m) *solver-epsilon*) middle) 
       ((> count *solver-iter-limit*) #f) 
       ((> (* f-l f-h) 0) #f)
       ((and (< 0 f-l) (< 0 f-h)) #f) 
       ((eq? (> 0 f-l) (> 0 f-m))
        (iter middle hi (+ count 1))) 
       ((eq? (> 0 f-h) (> 0 f-m)) 
        (iter lo middle (+ count 1)))))))
  (iter lo hi 0))

(define (secant-find-root f xn-1 xn)
  (define (iter xn-1 xn count)
    (let ((fn-1 (f xn-1)) 
   (fn (f xn))) 
      (solver-progress "secant" xn-1 xn fn-1 fn)
      (cond ((< (abs fn) *solver-epsilon*) xn) 
     ((> count *solver-iter-limit*) #f)      
     (#t 
      (display (string-append "   m=" (number->string (/ (- xn xn-1) (- fn fn-1))) "\n"))
      (iter xn (- xn (* fn (/ (- xn xn-1) 
         (- fn fn-1)))) 
     (+ count 1))))))
  (iter xn-1 xn 0))

(bisect-find-root (expr-solver-lambda x9 'x) -1 20)
(secant-find-root (expr-solver-lambda x9 'x) 5 15)
(define x9 '(- 2 (expt x 9)))
(define x91 '(- 2 (expt x 91)))