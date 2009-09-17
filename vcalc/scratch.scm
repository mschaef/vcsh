(lambda () 
  (let-internal 
   (#<US: GS-2> #<US: GS-3>) 
   ((gc-info 5) (gc-info 6)) 
   (let-internal 
    (#<US: GS-4>) 
    ((begin 
       (let-internal 
	(x y s) 
	((car (get-window-size w)) 
	 (cdr (get-window-size w)) 
	 (get-window-draw-surface w)) 
	(begin 
	  (let-internal 
	   (#<US: GS-5> #<US: GS-6>) 
	   (*geometry-stack* (quote ())) 
	   (begin 
	     (unwind-protect 
	      (lambda () 
		(set! *geometry-stack* (cons 
					(list (vector 0 0 x y) 
					      (lambda (g cw ch) #(0 0 0 0)) #f) 
					*geometry-stack*)) 
		(set! #<US: GS-6> (begin (let-internal (#<US: GS-7>) (0) (begin (let-internal (#<US: GS-8> #<US: GS-9>) (*geometry-stack* (quote ())) (begin (unwind-protect (lambda () (set! *geometry-stack* (cons (list (vector-set! (vector-copy (caar *geometry-stack*)) 2 150) (lambda (g cw ch) (let-internal (gx gy gw gh) ((vector-ref (caar *geometry-stack*) 0) (vector-ref (caar *geometry-stack*) 1) (vector-ref (caar *geometry-stack*) 2) (vector-ref (caar *geometry-stack*) 3)) (begin (set! #<US: GS-7> (+ #<US: GS-7> ch)) (if #f (vector-set! g 3 (- gh ch)) (vector-set! (vector-set! g 3 (- gh ch)) 1 (+ gy c)))))) #f) *geometry-stack*)) (set! #<US: GS-9> (begin (let-internal (#<US: GS-10> #<US: GS-11> #<US: GS-12> #<US: GS-13>) ((vector-ref (caar *geometry-stack*) 0) (vector-ref (caar *geometry-stack*) 1) (vector-ref (caar *geometry-stack*) 2) (vector-ref (caar *geometry-stack*) 3)) (if (caddar *geometry-stack*) (begin (with-geometry (- (+ #<US: GS-10> #<US: GS-12>) 50) (- (+ #<US: GS-11> #<US: GS-13>) 50) 50 50 (layout-box s) (consume 50 50))) (begin (let-internal (#<US: GS-14> #<US: GS-15>) (*geometry-stack* (quote ())) (begin (unwind-protect (lambda () (set! *geometry-stack* (cons (list (vector #<US: GS-10> #<US: GS-11> 50 50) (lambda (g cw ch) #(0 0 0 0)) #f) *geometry-stack*)) (set! #<US: GS-15> (begin (layout-box s)))) (lambda () (set! *geometry-stack* #<US: GS-14>))) #<US: GS-15>)) (consume 50 50))))))) (lambda () (set! *geometry-stack* #<US: GS-8>))) #<US: GS-9>)) (let-geometry gx gy gw gh (if #f (consume 150 gh) (consume 150 #<US: GS-7>)))))))) (lambda () (set! *geometry-stack* #<US: GS-5>))) #<US: GS-6>)) (draw-surface-release! s) (flush-window w))))) (begin (write (string-append "; Cells allocated in (Update) = " (number->string (- (gc-info 5) #<US: GS-2>)) " (" (number->string (- (gc-info 6) #<US: GS-3>)) " env.)")) #<US: GS-4>)))>