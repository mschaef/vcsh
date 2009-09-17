; plot.scm
; May 18th, 2006
;
; Temporary working file for plotting support

; !! stateless graphics
; !! log/log plots
; !! bar/column plots
; !! tick attributes 
; !! labeling
; !! pre-computed t-range

(define (sin-points)
  (let loop ((x -3.14) (pts ()))
    (if (<= x 3.14)
	(loop (scheme::+ x 0.01) (cons (point x (scheme::sin x)) pts))
	pts)))

(define (cos-points)
  (let loop ((x -3.14) (pts ()))
    (if (<= x 3.14)
	(loop (scheme::+ x 0.01) (cons (point x (scheme::cos x)) pts))
	pts)))

(define (tan-points)
  (let loop ((x -3.14) (pts ()))
    (if (<= x 3.14)
	(loop (scheme::+ x 0.01) (cons (point x (scheme::tan x)) pts))
	pts)))

(define (scalars->points xs) ; !! depends on map call order
  "Given a list of scalars <xs>, return the list as a series of evenly spaced
   points starting at y=0 and 1 unit apart."
  (let ((i -1))
    (map (lambda (x)
	   (incr! i)
	   (point i (scheme:real-part x)))
	 xs)))

(define point-vector-length scheme:magnitude)

(define (normalize-point-vector pt)
  "Given a point <pt>, representing a vector from 0+0i to a coordinate
   in 2-space, normalize it to a length of 1."
  (let ((r (point-vector-length pt)))
    (point (/ (point-x pt) r) (/ (point-y pt) r))))

(define (perpendicular-point-vector pt)
  "Given a point <pt>, representing a vector from 0+0i to a coordinate
   in 2-space, return a point representing a vector perpendicular to,
   and the same length as, <pt>."
  (point (point-y pt) (scheme:- (point-x pt))))

(define (interpolate t=0 t=1 t)
  (scheme:+ t=0 (scheme:* (scheme:- t=1 t=0) t)))

(define (t-for-x=0 x/t=0 x/t=1)
  "Given f(0)=<x/t=0> and f(1)=<x/t=1>, find t for f(t)=0."
  (check real? x/t=0)
  (check real? x/t=1)
  (scheme:/ (scheme:- x/t=0) (scheme:- x/t=1 x/t=0)))

(define (draw-axis start-pt end-pt range tick-interval)
  (let ((tick-direction (* 10 (perpendicular-point-vector (normalize-point-vector (scheme:- end-pt start-pt))))))

    (define (draw-ticks start-t end-t)
      (let ((tick-interval (if (scheme:> start-t end-t) (scheme:- tick-interval) tick-interval)))
	(let loop ((t start-t))
	  (when (within? t start-t end-t)
	    (let ((midpoint (interpolate start-pt end-pt t)))
	      (draw-line (scheme:- midpoint tick-direction) (scheme:+ midpoint tick-direction))
	      (loop (scheme:+ t tick-interval)))))))

    (draw-line start-pt end-pt)

    (when tick-interval
      (let ((t-origin (t-for-x=0 (car range) (cdr range))))
	(if (within? t-origin 0.0 1.0)
	    (begin
	      (draw-ticks (- t-origin tick-interval) 0.0)
	      (draw-ticks (+ t-origin tick-interval) 1.0))
	    (draw-ticks 0.0 1.0))))))


(define (axis-location scaler)
  "Given <scaler>, return two values, the y coordinate of the x-axis
   and the x-coordinate of the y-axis. If either axis is not visible,
   its coortinate is returned as #f. Both coordinates are reported in
   image coordinates."
  (let ((min-pt (scaler :min-pt))
	(max-pt (scaler :max-pt))
	(scaled-origin (scaler 0+0i)))
    (values (if (within? 0.0 (point-y min-pt) (point-y max-pt)) (point-y scaled-origin) #f)
	    (if (within? 0.0 (point-x min-pt) (point-x max-pt)) (point-x scaled-origin) #f))))


(define (draw-axis/x scaler range :optional (color #f) (ticks 0.1))
  (let ((min-pt (scaler :min-pt))
	(max-pt (scaler :max-pt)))
    (when (within? 0 (point-y min-pt) (point-y max-pt))
      (when color (set-foreground-color! color))
      (draw-axis (scaler (point (point-x min-pt) 0.0))
		 (scaler (point (point-x max-pt) 0.0))
		 range
		 ticks))))

(define (draw-axis/y scaler range :optional (color #f) (ticks 0.1))
  (let ((min-pt (scaler :min-pt))
	(max-pt (scaler :max-pt)))
    (when (within? 0 (point-x min-pt) (point-x max-pt))
      (when color (set-foreground-color! color))
      (draw-axis (scaler (point 0.0 (point-y min-pt)))
		 (scaler (point 0.0 (point-y max-pt)))
		 range
		 ticks))))


(define (point-scaler min-pt max-pt image-size)
  "Returns a closure that scales a point in the range <min-pt>-<max-pt>
   to a coordinate for a <image-size> image. Given :min-pt or :max-pt,
   the closure will return the correspoinding point."
  (let ((plot-range (scheme::- max-pt min-pt)))
    (lambda (point)
      (case point
	((:min-pt) min-pt)
	((:max-pt) max-pt)
	((#f) #f)
	(#t (point-scale (scheme:-  point min-pt) image-size plot-range))))))


(define (draw-marker center size :optional (style :cross))
  (case style
    ((:point)
     (draw-point center))
    ((:cross)
     (draw-line (scheme::- center (point size 0)) (scheme::+ center (point (scheme::+ 1 size) 0)))
     (draw-line (scheme::- center (point 0 size)) (scheme::+ center (point 0 (scheme::+ 1 size)))))
    ((:box)
     (draw-rectangle (scheme::- center (point size size))
		     (scheme::+ center (point size size) 1+1i)))
    ((:solid-box)
     (fill-rectangle (scheme::- center (point size size))
		     (scheme::+ center (point size size) 1+1i)))
    ((:circle)
     (draw-ellipse (scheme::- center (point size size))
		     (scheme::+ center (point size size) 1+1i)))
    ((:solid-circle)
     (fill-ellipse (scheme::- center (point size size))
		     (scheme::+ center (point size size) 1+1i)))
    ((:diamond)
     (draw-polyline (list (scheme::- center (point size 0))
			  (scheme::- center (point 0 size))
			  (scheme::+ center (point size 0))
			  (scheme::+ center (point 0 size))
			  (scheme::- center (point size 0)))))
    ))

(define *asymptote-threshold* 1.5)

(define (break-series-points-at-y-asymptotes points)
  (define (probable-asymptote? y-1 y-2)
    (and (scheme:< (scheme:* y-1 y-2) 0)
	 (not (and (infinite? y-1) (infinite? y-2)))
	 (or (infinite? y-1)
	     (infinite? y-2)
	     (scheme:> (scheme::log10 (scheme:abs (scheme:- y-1 y-2))) *asymptote-threshold*))))
  (let loop ((last-y #f) (remaining-points points) (accumulated-points ()))
    (if (null? remaining-points)
	(nreverse accumulated-points)
	(let ((this-point (car remaining-points)))
	  (cond ((not last-y)
		 (loop (point-y this-point) (cdr remaining-points) (cons this-point accumulated-points)))
		((probable-asymptote? last-y (point-y this-point))
		 (loop (point-y this-point) remaining-points (cons #f accumulated-points)))
		(#t
		 (loop (point-y this-point) (cdr remaining-points) (cons this-point accumulated-points))))))))
	   	   
(define (draw-series-points points scaler default-color args)
  (write `(draw-series-points points ,scaler ,default-color ,args))(newline) ; !!!!
  (if (parameterized-series? points)
      (draw-series-points (second points) scaler default-color (append (cddr points) args))
      (assoc-let args ((connect-points? :connect-points #t)
		       (point-line-color :point-line-color default-color)
		       (point-color :point-color default-color)
		       (point-style :point-style #f)
		       (point-size :point-size 2)
		       (fast-points :fast-points #f)
		       (x-axis-connector-color :x-axis-connector-color #f)
		       (y-axis-connector-color :y-axis-connector-color #f)
		       (break-at-y-asymptotes :break-at-y-asymptotes #t)
		       )
	
	(let ((scaled-points (map scaler (if break-at-y-asymptotes 
					     (break-series-points-at-y-asymptotes points)
					     point))))
	  (values-bind (axis-location scaler) (x-axis/y y-axis/x)
	    (when (and x-axis-connector-color x-axis/y)
	      (set-foreground-color! x-axis-connector-color)
	      (dolist (pt scaled-points)
		(draw-line pt (point (point-x pt) x-axis/y))))
	    (when (and y-axis-connector-color y-axis/x)
	      (set-foreground-color! y-axis-connector-color)
	      (dolist (pt scaled-points)
		(draw-line pt (point y-axis/x (point-y pt))))))

	  (when default-color 
	    (set-foreground-color! point-line-color))

	  (when connect-points?
	    (case connect-points?
	      ((:in-x-order)
	       (draw-polyline (qsort scaled-points > point-x)))
	      ((:in-y-order)
	       (draw-polyline (qsort scaled-points > point-y)))
	      (#t
	       (draw-polyline scaled-points))))

	  (when fast-points
	    (set-foreground-color! point-color)
	    (draw-point scaled-points))
	  (when point-style
	    (set-color! point-color)
	    (for-each (cut draw-marker <> point-size point-style) scaled-points))))))


      
(define (points-range . pointss)
  (define (comparable? x) (not (or (nan? x) (infinite? x))))
  (let loop ((points ()) (pointss pointss)
	     (min-x *most-positive-flonum*) 
	     (max-x *most-negative-flonum*) 
	     (min-y *most-positive-flonum*) 
	     (max-y *most-negative-flonum*))
    (cond ((null? points)
	   (if (null? pointss)
	       (values min-x max-x min-y max-y)
	       (let ((next-series (car pointss)))
		 (loop (if (parameterized-series? next-series)
			   (second next-series)
			   next-series)
		       (cdr pointss) min-x max-x min-y max-y))))
	  ((or (not (pair? points))
	       (not (point? (car points))))
	   (error "Bad points list: ~a" points))
	  (#t
	   (let ((pt-x (point-x (car points)))
		 (pt-y (point-y (car points))))
	     (loop (cdr points) pointss
		   (if (comparable? pt-x) (min min-x pt-x) min-x)
		   (if (comparable? pt-x) (max max-x pt-x) max-x)
		   (if (comparable? pt-y) (min min-y pt-y) min-y)
		   (if (comparable? pt-y) (max max-y pt-y) max-y)))))))

(define (valid-range? range) 
  (or (not range)
      (and (pair? range)
	   (real? (car range))
	   (real? (cdr range))
	   (< (car range) (cdr range)))))

(define (valid-image-size? size)
  (and (number? size)
       (> size 0)))


(define (autorange pointss args)
  "Given a list of point series, compute the bounding box, using the
   specified padding factors."
  (define (enforce-aspect-ratio x-range y-range ratio)
    (if ratio
	(values (max x-range (* y-range ratio))
		(max y-range (/ x-range ratio)))
	(values x-range 
		y-range)))
  (assoc-let args ((x-pad :autorange-x-pad 0.0)
		   (y-pad :autorange-y-pad 0.1)
		   (force-origin :autorange-force-origin #f)
		   (target-aspect-ratio :autorange-aspect-ratio #f))
	     (check (scheme:>= 0.0) y-pad)
	     (check (scheme:>= 0.0) x-pad)
	     (let ((pointss (if force-origin (cons (list 0.0+0.0i) pointss) pointss)))
	       (values-bind (apply points-range pointss) (min-x max-x min-y max-y)
		 (values-bind (enforce-aspect-ratio (- max-x min-x) (+ max-y min-y) target-aspect-ratio) (x-range y-range)
		   (values (cons (- min-x (* x-range x-pad))
				 (+ max-x (* x-range x-pad)))
			   (cons (- min-y (* y-range y-pad))
				 (+ max-y (* y-range y-pad)))))))))

(define *plot-color-list* (list *color-red*
				*color-green*
				*color-blue*
				*color-yellow*
				*color-cyan*
				*color-magenta*
				*color-dark-red*
				*color-dark-green*
				*color-dark-blue*
				*color-dark-yellow*
				*color-dark-cyan*
				*color-dark-magenta*
				))


(define (function-points function t-range steps function-type)
  (check procedure? function)
  (check valid-range? t-range)
  (check real? steps)
  
  (let* ((min-range (min (car t-range) (cdr t-range)))
	 (max-range (max (car t-range) (cdr t-range)))
	 (step-size (scheme::/ (scheme::- max-range min-range) (scheme::- steps 1))))
    (let loop ((i 0) (points ()))
      (let ((t (scheme::+ min-range (scheme::* i step-size))))
	(if (scheme::>= i steps)
	    (nreverse points)
	    (let ((fn-value (function t)))
	      (loop (scheme::+ i 1) (cons (case function-type
					    ((:function) (point t fn-value))
					    ((:rect-parametric) fn-value)
					    ((:polar) (polar->rect (point t fn-value)))
					    ((:polar-parametric) (polar->rect fn-value))
					    (#t (error "Invalid function type: ~a" function-type)))
					  points))))))))

(define (scalar-list? maybe-scalars)
  "Determines if <maybe-point> is a list of scalars."
  (and (list? maybe-scalars)
       (every? real? maybe-scalars)))

(define (point-list? maybe-points)
  "Determines if <maybe-points> is a list of points."
  (and (list? maybe-points)
       (every? point? maybe-points)))

(define (parameterized-series? series)
  (and (list? series) (eq? (car series) :series)))

(define (compute-series-points series args)
  (if (parameterized-series? series)
      (let ((series-args (append (cddr series) args)))
	(cons :series (cons (compute-series-points (second series) series-args) series-args)))
      (assoc-let* args ((series-type :series-type :function)
			(x-range :x-range '(-10 . 10))
			(t-range :t-range x-range)
			(image-size :image-size 501+501i)
			(steps :steps (point-x image-size)))
		  (cond ((procedure? series)
			 (function-points series t-range steps series-type))
			((scalar-list? series)
			 (scalars->points series))
			((point-list? series)
			 series)
			(#t
			 (error "Bad series to plot: ~s" series))))))


(define (plot args . plot-data-series)
  (write `(plot ,args ,@plot-data-series))(newline)
  (assoc-let args ((plot-colors :plot-colors *plot-color-list*)
		   (axis-color :axis-color *color-gray-80*)
		   (background-color :background-color *color-gray-20*)
		   (x-ticks :x-ticks 0.1)
		   (y-ticks :y-ticks 0.1)
		   (x-range :x-range '(-10 . 10))
		   (y-range :y-range :auto)
		   (image-size :image-size 501+501i))
	     
	     (let ((data-series-points (map (cut compute-series-points <> args) plot-data-series)))
	       
	       (when (or (eq? x-range :auto) (eq? y-range :auto))
		 (values-bind (autorange data-series-points args) (auto-x-range auto-y-range)		 
		   (when (eq? x-range :auto) (set! x-range auto-x-range))
		   (when (eq? y-range :auto) (set! y-range auto-y-range))))
	       
	       (check valid-range? x-range)
	       (check valid-range? y-range)
	       (check valid-image-size? image-size)
	       
	       (let ((scaler (point-scaler (point (car x-range) (cdr y-range)) (point (cdr x-range) (car y-range)) image-size)))
		 (with-new-image image-size
		     (set-color! background-color)
		   (fill-rectangle 0 image-size)
		   (dolist (points data-series-points)
		     (when (null? plot-colors)
		       (error "Out of plot colors!!!"))
		     (draw-series-points points scaler (car plot-colors) args)
		     (pop! plot-colors))
		   (when axis-color
		     (draw-axis/x scaler x-range axis-color x-ticks)
		     (draw-axis/y scaler y-range axis-color y-ticks)))))))


; !!! scheme:/   -- causes assert failure

(define (find-slopes xs)
  (let ((last-x #f))
    (fold (lambda (x xs)
	    (let ((rv (if last-x
			  (cons (- x  last-x) xs)
			  ())))
	      (set! last-x x)
	      rv))

	  #f
	  xs)))




(define (polar->rect p)
  (let ((theta (scheme::real-part p))
	(r (scheme::imag-part p)))
    (point (* r (scheme::cos theta))
	   (* r (scheme::sin theta)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interesting test plotts

(define (t1) (plot `((:background-color . ,*color-dark-blue*)
		     (:axis-color . ,*color-yellow*))
		   scheme:sin
		   scheme:cos))

(define (t1a) (plot `((:background-color . ,*color-dark-blue*)
		     (:axis-color . ,*color-yellow*)
		     (:image-size . 20+20i))
		   scheme:sin
		   scheme:cos))

(define (t2) (plot ()
		   scheme:sin
		   (lambda (x) (+ (/ (scheme:sin (* 1 x)) 1)
				  (/ (scheme:sin (* 3 x)) 3)
				  (/ (scheme:sin (* 5 x)) 5)))
		   ))


(define (t3) (plot '((:x-range . :auto)
		     (:y-range . :auto)
		     (:autorange-x-pad . 0.1)
		     (:autorange-y-pad . 0.1)
		     )
		   '(1 2 3 4 5 8 7)))

(define (t3a) (plot '((:autorange-force-origin . #t)
		      (:x-range . :auto)
		      (:y-range . :auto)
		     (:autorange-x-pad . 0.1)
		     (:autorange-y-pad . 0.1)
		      ) 
		    '(1 2 3 4 5 8 7)))

(define (t3b) (plot `((:autorange-force-origin . #t)
		      (:x-range . :auto)
		      (:y-range . :auto)
		      (:autorange-x-pad . 0.1)
		      (:autorange-y-pad . 0.1)
		      (:x-axis-connector-color . ,*color-gray-50*)
		      (:y-axis-connector-color . ,*color-gray-50*)
		      (:point-style . :solid-circle)
		      (:point-color . ,*color-white*)
		      (:point-line-color . ,*color-red*)
		      ) 
		    '(1 2 3 4 5 8 7)))


(define (t3c) (plot `((:autorange-force-origin . #t)
		      (:x-range . :auto)
		      (:y-range . :auto)
		      (:autorange-x-pad . 0.1)
		      (:autorange-y-pad . 0.1)
		      (:x-axis-connector-color . ,*color-white*)
		      (:point-style . :solid-circle)
		      (:point-color . ,*color-white*)
		      ) 
		    '(2 4 -6 5 8 4 -2 4 7 9 0)))


(define (t4) (plot ()
		   scheme:sin
		   scheme:cos
		   (lambda (x) (+ (scheme:sin x) (scheme:cos x)))
		   (lambda (x) (+ (scheme:sin x) (scheme:cos (* x 2))))
		   ))

(define (t5) (apply plot (cons '((:x-range 0 . 3.14)) (map (lambda (ofs) (lambda (x) (scheme::sin (- x ofs)))) 
				       '(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2)))))


(define (t6 :optional (image-size 500+500i)) 
  (plot `((:image-size . ,image-size)
	  (:x-ticks . #f)
	  (:y-ticks . #f))
	(lambda (t) t) (lambda (t) (scheme::- t))))

(define (t7) (plot () round identity (always 1) (always 2)))

(define (t8) (plot '((:t-range 0 . 6.28)
		     (:x-range . :auto)
		     (:y-range . :auto))
		   (list :series scheme::sin '(:series-type . :polar))))


(define (t8a) (plot '((:t-range 0 . 6.28)
		      (:x-range . :auto)
		      (:y-range . :auto)
		      (:series-type . :polar))
		    scheme:sin))

(define (t8b) (plot '((:t-range 0 . 6.28)
		      (:x-range . :auto)
		      (:y-range . :auto)
		      (:series-type . :polar))
		    (lambda (t) (scheme:sin (* t 9)))))

(define (t9) (plot '((:t-range 0 . 30)
		     (:x-range . :auto)
		     (:y-range . :auto))
		   (list :series scheme::identity '(:series-type . :polar))))


(define (r2) (scheme::- (scheme::+ (scheme::random 1+1i) (scheme::random 1+1i)) 1.0+1.0i))

(define (bunches proc count)
  (let loop ((i 0) (xs ()))
    (if (> i count)
	xs
	(loop (+ 1 i) (cons (proc) xs)))))

(define (t10 :optional (count 100))
  (plot '((:x-range . :auto)
	  (:y-range . :auto)
	  (:connect-points . #f))
	(bunches r2 count)))
	  

(define (t11 :optional (xs 1) (ys 1))
  (plot '((:x-range . :auto)
	  (:y-range . :auto)
	  (:t-range -13.56 . 13.56)
	  (:series-type . :rect-parametric))
	(lambda (t)
	  (point (scheme::sin (* xs t)) (scheme::cos (* ys t))))))

(define (t12)
  (map (lambda (style/size)
	 (let ((style (car style/size))
	       (size (cadr style/size)))
	   (with-new-image 50+50i
	       (set-color! *color-white*)
	     (draw-marker 25+25i size style)
	     (set-color! *color-red*)
	     (draw-point 25+25i))))
       (list-combinations '((:cross :box :solid-box :circle :solid-circle :point)
			    (0 1 2 3 4 5 6 7 8 9 10)))))







(define a1 '(10.0+8.04i 8.0+6.9i 13.0+7.5i 9.0+8.8i 11.0+8.3i
			14.0+9.9i 6.0+7.2i 4.0+4.2i 12.0+10.8i 7.0+4.8i
			5.0+5.6i))

(define a2 '(10.0+9.1i 8.0+8.1i 13.0+8.7i 9.0+8.7i 11.0+9.2i
		       14.0+8.1i 6.0+6.1i 4.0+3.1i 12.0+9.1i 7.0+7.2i
		       5.0+4.7i))

(define a3 '(10.0+7.4i 8.0+6.7i 13.0+11.7i 9.0+7.1i 11.0+7.8i
		       14.0+8.8i 6.0+6.0i 4.0+5.3i 12.0+8.1i 7.0+6.4i
		       5.0+5.7i))


(define a4 '(8.0+6.5i 8.0+5.7i 8.0+7.7i 8.0+8.8i 8.0+8.4i
		      8.0+7.0i 8.0+5.2i 19.0+12.5i 8.0+5.5i 8.0+7.9i
		      8.0+6.8i))

(define (t13) (plot '((:autorange-force-origin . #t)
		      (:connect-points . #f)
		      (:x-range . :auto)
		      (:y-range . :auto)
		      (:point-style . :cross)
		      (:autorange-x-pad . 0.1)
		      (:autorange-y-pad . 0.1))
		    a1 a2 a3 a4))

(define (T13) (plot '((:autorange-force-origin . #t)
		      (:connect-points . #f)
		      (:x-range . :auto)
		      (:y-range . :auto)
		      (:t-range 0 . 16) ; !!! t-range when x-range is auto
		      (:point-style . :solid-circle)
		      (:point-size . 2)
		      (:autorange-x-pad . 0.1)
		      (:autorange-y-pad . 0.1)
		      (:image-size . 800+800i))
		    (list :series
			  (lambda (t) (scheme::+ (scheme::* t 0.792607392607393) 0.3247152847152846))
			  '(:point-style . #f)
			  '(:connect-points . #t))
		    a1 a2 a3 a4))
 



(define (T13x) (plot '((:autorange-force-origin . #t)
		      (:connect-points . :in-x-order)
		      (:x-range . :auto)
		      (:y-range . :auto)
		      (:point-style . :solid-circle)
		      (:point-size . 2)
		      (:autorange-x-pad . 0.1)
		      (:autorange-y-pad . 0.1)
		      (:image-size . 800+800i))
		    a1 a2 a3 a4))

(define (t13x) (plot '((:autorange-force-origin . #t)
		      (:connect-points . :in-x-order)
		      (:x-range . :auto)
		      (:y-range . :auto)
		      (:point-style . :cross)
		      (:autorange-x-pad . 0.1)
		      (:autorange-y-pad . 0.1))
		    a1 a2 a3 a4))

(define (t13y) (plot '((:autorange-force-origin . #t)
		      (:connect-points . :in-y-order)
		      (:x-range . :auto)
		      (:y-range . :auto)
		      (:point-style . :cross)
		      (:autorange-x-pad . 0.1)
		      (:autorange-y-pad . 0.1))
		    a1 a2 a3 a4))

(define (t14) (plot '((:x-range -4 . 4)(:y-range -5 . 2)(:image-size . 200)) gamma))

(define (t14a) (plot '((:x-range -4 . 4)(:y-range -5 . 2)(:image-size . 200)) 
		    gamma
		    (lambda (t) (- (gamma t) (gamma (+ t 0.01))))))

(define (t15) (plot () (lambda (x) (scheme:/ 1 x)) (lambda (x) (scheme:/ 1 (scheme:+ 1 x)))))

(define (tos-data)
  (map (lambda (t) (point (first t) (second t))) (filter list? (car *stack*))))

 (set! vcalc::*flonum-print-precision* 2)
 (set! vcalc::*print-length* 4)
 .t plot draw-series-points compute-series-points