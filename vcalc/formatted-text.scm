;;;; formatted-text.scm
;;;; Mike Schaeffer
;
; Text formatting.

;;; Formatted text
;
; This set of functions supports drawing of formatted text.
; The basic syntax for formatted text is a list of the
; following format:
;
; <text> := ( <format-modifier> . ( <text> ... ) )
; <text> := <string>
;
; The resulting list is traversed left to right and the text
; is placed on the drawing surface as it is encountered. 


(define *default-font* '( "Courier New" 24))

(define (parse-text-modifier m)
  "A modifier is an atom that is used to change a text attribute. A string adjusts the face name, numbers adjusts the face size, and symbols add and remove attributes from the attribute list. (If the symbol begins with a !, the corresponding symbol is removed, otherwise it's added)"
  (cond ((eq? m #f)  m)
        ((string? m) m)
        ((number? m) m)
        ((symbol? m) 
          (let ((symstring (symbol-name m)))
             (if (eq? (string-ref symstring 0) #\!)
                (cons #f 
                     (intern!
                        (string-append 
                          ":"
                          (substring symstring 
                                     1
                                     (string-length symstring)))))
               (cons #t m))))
        (#t
          (error "Invalid text style modifier [ ~a ] in parse-modifier" m))))


(define (apply-text-modifier desc modifier)
  "Apply one or more modifiers to a text description. This function recurses over a list of
   modifiers, using each one to rewrite a font description. The nested function apply-text-modifer
   contains the code that applies a single parsed modifier to a description."  
  (let ((modifier (parse-text-modifier modifier)))
    (cond ((eq? modifier #f)
	   desc)
	  ((string? modifier)
	   (list-set! (list-copy desc) 0 modifier))
	  ((number? modifier)
	   (list-set! (list-copy desc) 1 modifier))
	  ((pair? modifier)
	   (let ((add-sym? (car modifier))
		 (sym (cdr modifier)))
	     (cons (car desc)
		   (cons (cadr desc)
			 (if add-sym?
			     (if (member sym (cddr desc))
				 (cddr desc)
				 (cons sym (cddr desc)))
			     (delete sym (cddr desc))))))))))

(define *linearize-cache* (make-hash))
(define *linearize-cache-enabled* #t)


(define (split-string-by-whitespace string) ; REVISIT: use the standard split-string
  "Split a string by spaces into a list of substrings"
  (let ((is (open-input-string string)))
    (define (next-substring)
      (let ((os (open-output-string)))
	(while (not (or (port-at-end? is)
			(char-whitespace? (peek-char is))))
	       (display (read-char is) os))
	(get-output-string os)))
    (define (split-next substrings in-whitespace?)
      (cond ((port-at-end? is)
	     (reverse substrings))
	    ((char-whitespace? (peek-char is))
	     (read-char is)
	     (split-next (if in-whitespace? substrings (cons " " substrings)) #t))
	    (#t
	     (split-next (cons (next-substring) substrings) #f))))
    (split-next '() #f)))

(define (linearize-formatted-text f-text)
  "This function translates a formatted text tree, as described above, into a linear list of discrete text segment. The segment list takes the following form: ( :linear-text ( <format> . <text> )  ... )"
  (define (linearize-formatted-text-1 f-text desc)
    (cond ((keyword? f-text)
	   (let ((str (system-string f-text)))
	     (if str
		 (list (cons desc (system-string f-text)))
		 (list (cons desc (string-append (display-to-string f-text) " not found!"))))))
	  ((string? f-text)
	   (map (lambda (sub-text) 
		     (cons desc sub-text)) 
		   (split-string-by-whitespace f-text)))

	  ((pair? f-text)
	   (let ((new-desc (apply-text-modifier desc (car f-text))))
	     (append-map! (lambda (t) (linearize-formatted-text-1 t new-desc))
			  (cdr f-text))))
	  ((null? f-text)
	   (list (cons desc "")))
	  (#t
	   (error "Invalid formatted text [ ~a ] in linearize-formatted-text" f-text))))

  (if *linearize-cache-enabled*
      (let ((cached-result (hash-ref *linearize-cache* f-text)))
	(if (not cached-result)
	    (let ((result (cons :linear-text (linearize-formatted-text-1 f-text *default-font*))))
	      (hash-set! *linearize-cache* f-text result)
	      result)
	    cached-result))
      (cons :linear-text (linearize-formatted-text-1 f-text *default-font*))))

(define (flush-text-cache)
  (set! *linearize-cache* (make-hash)))

(define (linear-text? text)
  (and (pair? text) (eq? (car text) :linear-text)))

(define (ensure-linear-text text)
  (if (linear-text? text)
      text
      (linearize-formatted-text text)))

(define (draw-formatted-text pt f-text :optional s)
  "Draw formatted text on the drawing surface s. The text is drawn from
               left to right, starting at x and with the baseline of the text at y.
               Note: for a scheme implementation of draw-linear-text, see VCalcGraphics.cpp"
  (draw-linear-text pt (cdr (ensure-linear-text f-text)) '() s))
  
(define (measure-formatted-text f-text :optional s)
  "Computes the extent of the given text. The extent is returned as a
               list of the following format: ( ( <height> . <width> ) . <baseline-adjust> )"
  (cdr (measure-linear-text (cdr (ensure-linear-text f-text)) '() s)))

(define (flow-formatted-text text wrap-limit :optional surface)
  "Split formatted text into multiple lines"
  (define (flow-remaining remaining-text line-extents)
    (if (null? remaining-text)
	(reverse line-extents)
	(let ((line-extent (measure-linear-text remaining-text wrap-limit surface)))
	  (flow-remaining (car line-extent) 
			  (cons (cons remaining-text (cdr line-extent)) line-extents)))))
  (flow-remaining (cdr (ensure-linear-text text)) '()))
    

; !!!!! This is crap
(define (draw-flowed-text pt w flowed-text :optional s)
  (define (draw-next-line lines y)
    (let ((line (car lines)))
      (unless (null? lines)
	(draw-linear-text (point (point-x pt) (scheme::+ y
							 (caddr line)
							 (scheme::- (cadddr line))))
			  (car line) w s)
	(draw-next-line (cdr lines) (scheme::+ y (caddr line))))))
  (draw-next-line flowed-text (point-y pt)))

(define (flowed-text-size flowed-text :optional s)
  (define (next-line lines xs ys)
    (if (null? lines)
	(cons xs ys)
	(next-line (cdr lines)
		   (max xs (cadar lines))
		   (+ ys (caddar lines)))))
  (next-line flowed-text 0 0))
	
(define (flow-test formatted-text w)
  (with-new-image w 200
    (let* ((flowed-text (flow-formatted-text formatted-text w))
	   (size (flowed-text-size flowed-text)))
      (fill-rectangle (point 0 0) (point w 200))
      (draw-flowed-text (point 0 0) w flowed-text))))
      
(define test-text   "The quick brown fox jumps over the lazy dog.  The quick brown fox jumps over the lazy dog.  The quick brown fox jumps over the lazy dog.  The quick brown fox jumps over the lazy dog.  The quick brown fox jumps over the lazy dog.  The quick brown fox jumps over the lazy dog.  The quick brown fox jumps over the lazy dog.  The quick brown fox jumps over the lazy dog.  The quick brown fox jumps over the lazy dog.  The quick brown fox jumps over the lazy dog.")


(define (draw-anchored-text at anchor f-text :optional s)
  "Draw formatted text on the drawing surface <s>. The text is drawn
               from left to right. It is placed in such a way that the point (<x>, <y>)
               is at the <anchor> corner of a bounding box surrounding the text."  
  (let* ((text (ensure-linear-text f-text))
	 (extent (measure-formatted-text text s)))
    (let ((xs (car extent))
	  (ys (cadr extent))
	  (ba (caddr extent)))
      (let ((x (point-x at))
	    (y (+ (point-y at) (- ys ba))))
	(draw-formatted-text (case anchor 
			       ((:sw) (point x               (- y ys)))
			       ((:s)  (point (- x (/ xs 2))  (- y ys)))
			       ((:se) (point (- x xs)        (- y ys)))
			       ((:w)  (point x               (- y (/ ys 2))))
			       ((:c)  (point (- x (/ xs 2))  (- y (/ ys 2))))
			       ((:e)  (point (- x xs)        (- y (/ ys 2))))
			       ((:nw) (point x               y))
			       ((:n)  (point (- x (/ xs 2))  y))
			       ((:ne) (point (- x xs)        y))
			       (#t (error "Invalid text anchor:  ~a." anchor)))
			     text s)))))
