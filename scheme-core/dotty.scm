;;;; dotty.scm
;
; Support for visualizinng Lisp data structures using dotty.
;
; !!! Dotty could use some work:
; - Factor out 'write-dotty-vertex' from write/dotty
; - Add some controls to vary the level of detail
; - Consider using generic functions for write-dotty-vertex and write/dotty
; - Put lisp-name->c-name and quote-c-string in another module
; - Add support for non-text characters to quote-c-string
; - Add hash table support
; - Add subr support
; - Closures should display their names in their vertex
;


(define-package "dotty"
  (:uses "scheme")
  (:exports "write/dotty"
            "dotty"))

(define (lisp-name->c-name name)
  "Converts a lisp style name to a c style name. Dashes are replaced
   with underscores, arrows with 2, and leading numbers are prefixed
   with an underscore. This algorithm does not guarantee that uniqueness
   of names is preserved (eg: foo2bar and foo->bar), but it does produce
   reasonable human-readable names."
  (if (symbol? name)
      (lisp-name->c-name (symbol-name name))
      (let ((ip (open-input-string name))
	    (op (open-output-string)))
	(let loop ((leading? #t))
	  (let ((ch (read-char ip)))
	    (cond ((and leading? (member ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
		   (display ch op))
		  ((and (eq? ch #\-) (eq? (peek-char ip) #\>))
		   (read-char ip) ; eat the #\>
		   (when leading?
		     (display #\_ op))
		   (display #\2 op))
		  ((eq? ch #\-)
		   (display #\_ op))
		  ((eof-object? ch)
		   )
		  (#t
		   (display ch op)))
	    (unless (port-at-end? ip)
	      (loop #f))))
	(get-output-string op))))

(define (quote-c-string string)
  "Returns <string>, with the necessary quotations to make it representable
   as a C-style string."
  (let ((ip (open-input-string string))
	(op (open-output-string)))
    (let next-char ((ch (read-char ip)))
      (unless (eof-object? ch)
	(case ch
	  ((#\\ #\")
	   (display #\\ op)
	   (display ch op))
	  ((#\newline) (display "\\n" op))
	  ((#\cr)      (display "\\r" op))
	  ((#\tab)     (display "\\t" op))
	  (#t
	   (display ch op)))
	(next-char (read-char ip))))
    (get-output-string op)))

(define (dotty-object-name x)
  "Returns a string naming the object in dotty syntax. Each object, distinguished
   by eq?, is guaranteed to have a unique name."
  (format #f "~a_~a" (lisp-name->c-name (type-of x))
	  (number->string (%obaddr x) 16)))

(define (write-dotty-edge p src src-elem dest)
  (when (and (not (null? src)) (not (null? dest)))
    (if src-elem
	(format p "~a:~a->~a;\n"
		(dotty-object-name src)
		src-elem
		(dotty-object-name dest))
	(format p "~a->~a;\n"
		(dotty-object-name src)
		(dotty-object-name dest)))))

(define *dotty-start-counter* 0)

(define (write/dotty x p)
  (format p "start_~a [ color=green, style=filled, label=\"start\" ]" *dotty-start-counter*)
  (format p "start_~a->~a;\n" *dotty-start-counter* (dotty-object-name x))
  (incr! *dotty-start-counter*)
  (let ((visited (make-hash :eq)))
    (let loop ((x x))
      (unless (hash-has? visited x)
	(hash-set! visited x #t)
	(case (type-of x)
	  ((character fixnum flonum complex boolean)
	   (format p "~a [ label=\"~s\" ];\n" (dotty-object-name x) x))
	  ((boolean)
	   (format p "~a\n" (dotty-object-name x)))
	  ((package)
	   (format p "~a [ label=\"~a\" ];\n" (dotty-object-name x) (package-name x)))
	  ((symbol)
	   (format p "~a [ label=\"~a\"];\n"
		   (dotty-object-name x)
		   (symbol-name x)))
	  ((closure)
	   (format p "~a [ shape=record, label=\"closure | { <code>code|<env>env|<p_list>p_list }\"];\n"
		   (dotty-object-name x))
	   (write-dotty-edge p x "code" (%closure-code x))
	   (write-dotty-edge p x "env" (%closure-env x))
	   (write-dotty-edge p x "p_list" (%procedure-property-list x))
	   (loop (%closure-code x))
	   (loop (%closure-env x))
	   (loop (%procedure-property-list x)))
	  ((lisp-array)
	   (format p "~a [ shape=record, label=\"#()|" (dotty-object-name x))
	   (let next-element ((i 0) (needs-seperator? #f))
	     (when (< i (length x))
	       (format p "~a<~a>" (if needs-seperator? "|" "") i)
	       (next-element (+ i 1) #t)))
	   (format p "\" ];\n")
	   (let next-element ((i 0))
	     (when (< i (length x))
	       (loop (vector-ref x i))
	       (write-dotty-edge p x i (vector-ref x i))
	       (next-element (+ 1 i)))))
	  ((string)
	   (format p "~a [ shape=record, label=\"string|~a\" ];\n"
		   (dotty-object-name x)
		   (quote-c-string x)))
	  ((cons)
	   (format p "~a [ shape=record, label=\"<car>|<cdr>\" ];\n" (dotty-object-name x))
	   (write-dotty-edge p x "car" (car x))
	   (write-dotty-edge p x "cdr" (cdr x))
	   (loop (car x))
	   (loop (cdr x)))))))

)
;    ((string hash macro)

(define (dotty . xs)
  (let ((dotty-filename (temporary-file-name "dot")))
    (with-port p (open-output-file dotty-filename)
      (format p "digraph G {\n")
      (dolist (x xs)
        (write/dotty x p))
      (format p "};\n"))
    ;;    (system (format #f "start /wait dotty ~a" dotty-filename)) - win32
    (system (format #f "dotty ~a" dotty-filename))
    (delete-file dotty-filename)))





