;;;; vcinit.scm
;;;; Mike Schaeffer
;;
;; A program object with a postfix user representation.


;;;; postfix parser

(define (token-stream tokens)
  "Given a list of <tokens> retuns a token stream, a closure that
   supports operations on the token stream. :peek - peeks at the
   next token, :read - destructively reads the next token, :dup -
   creates a duplicate copy of the stream, :expect - returns #t
   or #f depending on if the next token is the expected value.
   :next is the same stream destructively advanced to the next token."
  (check list? tokens)
  (letrec ((self (lambda (op :optional expected)
		   (case op
		     ((:peek) (if (null? tokens) (%make-eof) (car tokens)))
		     ((:read) (if (null? tokens) (%make-eof) (pop! tokens)))
		     ((:dup) (token-stream tokens))
		     ((:at-end?) (null? tokens))
		     ((:next-token-is?) 
		      (and (eq? (self :peek) expected)
			   (self :read)))
		     (#t (error "Bad token-stream op: ~s" op))))))
    self))

;; Notes on postfix syntax:
;;
;; statement := 
;;    * Any one Lisp object
;;    * <if-statement>
;;    * <while-statement>
;;    * <repeat-statement>
;;    * <unwind-protect-statement>
;;    * <statement-sequence>
;;
;; statement-sequence :=
;;    * { statement* }
;;
;; if-statement :=
;;     if <statement> then <statement>
;;     if <statement> then <statement> else <statement>
;;
;; while-statement :=
;;     while <statement> end
;;
;; repeat-statement :=
;;     repeat <statement> 
;;
;; unwind-protect-statement :=
;;     unwind-protect <statement> <statement>
;;

(define *open-brace* '{ )
(define *close-brace* '} )

(define postfix-syntax-error error)

(define (postfix-if-statement stream)
  (let* ((cond-statement (postfix-statement stream))
	 (then-statement (postfix-statement stream))
	 (has-else? (stream :next-token-is? 'else))
	 (else-statement (if has-else? (postfix-statement stream) ())))
    `(begin
       ,cond-statement
       (if (stack-pop)
	   ,then-statement 
	   ,@(if has-else? (list else-statement) ())))))

(define (postfix-repeat-statement stream)
  (let ((loop-body (postfix-statement stream)))
    `(repeat (stack-pop)
       (pump-messages)
       ,loop-body)))

(define (postfix-while-statement stream)
  (with-gensyms (loop-sym)
    (let* ((cond-statement (postfix-statement stream))
	   (loop-body (postfix-statement stream)))
      `(begin
	 (let ,loop-sym ()
	      ,cond-statement
	      (when (stack-pop)
		(pump-messages)
		,loop-body
		(,loop-sym)))))))

(define (postfix-unwind-protect-statement stream)
  (let* ((body-statement (postfix-statement stream))
	 (protect-statement (postfix-statement stream)))
    `(unwind-protect 
      (lambda () ,body-statement)
      (lambda () ,protect-statement))))

(define (postfix-statement stream)
  (cond ((stream :at-end?)
	 (postfix-syntax-error "Expected token in stream"))
	((stream :next-token-is? 'repeat)          (postfix-repeat-statement stream))
	((stream :next-token-is? 'while)           (postfix-while-statement stream))
	((stream :next-token-is? 'if)              (postfix-if-statement stream))
	((stream :next-token-is? 'unwind-protect)  (postfix-unwind-protect-statement stream))
	((stream :next-token-is? *open-brace*)     (postfix-statements stream #t))
	(#t
	 (let ((statement (stream :read)))
	   (if (symbol? statement)
	       `(apply-to-stack ,statement)
	       `(stack-push ,statement))))))

(define (postfix-statements stream statement-block?)
  (let loop ((tokens ()))
    (cond ((and (stream :at-end?) statement-block?)
	   (postfix-syntax-error "End of statement in statement list"))
	  ((or (stream :at-end?)
	       (stream :next-token-is? *close-brace*))
	   `(begin (pump-messages) ,@(reverse! tokens)))
	  (#t
	   (loop (cons (postfix-statement stream) tokens))))))

(define (expand-postfix-code code)
  (postfix-statements (token-stream code) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro (postfix-program-object args docs body)
  `(scheme::%lambda ((postfix ,@(recursive-list-copy body))
	     (lambda-list ,@args))
	    ,args
	    (begin
	      ,(expand-postfix-code body)
	      (values))))

(define (postfix-program-object? object)
  (and (closure? object)
        (scheme::%procedure-property object 'postfix)))

(define (objects->postfix-program objects)
  "Given a list of <objects>, returns a program that evaluates the list
   as if it were the body of an 0-arity postfix program."
  (eval `(postfix-program-object () "" ,objects)))

; postfix code object reader

(define (read-postfix-program ip)
  (read-char ip)
  (let ((location (port-location ip)))
    (if (char=? (peek-char ip) #\{)
        (read-error :postfix-program-written-with-environment ip location))
    (let loop ((reading-arguments? #t)
               (arguments ())
               (doc-string #f)
               (body ())
               (nest-level 0))
      (define (current-program)
        `(postfix-program-object ,(reverse arguments) ,doc-string ,(reverse body)))
      (let ((ch (flush-whitespace ip)))
        (cond ((eof-object? ch)
               (read-error :incomplete-postfix-program ip location))
              ((and reading-arguments? (char=? ch #\|))
               (read-char ip)
               (loop #f arguments doc-string body nest-level))
              (reading-arguments?
               (let ((next-form (read ip)))
                 (cond ((symbol? next-form)
                        (loop reading-arguments? 
                              (cons next-form arguments) 
                              doc-string 
                              body
                              nest-level))
                       ((string? next-form)
                        (loop reading-arguments? 
                              arguments 
                              next-form 
                              body
                              nest-level))
                       (#t
                        (read-error :invalid-postfix-lambda-object ip location)))))
              ((and (= nest-level 0) (char=? ch #\}))
               (read-char ip)
               (current-program))
              ((char=? ch #\})
               (read-char ip)
               (loop reading-arguments? arguments doc-string (cons *close-brace* body) (- nest-level 1)))
              ((char=? ch #\{)
               (read-char ip)
               (loop reading-arguments? arguments doc-string (cons *open-brace* body) (+ nest-level 1)))
              (#t
               (loop reading-arguments? 
                     arguments 
                     doc-string 
                     (cons (read ip) body)
                     nest-level)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-char-syntax! *readsharp-syntax* #\{ 'read-postfix-program))





