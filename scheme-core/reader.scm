
;;;; reader.scm --
;;;;
;;;; The Lisp Reader.
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(in-package! "scheme") ;; REVISIT: Is this necessary?

;;; The default syntax table

;;;; Utility functions

(define (read-expected-char char port error-type)
  "Read the next character from <port> optionally issue a read error
if it is not <char>.  <char> can be either a character, a list of
characters, or a predicate function on characters. The return value is
the expected character. <error-type> is the name of a valid read error
type. Unexpected characters are left on the port."
  (let ((correct-char? (etypecase char
                         ((character) #L(eq? char _))
                         ((closure) char)
                         ((cons) #L(memq _ char)))))
    (if (correct-char? (peek-char port))
        (read-char port)
        (read-error error-type port (port-location port)))))

(define (read-optional-char char port)
  "Read the next character from <port>, and return #f if it is not
<char>.  <char> can be either a character, a list of characters, or a
predicate function on characters. The return value is the expected
character or #f if not found."
  (let ((correct-char? (etypecase char
                         ((character) #L(eq? char _))
                         ((closure) char)
                         ((cons) #L(memq _ char)))))
    (if (correct-char? (peek-char port))
        (read-char port)
        #f)))

;;;; Error strings for the reader

(define-text
  :read-error-bad-symbol-syntax "Bad symbol syntax"
  :read-error-package-not-found "Package not found"
  :read-error-symbol-not-found-in-package "Symbol not found in package"
  :read-error-symbol-private-to-package "Symbol private to package"
  :reader-bad-base-instance "Bad base instance"
  :reader-bad-character-code "Bad character code"
  :reader-bad-date-syntax "Bad date syntax"
  :reader-bad-dotted-list "Bad dotted list"
  :reader-bad-escape "Bad escape"
  :reader-bad-inexact-number-syntax "Bad inexact number syntad"
  :reader-bad-number-syntax-syntax "Bad number syntax"
  :reader-bad-readsharp-syntax "Bad readsharp syntax"
  :reader-bad-slot-reference "Bad slot reference"
  :reader-bad-time-syntax "Bad time syntax"
  :reader-eos-in-list "End of input while reading list"
  :reader-eos-in-string "End of input while reading string"
  :reader-unexpected-close "Unexpected close"
  :reader-unexpected-open "Unexpected open"
  :reader-unknown-syntax "Unknown syntax"
  )

;;;; The main reader

(define (read-error error-type port location . args)
  "Signals a read error."
  (abort 'read-error error-type port location args))

(define *location-mapping* (make-identity-hash))

(define (open-output-buffer)
  (let ((buf (open-output-string)))
    (set-port-translate-mode! buf #f)
    buf))

(define (read-string port)

  (define (read-string-escape)
    (define (digit-char->number char)
      (- (char->integer char) (char->integer #\0)))
    (let* ((escape-location (port-location port))
           (ch (read-char port)))
      (case ch
        ((#\n) #\newline)
        ((#\t) #\tab)
        ((#\r) #\cr)
        ((#\d) #\eot)
        ((#\s) #\space)
        ((#\\) #\\)
        ((#\") #\")
        ;; REVISIT: hex character escapes in strings
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
         (let loop ((code (digit-char->number ch))
                    (ii 0))
           (let ((ch (peek-char port)))
             (cond ((or (not (char-octal? ch))
                        (>= ii 2))
                    (when (or (< code 0) (> code 255))
                          (read-error :reader-bad-character-code port escape-location))
                    (integer->char code))
                   ((char-octal? ch)
                    (loop (+ (* 8 code) (digit-char->number (read-char port))) (+ ii 1)))
                   (#t
                    (read-error :reader-bad-character-code port escape-location))))))
        (#t
         (if (port-at-end? port)
             (read-error :reader-eos-in-string port escape-location)
             (read-error :reader-bad-escape port escape-location))))))


  (let ((string-location (port-location port))
        (string-text (open-output-buffer port)))
    (read-expected-char #\" port :reader-bad-syntax)
    (let read-next ()
      (let ((ch (read-char port)))
        (cond ((eof-object? ch)
               (read-error :reader-eos-in-string port string-location))
              ((eq? ch #\\)
               (write-strings string-text (read-string-escape))
               (read-next))
              ((eq? ch #\")
               (get-output-string string-text))
              (#t
               (write-strings string-text ch)
               (read-next)))))))



(define (read-character port)

  ;; REVISIT: hex character escapes in characters
  (define (code-character text char-location)
    (unless (and (eq? #\< (string-ref text 0))
                 (eq? #\> (string-ref text (- (length text) 1)))
                 (> (length text) 2))
            (read-error :reader-bad-character-code port char-location))
    (let ((code (string->number (substring text 1 (- (length text) 1)))))
      (when (or (< code 0) (> code 255))
            (read-error :reader-bad-character-code port char-location))
      (integer->char code)))

  (define (lookup-character-name name)
    (catch 'found
           (dotimes (ii (length *character-names*))
                    (when (equal? name (vector-ref *character-names* ii))
                          (throw 'found (integer->char ii))))
           #f))

  (read-char port)
  (let* ((char-location (port-location port))
         (char-text (read-token port #t)))
    (cond ((and (> (length char-text) 1)
                (eq? #\< (string-ref char-text 0)))
           (code-character char-text char-location))
          ((= (length char-text) 1)
           (string-ref char-text 0))
          (#t
           (aif (lookup-character-name char-text)
                it
                (begin
                  (unless (= 1 (length char-text))
                          (read-error :reader-bad-character-code port char-location))
                  (string-ref char-text 0)))))))

(define (read-true port)
  (read-char port)
  #t)

(define (read-false port)
  (read-char port)
  #f)

(define *charset-symbol-delimiter* 
  '#.(charset-vector *charset-whitespace*
                     #\( #\) #\[ #\] #\' #\{ #\}
                     #\; #\: #\" #\# #\, #\ #\\))

(define (char-symbol-delimiter? ch)
  "Returns <ch> if it is a character that is normally read as a delimiter between
   symbols if unescaped. Returns #f otherwise."
  (vector-ref *charset-symbol-delimiter* ch))

(define (char-symbol-constituent? ch)
  "Returns <ch> if it is a character that is normally read as part of a symbol
   even if unescaped. Returns #f otherwise."
  (not (vector-ref *charset-symbol-delimiter* ch)))

(define (read-token port :optional (accept-any-first-character? #f))
  "Reads a token from <port>. A token is defined as the sequence of characters
   up to the next unescaped character with syntactic significance to the reader."
  (let ((buf (open-output-buffer)))
    (define (escaped)
      (unless (port-at-end? port)
        (write-strings buf (read-char port)))
      (non-escaped))
    (define (non-escaped)
      (let ((ch (peek-char port)))
        (cond ((eof-object? ch)
               (get-output-string buf))
              ((and accept-any-first-character? (= (length buf) 0))
               (write-strings buf (read-char port))
               (non-escaped))
              ((eq? ch #\\)
               (write-strings buf (read-char port))
               (escaped))
              ((or (char-symbol-constituent? ch)
                   (eq? ch #\:))
               (write-strings buf (read-char port))
               (non-escaped))
              (#t
               (get-output-string buf)))))
    (non-escaped)))


(define (accept-fixnum-with-radix port radix error-port error-location :optional (error-return :throw-error))
  (aif (string->number (read-token port) radix)
       it
       (if (eq? error-return :throw-error)
           (read-error :reader-bad-number-syntax error-port error-location)
           error-return)))

(define (read-fixnum-with-radix port radix)
  (let ((location (port-location port)))
    (read-char port) ; skip radix char
    (accept-fixnum-with-radix port radix port location)))

(define (read-fixnum-with-radix-2  port)
  (read-fixnum-with-radix port 2))

(define (read-fixnum-with-radix-8  port)
  (read-fixnum-with-radix port 8))

(define (read-fixnum-with-radix-10 port)
  (read-fixnum-with-radix port 10))

(define (read-fixnum-with-radix-16 port)
  (read-fixnum-with-radix port 16))

(define (read-vector port)
  (let ((elems (read-list port)))
    (list->vector elems)))

(define (read-with-read-time-eval port)
  (let ((obj (read port #t)))
    (eval obj)))

(define (read-inexact port)
  (read-char port)
  (let* ((location (port-location port))
         (token (read-token port)))
    (cond ((equal? token "nan") #inan)
          ((equal? token "posinf") #iposinf)
          ((equal? token "neginf") #ineginf)
          (#t (read-error :reader-bad-inexact-number-syntax port location)))))

(define (read-sexpr-comment port)
  (read-char port)
  ;; Skip the next s-expr...
  (read port #t)
  ;; ...and return the following.
  (read port #t))

(define (read-annotated-object port)
  (let ((location (port-location port)))
    (read-text-until-character port #\=)
    (when (port-at-end? port)
       (read-error :reader-bad-readsharp-syntax port location))
    (read-char port)
    (read port #t)))

(define (read-and-evaluate port)
  (read-char port)
  (eval (read port)))

(define (read-shebang port) ; For Unix-style shell script support
  (read-line port)
  (read port))

(define read-date)
(define read-time)

;;;; Short Lambda
;;
;; Abbreviated syntax for lambda expressions.

(defmacro (shorter-lambda . form)
  "Defines a function with an implicit lambda list of (_). This
   is intended to be used with a readsharp handler that shortens
   the syntax."
  `(lambda (_) ,@form))

(defmacro (short-lambda args . form)
  "Defines a function with an implicit lambda list of
   (take '(_0 _1 _2 _3 _4 _5) args). This is intended to be
   used with a readsharp handler that shortens the syntax."
  (runtime-check (and exact? (>= 0) (< 6)) args)
  `(lambda ,(if args (take '(_0 _1 _2 _3 _4 _5) args) '(_))
     ,@form))

(define (read-short-lambda port)
  (read-char port)
  (if (char=? (peek-char port) #\()
      `(shorter-lambda ,(read port))
      `(short-lambda ,(read port) ,(read port))))


;;; String quasiquote
;;;
;;; This is a facility for embedding lisp expressions within
;;; strings in the style of shell scripting languages. Within
;;; a quasiquoted string, #\$ signals the beginning of a substitution
;;; expression, enclosed in #\{ and #\}. These expressions are evaluated
;;; at runtime, coerced to a string with ->string, and embedded in the
;;; result string.

(defmacro (string-quasiquote package string)
  (let ((ip (open-input-string string)))
    (define (read-string-segment)
      (begin-1
       (read-text-until-character ip #\$)
       (read-char ip)))
    (define (read-expression-segment)
      (let ((ch (flush-whitespace ip)))
        (case ch
          ((#\{)
           (begin-2
            (read-char ip)
            `(->string
              ,(with-package package (read-from-string (read-text-until-character ip #\}))))
            (read-char ip)))
          ((#\$)
           (read-char ip)
           "$")
          (#t
           (error "Invalid string-quasiquote syntax: ~s" string)))))
    (let loop ((reading-text? (if (char=? (peek-char ip) #\$)
                                  (begin (read-char ip) #f) ; Must skip the #\$...
                                  #t))
               (current-segments ()))
      (if (port-at-end? ip)
          ;; This check removes the string-append call in the degenerate case where
          ;; there are no substitution variables. In the case where all there is is
          ;; a substitution variable, we don't remove the call to string-append
          ;; in the interest of safety. (If the substitution expression doesn't
          ;; return a string, this results in more predicable failures.)
          (if (and (length=1? current-segments) (string? (car current-segments)))
              (car current-segments)
              `(string-append ,@(reverse! current-segments)))
          (loop (not reading-text?)
                (cons ((if reading-text? read-string-segment read-expression-segment))
                      current-segments))))))

(define (read-string-quasiquote port)
  (when (char=? (peek-char port) #\")
    `(string-quasiquote ,*package* ,(read port))))

(define (read-readsharp port)
  (read-expected-char #\# port :reader-bad-readsharp-syntax)
  (let ((location (port-location port)))
    (case (peek-char port)
      ((#\\) (read-character port))
      ((#\f) (read-false port))
      ((#\t) (read-true port))
      ((#\b) (read-fixnum-with-radix-2 port))
      ((#\o) (read-fixnum-with-radix-8 port))
      ((#\e) (read-fixnum-with-radix-10 port))
      ((#\d) (read-fixnum-with-radix-10 port))
      ((#\x) (read-fixnum-with-radix-16 port))
      ((#\() (read-vector port))
      ((#\n) (read-with-read-time-eval port))
      ((#\i) (read-inexact port))
      ((#\;) (read-sexpr-comment port))
      ((#\@) (read-annotated-object port))
      ((#\.) (read-and-evaluate port))
      ((#\!) (read-shebang port))
      ((#\L) (read-short-lambda port))
      ((#\D) (read-date port))
      ((#\T) (read-time port))
      ((#\") (read-string-quasiquote port))
      (#t (read-error :reader-bad-readsharp-syntax port location)))))

;; The list reader

(define (read-sequence port begin-char end-char)
  (runtime-check input-port? port)
  (runtime-check char? begin-char)
  (runtime-check char? end-char)
  (let ((list-location (port-location port))
        (current-sequence (%make-q)))
    (read-expected-char begin-char port :reader-list-expected)
    (let loop ((seen-dot? #f))
      (let ((ch (flush-whitespace port)))
        (cond ((eof-object? ch)
               (read-error :reader-eos-in-list port list-location))
              ((eq? ch end-char)
               (read-char port)
               (%q-items current-sequence))
              (seen-dot?
               (read-error :reader-bad-dotted-list port list-location))
              (#t
               (let* ((token-location (port-location port))
                      (next-object (read port #t)))
                 (cond ((or (eq? next-object '.)
                            (eq? next-object *reader-dot-marker*))
                        (%q-enqueue-cell! (read port #t) current-sequence)
                        (loop #t))
                       (#t
                        (let ((new-cdr (cons next-object)))
                          (%q-enqueue-cell! new-cdr current-sequence)
                          (when *location-mapping*
                            (hash-set! *location-mapping* new-cdr
                                       (cons port token-location))))
                        (loop #f))))))))))


(define (read-list port)
  (read-sequence port #\( #\)))

(define *reader-quotes-literal-lists* #f)

(define (read-literal-list port)
  (let ((l (read-list port)))
    (if *reader-quotes-literal-lists*
        `',l
        l)))

(define (read-literal-vector port)
  (list->vector (read-sequence port #\[ #\])))

(define (read-literal-hash port)
  (list->hash (read-sequence port #\{ #\})))

(define (accept-symbol-segment port)
  "Reads the next symbol segment from <port>, returning #f if there is no
   such segment."
  (define (accept-segment-text)
    (let ((buf (open-output-buffer)))
      (let loop ((escaped? #f))
        (let ((ch (peek-char port)))
          (cond ((port-at-end? port)
                 (get-output-string buf))
                ((or escaped?
                     (char-symbol-constituent? ch))
                 (write-strings buf (read-char port))
                 (loop #f))
                ((eq? ch #\\)
                 (read-char port)
                 (loop #t))
                (#t
                 (get-output-string buf)))))))
  (let ((text (accept-segment-text)))
    (if (> (length text) 0)
        text
        #f)))

(define (accept-symbol-package-qualifier port)
  (cond ((not (read-optional-char #\: port)) #f)
        ((not (read-optional-char #\: port)) :public-symbol)
        (#t                                  :private-symbol)))

; REVISIT: *ignore-read* - There's a better name for this in cltl2, but the
; basic idea is to have a way to put the reader into a mode where it can
; safely read through stuff and not intern symbols or make other mutations
; to the environment.
 
(define (accept-symbol port  error-port error-location) ; REVISIT: *read-symbol-hook* to allow symbols to be read but not interned
  "<error-port> and <error-location> are used to report any read
   errors in symbol parsing."
  (let* ((segment-1 (accept-symbol-segment port))
         (package-qualifier (accept-symbol-package-qualifier port))
         (segment-2 (accept-symbol-segment port)))
    (cond ((not (port-at-end? port))
           (read-error :read-error-bad-symbol-syntax error-port error-location))
          ((and (not segment-1) package-qualifier segment-2)
           (intern-keyword! segment-2))
          ((and segment-1 package-qualifier (not segment-2))
           (intern-keyword! segment-1))
          (package-qualifier
           (let ((sym-package (find-package segment-1)))
             (unless sym-package
                (read-error :read-error-package-not-found error-port error-location segment-1))
             (if (or (eq? package-qualifier :private-symbol)
                     (eq? *package* sym-package))
                 (intern! segment-2 sym-package)
                 (mvbind (sym status) (find-symbol segment-2 sym-package)
                   (unless sym
                     (read-error :read-error-symbol-not-found-in-package error-port error-location segment-2 sym-package))
                   (unless (eq? status :external)
                     (read-error :read-error-symbol-private-to-package error-port error-location segment-2 sym-package))
                   sym))))
          (#t
            (intern! segment-1)))))

(define *reader-defaults-to-flonum* #f)

(define (accept-number port error-port error-location)

  (define (accept-single-number)
    (let ((buf (open-output-buffer)))
      (awhen (read-optional-char '(#\- #\+) port)
        (write-strings buf it))
      (awhile (read-optional-char char-numeric? port)
        (write-strings buf it))
      (awhen (read-optional-char #\. port)
        (write-strings buf it)
        (awhile (read-optional-char char-numeric? port)
          (write-strings buf it)))
      (awhen (read-optional-char '(#\e #\E) port)
        (write-strings buf it)
      (awhen (read-optional-char '(#\- #\+) port)
        (write-strings buf it))
        (awhile (read-optional-char char-numeric? port)
          (write-strings buf it)))
      (if (> (length buf) 0)
          (let ((num (string->number (get-output-string buf))))
            (if (and (number? num) *reader-defaults-to-flonum*)
                (+ num 0.0)
                num))
          #f)))

  (let ((re (accept-single-number)))
    (cond ((read-optional-char #\i port)
           (and (port-at-end? port)
                re
                (make-rectangular 0 re)))
          ((port-at-end? port)
           re)
          (#t
           (let ((im (accept-single-number)))
             (if (and im
                      (read-optional-char #\i port)
                      (port-at-end? port))
                 (make-rectangular re im)
                 #f))))))

;; An internal marker value used internally to signal that a 'dot' has been read.
(define *reader-dot-marker* (gensym "reader-dot-marker"))

(define (accept-c-number port error-port error-location)
  (if (eq? #\0 (peek-char port))
      (begin
        (read-char port)
        (if (port-at-end? port)
            0
            (if (eq? #\x (char-downcase (peek-char port)))
                (begin
                  (read-char port)
                  (accept-fixnum-with-radix port 16 error-port error-location #f))
                (accept-fixnum-with-radix port 8 error-port error-location #f))))
      #f))

(define (read-number-or-symbol port)
  (let* ((location (port-location port))
         (tok (read-token port)))
    (if (equal? tok ".")
        *reader-dot-marker*
        (or (accept-c-number (open-input-string tok) port location)
            (accept-number (open-input-string tok) port location)
            (accept-symbol (open-input-string tok) port location)))))

(define (read-unexpected-open port)
  ;; Skip the unexpected open, in order to make progress
  (read-char port)
  (read-error :reader-unexpected-open port (port-location port)))

(define (read-unexpected-close port)
  ;; Skip the unexpected close, in order to make progress
  (read-char port)
  (read-error :reader-unexpected-close port (port-location port)))

(define (read-quote port)
  (read-char port)
  (list 'quote (read port #t)))

(define (read-slot-reference port)
  (read-char port)
  (let ((location (port-location port))
        (ref-form (read port)))
    (cond ((symbol? ref-form)
           `(slot-ref self ',ref-form))
          ((and (list? ref-form)
                (symbol? (second ref-form))
                (or (length=2? ref-form)
                    (length=3? ref-form)))
           (if (length=2? ref-form)
               `(slot-ref ,(first ref-form) ',(second ref-form))
               `(slot-ref ,(first ref-form) ',(second ref-form) ,(third ref-form))))
          (#t
           (read-error :reader-bad-slot-reference port location)))))

(define (read-form port)
  (case (peek-char port)
    ((#\() (read-literal-list port))
    ((#\)) (read-unexpected-close port))
    ((#\[) (read-literal-vector port))
    ((#\]) (read-unexpected-close port))
    ((#\{) (read-literal-hash port))
    ((#\}) (read-unexpected-close port))
    ((#\") (read-string port))
    ((#\#) (read-readsharp port))
    ((#\`) (read-quasiquote port))
    ((#\,) (read-unquote port))
    ((#\') (read-quote port))
    ((#\@) (read-slot-reference port))
    (#t (read-number-or-symbol port))))

(define (read :optional (port (current-input-port)) (recursive? #f))
  (runtime-check input-port? port)
  (flush-whitespace port)
  (let ((location (port-location port))
        (obj (or (eof-object? (peek-char port))
                 (read-form port))))
    (when (and *location-mapping* (not (scheme::%immediate? obj)))
       (hash-set! *location-mapping* obj (cons port location)))
    obj))
