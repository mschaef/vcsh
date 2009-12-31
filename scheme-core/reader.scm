;;;; reader.scm
;;;;
;;;; The scan reader

(in-package! "scheme")

;;;; Syntax table

(define-structure syntax-table
  "Defines actions taken by the reader upon encountering specific characters."

  (name
   "The user friendly name of the syntax table."
   :default (gensym "syntax-table")
   :set #f)

  (char-mapping
   "A mapping between characters and the action the reader takes upon encountering
    those characters. An action can either be a function to be invoked or a nested
    syntax-table."
   :default (make-hash :eq)

   ;; The mapping hash can be mutated, but not rebound.
   :set #f)

  (default-mapping
    "The default action to be taken if a character is not found in char-mapping."

    :default #f))


(define (reader-action? reader-action)
  "Predicate that determines if <reader-action> is a valid reader action. Valid
   reader actions include #f, to signal that a character is invalid, a syntax-table
   to be recursively consulted, a closure to be invoked, or a symbol that will be
   dereferenced at read time to find a valid reader action."
  (or (symbol? reader-action)
      (syntax-table? reader-action)
      (closure? reader-action)
      (not reader-action)))

(define (set-char-syntax! syntax-table char reader-action)
  "Sets the reader action in <syntax-table> for <char> to <reader-action>."
  (check syntax-table? syntax-table)
  (check char? char)
  (check reader-action? reader-action)
  (hash-set! (syntax-table-char-mapping syntax-table)
             char
             reader-action)
  reader-action)

(define (set-default-syntax! syntax-table reader-action)
  "Sets the default reader action in <syntax-table> to <reader-action>."
  (check syntax-table? syntax-table)
  (check reader-action? reader-action)
  (set-syntax-table-default-mapping! syntax-table reader-action)
  reader-action)

(define (char-syntax syntax-table char)
  "Returns the reader action for <char> based on the specified <syntax-table>.
   If there is no action, returns #f."
  (check char? char)
  (let loop ((syntax-binding (aif (hash-ref (syntax-table-char-mapping syntax-table) char #f)
                             it
                             (syntax-table-default-mapping syntax-table))))
    (cond ((symbol? syntax-binding)
           ;; REVISIT: possibility of infinite loop...
           (loop (symbol-value syntax-binding)))
          ((reader-action? syntax-binding)
           syntax-binding)
          (#t
           (error "Invalid syntax binding ~s for ~s in ~s."
                  syntax-binding char syntax-table)))))


;;; The default syntax table

(define *read-syntax* (make-syntax-table :name 'read-syntax))
(define *readsharp-syntax* (make-syntax-table :name 'readsharp-syntax))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-char-syntax! *read-syntax* #\# '*readsharp-syntax*))

;;;; Utility functions

(define (read-expected-char char port error-type)
  "Read the next character from <port>, and optionally issue a read error if it is
   not <char>.  <char> can be either a character, a list of characters, or
   a predicate function on characters. The return value is the expected
   character. <error-type> determines whether or not an unexpected character
   is reported as a read error, and must either be #f or the name of a valid
   read error type. Unexpected characters are left on the port."
  (let ((location (port-location port))
        (correct-char? (etypecase char
                         ((character) #L(eq? char _))
                         ((closure) char)
                         ((cons) #L(memq _ char)))))
    (if (and (not (port-at-end? port))
             (correct-char? (peek-char port)))
        (read-char port)
        (if error-type
            (read-error error-type port location)
            #f))))

;;;; The main reader

(define *location-mapping* (make-hash :eq))

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
        ;; TODO: hex character escapes in strings
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

  ;; TODO: hex character escapes in characters
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

(define (read-instance port)
  (read-char port)
  (let* ((instance-location (port-location port))
         (init-list (read-list port))
         (base-instance (car init-list))
         (init-args (cdr init-list)))
    (unless (or (symbol? base-instance) (not base-instance) (instance? base-instance))
      (read-error :reader-bad-base-instance port instance-location))
    (apply make-instance base-instance init-args)))

(define (read-structure port)
  (read-char port)
  (apply make-structure-by-name (read port)))

(define (read-true port)
  (read-char port)
  #t)

(define (read-false port)
  (read-char port)
  #f)

(define (char-symbol-constituent? ch) ;; TODO: The printer should honor this predicate when printing symbols
  "Returns <ch> if it is a character that is normally read as part of a symbol
   even if unescaped. Returns #f otherwise."
  (if (or (char-whitespace? ch)
          (memq ch '( #\( #\) #\[ #\]  #\'
                      #\; #\: #\" #\# #\, #\ #\\)))
      #f
      ch))

(define (read-token port :optional (accept-any-first-character? #f))
  "Reads a token from <port>. A token is defined as the sequence of characters
   up to the next unescaped character with syntactic significance to the reader."
  (let ((buf (open-output-buffer)))
    (let loop ((escaped? #f) (first? #t))
      (let ((ch (peek-char port)))
        (cond ((eof-object? ch)
               (get-output-string buf))
              (escaped?
               ;; REVISIT: Should read-token have a escape for char-literals (\xFF, etc.)
               (write-strings buf (read-char port))
               (loop #f #f))
              ((and first? accept-any-first-character?)
               (write-strings buf (read-char port))
               (loop #f #f))
              ((eq? ch #\\)
               (write-strings buf  (read-char port))
               (loop #t #f))
              ((or (char-symbol-constituent? ch)
                   (eq? ch #\:))
               (write-strings buf (read-char port))
               (loop #f #f))
              (#t
               (get-output-string buf)))))))

(define (read-fixnum-with-radix port radix)
  (let ((location (port-location port)))
    (read-char port) ; skip radix char
    (aif (string->number (read-token port) radix)
         it
         (read-error :reader-bad-number-syntax-syntax port location))))

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

(define (read-hash port)
  (read-char port)
  (let ((elems (read-list port)))
    (list->hash elems)))

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

(define (read-structure port)
  (read-char port)
  (apply make-structure-by-name (read port)))

(define (read-and-evaluate port)
  (read-char port)
  (eval (read port)))

(define (read-shebang port) ; For Unix-style shell script support
  (read-line port)
  (read port))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-char-syntax! *readsharp-syntax* #\I 'read-instance)
  (set-char-syntax! *readsharp-syntax* #\\ 'read-character)
  (set-char-syntax! *readsharp-syntax* #\f 'read-false)
  (set-char-syntax! *readsharp-syntax* #\t 'read-true)
  (set-char-syntax! *readsharp-syntax* #\b 'read-fixnum-with-radix-2)
  (set-char-syntax! *readsharp-syntax* #\o 'read-fixnum-with-radix-8)
  (set-char-syntax! *readsharp-syntax* #\e 'read-fixnum-with-radix-10)
  (set-char-syntax! *readsharp-syntax* #\d 'read-fixnum-with-radix-10)
  (set-char-syntax! *readsharp-syntax* #\x 'read-fixnum-with-radix-16)
  (set-char-syntax! *readsharp-syntax* #\( 'read-vector)
  (set-char-syntax! *readsharp-syntax* #\h 'read-hash)
  (set-char-syntax! *readsharp-syntax* #\n 'read-with-read-time-eval)
  (set-char-syntax! *readsharp-syntax* #\i 'read-inexact)
  (set-char-syntax! *readsharp-syntax* #\; 'read-sexpr-comment)
  (set-char-syntax! *readsharp-syntax* #\@ 'read-annotated-object)
  (set-char-syntax! *readsharp-syntax* #\S 'read-structure)
  (set-char-syntax! *readsharp-syntax* #\. 'read-and-evaluate)
  (set-char-syntax! *readsharp-syntax* #\! 'read-shebang)
  (set-char-syntax! *readsharp-syntax* #\L 'read-short-lambda)
  (set-char-syntax! *readsharp-syntax* #\S 'read-structure))


;; TODO: Add default readsharp syntax that throws an error

;; The list reader

(define (read-sequence port begin-char end-char)
  (check input-port? port)
  (check char? begin-char)
  (check char? end-char)
  (let ((list-location (port-location port))
        (current-list ())
        (last-list-cell #f))

    (define (extend-list! new-cdr)
      "Extends the current list by appending the <new-cdr> to the last cell of
       the current list.  If the current list is dotted, throws an invalid dotted
       list error.  The return value is the <new-cdr>."
      (cond ((pair? last-list-cell)
             (set-cdr! last-list-cell new-cdr))
            ((and (not last-list-cell) (null? current-list))
             (set! current-list new-cdr))
            (#t
             (read-error :reader-bad-dotted-list port list-location)))
      (set! last-list-cell new-cdr))

    (read-expected-char begin-char port :reader-list-expected)
    (let loop ()
      (let ((ch (flush-whitespace port)))
        (cond ((eof-object? ch)
               (read-error :reader-eos-in-list port list-location))
              ((eq? ch end-char)
               (read-char port)
               current-list)
              (#t
               (let* ((token-location (port-location port))
                      (next-object (read port #t)))
                 (cond ((or (eq? next-object '.)
                            (eq? next-object *reader-dot-marker*))
                        (extend-list! (read port #t)))
                       (#t
                        (let ((new-cdr (extend-list! (cons next-object))))
                          (when *location-mapping*
                             (hash-set! *location-mapping* new-cdr (cons port token-location)))))))
               (loop)))))))

(define (read-list port)
  (read-sequence port #\( #\)))

(define *reader-quotes-literal-lists* #f)

(define (read-literal-list port)
  (let ((l (read-list port)))
    (if *reader-quotes-literal-lists*
        `',l
        l)))

(define (read-message-send port)
  (let ((location (port-location port))
        (form (read-sequence port #\[ #\])))
    (unless (and (>= (length form) 2) (symbol? (second form)))
      (read-error :reader-bad-message-send port location))
    `(send ,(first form) ',(second form) ,@(cddr form))))

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
  (cond ((not (read-expected-char #\: port #f)) #f)
        ((not (read-expected-char #\: port #f)) :public-symbol)
        (#t                                  :private-symbol)))

(define (accept-symbol port  error-port error-location) ; TODO: *read-symbol-hook* to allow symbols to be read but not interned
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
                 (values-bind (find-symbol segment-2 sym-package) (sym status)
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
      (awhen (read-expected-char '(#\- #\+) port #f)
        (write-strings buf it))
      (awhile (read-expected-char char-numeric? port #f)
        (write-strings buf it))
      (awhen (read-expected-char #\. port #f)
        (write-strings buf it)
        (awhile (read-expected-char char-numeric? port #f)
          (write-strings buf it)))
      (awhen (read-expected-char '(#\e #\E) port #f)
        (write-strings buf it)
      (awhen (read-expected-char '(#\- #\+) port #f)
        (write-strings buf it))
        (awhile (read-expected-char char-numeric? port #f)
          (write-strings buf it)))
      (if (> (length buf) 0)
          (let ((num (string->number (get-output-string buf))))
            (if (and (number? num) *reader-defaults-to-flonum*)
                (+ num 0.0)
                num))
          #f)))

  (let ((re (accept-single-number)))
    (cond ((read-expected-char #\i port #f)
           (and (port-at-end? port)
                re
                (make-rectangular 0 re)))
          ((port-at-end? port)
           re)
          (#t
           (let ((im (accept-single-number)))
             (if (and im
                      (read-expected-char #\i port #f)
                      (port-at-end? port))
                 (make-rectangular re im)
                 #f))))))

;; An internal marker value used internally to signal that a 'dot' has been read.
(define *reader-dot-marker* (gensym "reader-dot-marker"))

(define (read-number-or-symbol port)
  (let* ((location (port-location port))
         (tok (read-token port)))
    (if (equal? tok ".")
        *reader-dot-marker*
        (aif (accept-number (open-input-string tok) port location)
             it
             (accept-symbol (open-input-string tok) port location)))))

(define (read-unexpected-close port)
  ;; Skip the unexpected close, in order to make progress
  (read-char port)
  (read-error :reader-unexpected-close port (port-location port)))

(define (read-quote port)
  (read-char port)
  (list 'quote (read port #t)))

(define (read-quasiquote port)
  (read-char port)
  (list 'quasiquote (read port #t)))

(define (read-slot-reference port)
  (read-char port)
  (let ((location (port-location port))
        (ref-form (read port)))
    (cond ((symbol? ref-form)
           `(slot-ref self ',ref-form))
          ((and (list? ref-form) (length=2? ref-form) (symbol? (second ref-form)))
           `(slot-ref ,(first ref-form) ',(second ref-form)))
          (#t
           (read-error :reader-bad-slot-reference port location)))))

(define (read-unquote port)
  ;; This is used as a default handler for a syntax map, so
  ;; the usual read process has already consumed the leading comma.
  ;; (read-char port)
  (list 'unquote (read port #t)))

(define (read-unquote-splicing port)
  (read-char port)
  (list 'unquote-splicing (read port #t)))

(define (read-unquote-splicing-destructive port)
  (read-char port)
  (list 'unquote-splicing-destructive (read port #t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-char-syntax! *read-syntax* #\( 'read-literal-list)
  (set-char-syntax! *read-syntax* #\) 'read-unexpected-close)

  (set-char-syntax! *read-syntax* #\[ 'read-message-send)
  (set-char-syntax! *read-syntax* #\] 'read-unexpected-close)

  (set-char-syntax! *read-syntax* #\" 'read-string)

  (set-char-syntax! *read-syntax* #\' 'read-quote)

  (set-char-syntax! *read-syntax* #\` 'read-quasiquote)
  (set-char-syntax! *read-syntax* #\@ 'read-slot-reference)

  (let ((st (make-syntax-table :name 'unquote-syntax)))
    (set-char-syntax! *read-syntax* #\, st)
    (set-char-syntax! st #\@    'read-unquote-splicing)
    (set-char-syntax! st #\.    'read-unquote-splicing-destructive)
    (set-default-syntax! st  'read-unquote))

  (set-default-syntax! *read-syntax* 'read-number-or-symbol))


(define (read :optional (port (current-input-port)) (recursive? #f))
  (check input-port? port)
  (flush-whitespace port)
  (let* ((location (port-location port))
         (obj (let loop ((syntax-table *read-syntax*))
                (let ((ch (peek-char port)))
                  (if (eof-object? ch)
                      ch
                      (aif (char-syntax syntax-table ch)
                           (if (syntax-table? it)
                               (begin
                                 (read-char port)
                                 (loop it))
                               (it port))
                           (read-error :reader-unknown-syntax port location)))))))
    (when (and *location-mapping* (not (scheme::%immediate? obj)))
       (hash-set! *location-mapping* obj (cons port location)))
    obj))
