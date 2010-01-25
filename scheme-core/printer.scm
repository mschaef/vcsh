;;;; printer.scm
;;;; Mike Schaeffer
;;;
;;; The Lisp printer.

(define *print-shared-structure* #t)
(define *print-packages-always* #f)
(define *allow-rich-writes* #t)
(define *flonum-print-precision* 5)
(define *print-addresses* #f)
(define *print-length* #f)
(define *print-depth* #f)
(define *pretty-print-syntax* #t)
(define *print-readably* #f)

(define *current-print-depth* 0)

(set-property! 'quote 'pretty-print-syntax "'")
(set-property! 'quasiquote 'pretty-print-syntax "`")
(set-property! 'unquote 'pretty-print-syntax ",")
(set-property! 'unquote-splicing 'pretty-print-syntax ",@")
(set-property! 'unquote-splicing-destructive 'pretty-print-syntax ",.")

(define *printer-index-key* (gensym "printer-index-key")) ;; REVISIT: better suited as structure field?

(define (printer-shared-structures object) ; REVISIT: should this be made a gf?
  "Returns an identity hash of all printable objects referenced by <object>
   more than once. This includes both circular and shared structure. The
   value associated with each hash is #f."
  (define (ignored-for-sharing? obj)
    "Determine if <obj> should be ignored for the purpose sharing. We ignore
     immediate objects and interned symbols, as they are shared, by default."
    (or (%immediate? obj)
        (and (symbol? obj)
             (symbol-package obj))))
  (let ((visited-objects (make-hash :eq))
        (to-visit (cons object)))
    (let loop ()
      (unless (null? to-visit)
        (let ((o (pop! to-visit)))
          (cond ((ignored-for-sharing? o)
                 )
                ((hash-has? visited-objects o)
                 (hash-set! visited-objects o #t))
                (#t
                 (hash-set! visited-objects o #f)
                 (case (%representation-of o)
                   ((cons)
                    (push! (car o) to-visit)
                    (push! (cdr o) to-visit))
                   ((vector)
                    (dolist (x o)
                      (push! x to-visit)))
                   ((structure)
                    (dolist (slot-name (structure-slots o))
                      (push! (structure-slot-by-name o slot-name) to-visit)))
                   ((instance)
                    ;; Don't check the slots themselves, since we
                    ;; do not print them by default.
                    (push! (%instance-proto o) to-visit))
                   ((hash)
                    (dolist (k/v (hash->a-list o))
                      (push! (car k/v) to-visit)
                      (push! (cdr k/v) to-visit)))
                   ((fast-op)
                    (dolist (op-piece (%fast-op-args o))
                      (push! op-piece to-visit)))))
                (#t
                 ())))
        (loop)))
    (let ((table (make-hash :eq)))
      (dohash (k v visited-objects (hash-set! table *printer-index-key* 0))
          (when v
            (hash-set! table k #f))))))

(define (at-length-check-limit? elem)
  "Returns #t if a sequence element in position <elem> should
   not be displayed due to the length limit check."
  (and (exact? *print-length*)
       (> elem *print-length*)))

(define (maybe-rich-write obj machine-readable? port)
  "Returns <port> if <obj> was successfully rich written to
   the port. Returns #f otherwise."
  (if *allow-rich-writes*
      (rich-write obj machine-readable? port)
      #f))

(define *character-names* #("nul"      "soh"      "stx"
                            "etx"      "eot"      "eng"
                            "ack"      "bel"      "bs"
                            "tab"      "newline"  "vtab"
                            "formfeed" "cr"       "so"
                            "si"       "dle"      "dc1"
                            "dc2"      "dc3"      "dc4"
                            "nak"      "syn"      "etb"
                            "can"      "em"       "sub"
                            "esc"      "fs"       "gs"
                            "rs"       "us"       "space"))

(define (%obaddr-string obj)
  (let ((str (string-append "00000000" (number->string (%obaddr obj) 16))))
    (substring str (- (length str) 8))))

(defmacro (unreadable . code)
  `(begin
     (when *print-readably*
       (error "Cannot print unreadable object."))
     ,@code))

(define (print-unreadably obj port :optional (print-details #f))
  (unreadable (write-strings port "#<")
              (print (%representation-of obj) port #t #f)
              (when *print-addresses*
                (write-strings port "@#x" (%obaddr-string obj)))
              (when print-details
                (print-details))
              (write-strings port ">")))

(defmacro (print-unreadable-object obj port . forms)
  `(print-unreadably ,obj ,port (lambda () ,@forms)))

(define (call-with-new-print-level port fn)
  (dynamic-let ((*current-print-depth* (+ *current-print-depth* 1)))
    (cond ((and (number? *print-depth*)
                (> *current-print-depth* *print-depth*))
           (unreadable (write-strings port "#<(...)>")))
          (#t
           (fn)))))

(defmacro (with-new-print-level port . code)
  `(call-with-new-print-level ,port (lambda () ,@code)))

(define-generic-function (print-object obj port machine-readable? shared-structure-map)
  (print-unreadable-object obj port))

(define (print obj port machine-readable? shared-structure-map)
  (when (eq? *print-addresses* :all)
    (write-strings port "#@#x" (%obaddr-string obj) "="))
  (acond ((maybe-rich-write obj machine-readable? port)
          port)
         ((or (not shared-structure-map)
              (not (hash-has? shared-structure-map obj)))
          (print-object obj port machine-readable? shared-structure-map))
         ((number? (hash-ref shared-structure-map obj))
          (unreadable (write-strings port "#" (number->string it) "#")))
         (#t
          (let ((next-number (hash-ref shared-structure-map *printer-index-key*)))
            (hash-set! shared-structure-map  obj next-number)
            (hash-set! shared-structure-map *printer-index-key* (+ 1 next-number))
            (unreadable (write-strings port "#" (number->string next-number) "="))
            (print-object obj port machine-readable? shared-structure-map)))))

(define (printer obj port machine-readable?)
  (let ((shared-structure-map (if *print-shared-structure*
                                  (printer-shared-structures obj)
                                  #f)))
    (print obj port machine-readable? shared-structure-map)))


;;; Specializations of print-object

(define-method (print-object (obj cons) port machine-readable? shared-structure-map)
  (define (pretty-print-syntax obj)
    (if (and *pretty-print-syntax*
             ;; Explict check for length=2?, but guaranteed to not throw errors
             (pair? obj) (pair? (cdr obj)) (null? (cddr obj)))
        (get-property (car obj) 'pretty-print-syntax)
        #f))
  (with-new-print-level port
    (acond ((pretty-print-syntax obj)
            (write-strings port it)
            (print (cadr obj) port machine-readable? shared-structure-map))
           (#t
            (write-strings port "(")
            (print (car obj) port machine-readable? shared-structure-map)
            (let loop ((elem 1) (xs (cdr obj)))
              (cond ((at-length-check-limit? elem)
                     (unreadable (format port " #<... n=~a>)" (length xs))))
                    ((null? xs)
                     )
                    ((or (atom? xs)
                         (and shared-structure-map
                              (hash-has? shared-structure-map xs)))
                     (write-strings port " . ")
                     (print xs port machine-readable? shared-structure-map))
                    (#t
                     (write-strings port " ")
                     (print (car xs) port machine-readable? shared-structure-map)
                     (loop (+ elem 1) (cdr xs)))))
            (write-strings port ")")))))

(define-method (print-object (obj character) port machine-readable? shared-structure-map)
  (let ((ord (char->integer obj)))
    (cond ((not machine-readable?)
           (write-strings port obj))
          ((< ord (length *character-names*))
           (write-strings port "#\\" (vector-ref *character-names* ord)))
          ((>= ord 127)
           (write-strings port "#\\<" (number->string ord) ">"))
          (#t
           (write-strings port "#\\" obj)))))


(define-method (print-object (obj symbol) port machine-readable? shared-structure-map)
  (define (write-symbol-string port string)
    (let next-segment ((beg 0))
      (let ((end (string-first-character string ":\\ \n\t" beg)))
        (cond (end
               (write-strings port
                              (substring string beg end)
                              #\\
                              (string-ref string end))
               (next-segment (+ end 1)))
              (#t
               (write-strings port (substring string beg)))))))
  (cond ((not (symbol-package obj))
         (write-strings port "#:" (symbol-name obj))
         (when *print-addresses*
           (write-strings port "@#x" (%obaddr-string obj))))
        ((and (not *print-packages-always*)
              (values-bind (find-symbol (symbol-name obj) *package*) (found-sym status)
                (eq? found-sym obj)))
         (write-symbol-string port (symbol-name obj)))
        ((keyword? obj)
         (write-strings port ":")
         (write-symbol-string port (symbol-name obj)))
        (#t
         (write-symbol-string port (package-name (symbol-package obj)))
         (write-strings port (if (exported? obj) ":" "::"))
         (write-symbol-string port (symbol-name obj)))))

(define-method (print-object (obj vector) port machine-readable? shared-structure-map)
  (with-new-print-level port
    (write-strings port "#(")
    (let loop ((ii 0) (need-space? #f))
      (cond ((at-length-check-limit? ii)
             (unreadable (format port " #<... n=~a>)" (- (length obj) ii))))
            ((< ii (length obj))
             (when need-space?
               (write-strings port " "))
             (print (vector-ref obj ii) port machine-readable? shared-structure-map)
             (loop (+ ii 1) #t))
            (#t
             (write-strings port ")"))))))

(define-method (print-object (obj subr) port machine-readable? shared-structure-map)
  (print-unreadable-object obj port
    (print (%primitive-kind obj) port #t #f)
    (write-strings port ":")
    (print (procedure-name obj) port #f #f)))

(define-method (print-object (obj closure) port machine-readable? shared-structure-map)
  (if (pair? (%closure-code obj))
      (print-unreadable-object obj port
        (print (cons (get-property obj 'name)
                            (car (%closure-code obj)))
                      port
                      machine-readable?
                      #f))
      (print-unreadable-object obj port)))

(define-method (print-object (obj structure) port machine-readable? shared-structure-map)
  (with-new-print-level port
    (write-strings port "#S(")
    (print (structure-type obj) port machine-readable? shared-structure-map)
    (dolist (slot (structure-slots obj))
      (write-strings port " ")
      (print slot port machine-readable? shared-structure-map)
      (write-strings port " ")
      (print (structure-slot-by-name obj slot) port machine-readable? shared-structure-map))
    (write-strings port ")")))

(define-method (print-object (obj nil) port machine-readable? shared-structure-map)
  (write-strings port "()"))

(define-method (print-object (obj boolean) port machine-readable? shared-structure-map)
  (if obj
      (write-strings port "#t")
      (write-strings port "#f")))

(define-method (print-object (obj fixnum) port machine-readable? shared-structure-map)
  (write-strings port (number->string obj 10 #t)))

(define-method (print-object (obj flonum) port machine-readable? shared-structure-map)
  (cond ((not (infinite? obj))
         (write-strings port (number->string obj 10 #t *flonum-print-precision*)))
        ((> obj 0.0)
         (write-strings port "#iposinf"))
        ((< obj 0.0)
         (write-strings port "#ineginf"))
        (#t
         (write-strings port "#inan"))))

(define-method (print-object (obj complex) port machine-readable? shared-structure-map)
  (let ((re (real-part obj))
        (im (imag-part obj)))
    (print re port machine-readable? #f)
    (unless (< im 0.0)
      (write-strings port "+"))
    (print im port machine-readable? #f)
    (write-strings port "i")))

(define-method (print-object (obj package) port machine-readable? shared-structure-map)
  (print-unreadable-object obj port
    (write-strings port " ")
    (write-strings port (package-name obj))))

(define-method (print-object (obj values-tuple) port machine-readable? shared-structure-map)
  (print-unreadable-object obj port
    (print (values-bind obj vals vals) port machine-readable? #f)))

(define-method (print-object (obj string) port machine-readable? shared-structure-map)
  (define (print-machine-string string port)
    (write-strings port "\"")
    (let ((is (open-input-string string)))
      (set-port-translate-mode! is #f)
      (let loop ()
        (let ((ch (read-char is)))
          (unless (eof-object? ch)
            (cond ((eq? ch #\\) (write-strings port "\\\\"))
                  ((eq? ch #\") (write-strings port "\\\""))
                  ((eq? ch #\newline) (write-strings port "\\n"))
                  ((eq? ch #\cr) (write-strings port "\\r"))
                  ((eq? ch #\tab) (write-strings port "\\t"))
                  ((eq? ch #\nul) (write-strings port "\\000"))
                  ((or (< (char->integer ch) 32)
                       (>= (char->integer ch) 128))
                   (write-strings port "\\" (string-take-right
                                             (string-append "000"
                                                            (number->string (char->integer ch) 8))
                                             3)))
                  (#t
                   (write-char ch port)))
            (loop)))))
    (write-strings port "\""))
  (if machine-readable?
      (print-machine-string obj port)
      (write-strings port obj)))

(define-method (print-object (obj free-cell) port machine-readable? shared-structure-map)
  (unreadable (write-strings port "#<FREE-CELL@#x" (%obaddr-string obj) ">")))

(define-method (print-object (obj unbound-marker) port machine-readable? shared-structure-map)
  (unreadable (write-strings port "#<UNBOUND-MARKER>")))

(define-method (print-object (obj fast-op) port machine-readable? shared-structure-map)
  (values-bind (parse-fast-op obj) (op-name args)
     (print-unreadable-object obj port
       (print op-name port machine-readable? shared-structure-map)
       (dolist (arg args)
         (write-strings port " ")
         (print arg port machine-readable? shared-structure-map)))))

(define-method  (print-object (obj port) port machine-readable? shared-structure-map)
  (print-unreadable-object obj port
    (write-strings port " name:")
    (print (port-name obj) port machine-readable? #f)
    (write-strings port " ")
    (when (binary-port? obj)
      (write-strings port "binary "))
    (print (port-mode obj) port machine-readable? #f)))

(define-method (print-object (obj external) port machine-readable? shared-structure-map)
  (print-unreadable-object obj port
   (write-strings port ":#x" (%obaddr-string (external-data obj)))
   (awhen (string? (external-type-name obj))
     (write-strings port " type-name:" it))
   (unless (print-external-details obj port)
     (unless (null? (external-desc obj))
       (write-strings port " ")
       (print (external-desc obj) port machine-readable? #f)))))

(define (print-hash-elements obj port machine-readable? shared-structure-map)
  (catch 'abort-hash-element-print
    (let ((count 0))
      (dohash (k v obj)
        (write-strings port " ")
        (when (at-length-check-limit? (incr! count))
          (unreadable (format port " #<... n=~a>" (- (length obj) count)))
          (throw 'abort-hash-element-print))
        (print k port machine-readable? shared-structure-map)
        (write-strings port " ")
        (print v port machine-readable? shared-structure-map)))))

(define-method (print-object (obj hash) port machine-readable? shared-structure-map)
  (with-new-print-level port
    (write-strings port "#h(")
    (print (hash-type obj) port #t shared-structure-map)
    (print-hash-elements obj port machine-readable? shared-structure-map)
    (write-strings port ")")))

(define-method (print-object (obj instance) port machine-readable? shared-structure-map)
  (with-new-print-level port
    (print-unreadable-object obj port
       (write-strings port "(")
       (print (%instance-proto obj) port #t shared-structure-map)
       (write-strings port ")")
       (when (instance-understands? obj 'print-unreadably)
         [obj print-unreadably port]))))


;;;; formatter

(define *default-tab-stop-interval* 8)

(define (formatter/string string next)
  (lambda (remaining-args all-args port)
    (display string port)
    (next remaining-args all-args port)))

(define (formatter/tilde next)
  "Creates a formatter that displays a literal tildeand calls the
   <next> formatter."
  (lambda (remaining-args all-args port)
    (write-char #\~ port)
    (next remaining-args all-args port)))

(define (formatter/display next)
  "Creates a formatter that displays its argument and calls the
   <next> formatter."
  (lambda (remaining-args all-args port)
    (display (car remaining-args) port)
    (next (cdr remaining-args) all-args port)))

(define (formatter/write next)
  "Creates a formatter that writes its argument and calls the
   <next> formatter."
  (lambda (remaining-args all-args port)
    (write (car remaining-args) port)
    (next (cdr remaining-args) all-args port)))

(define (formatter/newline next)
  "Creates a formatter that writes a newline and calls the
   <next> formatter."
  (lambda (remaining-args all-args port)
    (newline port)
    (next remaining-args all-args port)))

(define (formatter/fresh-line next)
  "Creates a formatter that calls fresh-line on the port and
   calls the <next> formatter."
  (lambda (remaining-args all-args port)
    (fresh-line port)
    (next remaining-args all-args port)))

(define (formatter/obaddr next)
  "Creates a formatter that displays the obaddr of its argument
   and calls the  <next> formatter."
  (lambda (remaining-args all-args port)
    (display "#x" port)
    (display (%obaddr-string (car remaining-args)) port)
    (next (cdr remaining-args) all-args port)))

(define (formatter/list-subformat next)
  "Creates a formatter that processes a sub-format string with arguments
   from a list and calls the <next> formatter. Arguments to the sub-format
   string are consumed from the argument immediately following the sub-format
   string, which must be a list. The sub-format string can also be a reference
   to global dictionary text."
  (lambda (remaining-args all-args port)
    (let ((subformat-string (car remaining-args))
          (subformat-remaining-args (cadr remaining-args)))
      (unless (or (string? subformat-string)
                  (symbol? subformat-string))
              (error "Subformat strings must be strings or text id symbols: ~s" subformat-string))
      (unless (list? subformat-remaining-args)
              (error "Subformat argument lists must be lists: ~s" subformat-remaining-args))
      ((formatter subformat-string) subformat-remaining-args subformat-remaining-args port)
      (next (cddr remaining-args) all-args port))))

(define (formatter/inline-subformat next)
  "Creates a formatter that processes a sub-format string with inline
   arguments and calls the <next> formatter. Arguments to the sub-format
   string are consumed from the argument list of the parent formatter.
   The sub-format string can also be a reference to global dictionary
   text."
  (lambda (remaining-args all-args port)
    (let ((subformat-string (car remaining-args)))
      (unless (string? subformat-string)
              (error "Subformat strings must be strings: ~s" subformat-string))
      ((formatter (car remaining-args)
                  (lambda (remaining-args all-args port)
                    (next remaining-args all-args port)))
       (cdr remaining-args) all-args port))))

(define (formatter/periodic-tab-stop tab-stop-interval next)
  "Creates a formatter that indents to the next column for
   integer n in (* n <tab-stop-interval>), then calls the <next>
   formatter."
  (unless (or (not tab-stop-interval) (> tab-stop-interval 0))
          (error "Relative tab stops must be non-negative: ~s" tab-stop-interval))
  (lambda (remaining-args all-args port)
    (let ((tab-stop-interval (if tab-stop-interval
                                 tab-stop-interval
                                 *default-tab-stop-interval*)))
      (indent (* tab-stop-interval
                 (+ 1 (quotient (cdr (port-location port)) tab-stop-interval)))
              #\space port)
      (next remaining-args all-args port))))

(define (formatter/absolute-tab-stop tab-col next)
  "Creates a formatter that indents to the <tab-col> column
   using indent, then calling the <next> formatter.. Like indent,
   if the output port is already beyond <tab-col>, the formatter will
   signal  'beyond-requested-column with the current column position,
   and makes no changes to the output port."
  (unless (> tab-col 0)
          (error "Absolute tab stops must be non-negative: ~s" tab-col))
  (lambda (remaining-args all-args port)
    (indent tab-col #\space port)
    (next remaining-args all-args port)))

(define (formatter/absolute-goto index next)
  "Create a formatter that resets the argument pointer to the
   <index>'th argument and calls the <next> formatter."
  (unless (> index 0)
          (error "Absolute references must be non-negative: ~s" index))
  (lambda (remaining-args all-args port)
    (next (nth-cdr all-args index) all-args port)))

(define (formatter/null :optional (next #f))
  "Create a formatter that does nothing but call the <next> formatter.
   It is not an error to omit the <next> formatter."
  (lambda (remaining-args all-args port)
    (when next  (next remaining-args all-args port))
    port))

(define (formatter format-spec :optional (next (formatter/null)))
  "Parse the format specifier <format-spec>, returning a (possibly)
   composite formatter, a closure taking three arguments:
   (<remaining-args> <all-args> <port>). The returned formatter will
   execute the specified format upon <port>, using the provided argument
   lists. <remaining-args> is a reference to the next argument to be
   processed by the formatter, <all-args> is a reference to all arguments
   processed by the formatter. Normally these should be the same: argument
   references are relative to <all-args>.  <format-spec> can also  be a
   reference to global dictionary text. Invalid format strings throw
   errors."
  (let ((format-string (->text format-spec)))
    (let ((ip (open-input-string format-string)))
      ;; This design parses command arguments with simple recursive descent:
      ;; format codes taking arguments are handled by a recursive call to
      ;; a function that reads the argument and processes the code.  Another
      ;; approach, possibly simpler as format codes and their arguments grow
      ;; more complex is to have a call that parses a format code argument list
      ;; that is _always_ called, and a single case that dispatches based on
      ;; whatever format code follows the arg list.
      (define (accept-paramaterized-format-code)
        (let ((arg (read-exact-number ip)))
          (cond ((not (exact? arg))
                 (error "Non-exact format-code argument \"~s\" in format string: ~s" arg format-string))
                ((port-at-end? ip)
                 (error "Incomplete paramaterized format code in format string: ~s" format-string))
                (#t
                 (case (read-char ip)
                   ((#\t) (formatter/periodic-tab-stop arg (next-segment)))
                   ((#\T) (formatter/absolute-tab-stop arg (next-segment)))
                   ((#\@)
                    (unless (eq? (read-char ip) #\*)
                            (error "Invalid format reference syntax in format string: ~s" format string))
                    (formatter/absolute-goto arg (next-segment)))
                   (#t (error "Unknown paramaterized format code in format string: ~s" format-string)))))))
      (define (accept-format-code)
        (cond ((port-at-end? ip)
               (error "Incomplete format code in format string: ~s" format-string))
              ((char-numeric? (peek-char ip))
               (accept-paramaterized-format-code))
              (#t
               (let ((format-code (read-char ip)))
                 (case format-code
                   ((#\~) (formatter/tilde (next-segment)))
                   ((#\s) (formatter/write (next-segment)))
                   ((#\a) (formatter/display (next-segment)))
                   ((#\%) (formatter/newline (next-segment)))
                   ((#\&) (formatter/fresh-line (next-segment)))
                   ((#\@) (formatter/obaddr (next-segment)))
                   ((#\t) (formatter/periodic-tab-stop #f (next-segment)))
                   ((#\I) (formatter/list-subformat (next-segment)))
                   ((#\S) (formatter/inline-subformat (next-segment)))
                   (#t (error "Unknown format code ~s in format string: ~s" format-code format-string)))))))
      (define (next-segment)
        (cond ((port-at-end? ip)
               (formatter/null next))
              ((eq? (peek-char ip) #\~)
               (read-char ip) ; eat the initial #\~
               (accept-format-code))
              (#t
               (formatter/string (read-text-until-character ip #\~)
                                 (next-segment)))))
      (set-port-translate-mode! ip #f)
      (next-segment))))

(define (format port format-string . args)
  (define (do-format port)
    ((formatter format-string) args args port))
  (cond ((output-port? port) (do-format port))
        ((eq? port #t) (do-format (current-output-port)))
        ((eq? port #f) (get-output-string (do-format (open-output-string))))
        (#t (error "Invalid port specifier: ~s" port))))


