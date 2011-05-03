
;;;; tools.scm --
;;;;
;;;; A library of debugging related tools that gets included in the
;;;; Scheme core.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (frame->a-list frame)
  "Given a local environment frame <frame>, return the frame's bindings
   as an a-list."
  (let recur ((vars (car frame))
              (bindings (cdr frame)))
    (cond ((null? vars)
           ())
          ((symbol? vars)
           (cons (cons vars bindings)))
          ((and (pair? vars) (pair? bindings))
           (cons (cons (car vars) (car bindings))
                 (recur (cdr vars) (cdr bindings))))
          (#t
           (error "Invalid frame: ~s" frame)))))

(define (closure-env->a-list closure-env)
  "Given a local environment <closure-env>, return the bindings
   as an a-list.  All bindings are returned in the list, even those
   shadowed by other bindings of the same name."
  (fold (lambda (frame bindings)
          (append bindings (frame->a-list frame)))
        ()
        closure-env))

(define (closure-bindings closure)
  "Given a closure <closure>, return an a-list with the bindings
   visible within that closure.  The a-list returned is minimal:
   shadowed locals are not returned."
  (minimal-alist (closure-env->a-list (%closure-env closure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Performance Analysis Utilities

(define *time-flonum-print-precision* 5)

(define (call-with-time-output fn)
  "Times the execution of a call to the paramaterless function <fn>, printing out a
   message to stdout with timing information."
  (let ((result (%time-apply0 fn)))
    (dynamic-let ((*print-addresses* #f)
                  (*flonum-print-precision* *time-flonum-print-precision*))
      (format #t  "~&; time = ~a ms (~a gc), ~a cons work\n"
              (* 1000.0 (vector-ref result 1))
              (* 1000.0 (vector-ref result 2))
              (vector-ref result 3)))
    (vector-ref result 0)))

(defmacro (time . code)
  "Times the execution of <code>, printing out a message to stdout with timing information."
  `(call-with-time-output (lambda () ,@code)))

(define (call-with-port-bandwidth port fn)
  (let* ((starting-counts (port-io-counts port))
         (result (%time-apply0 fn))
         (ending-counts (port-io-counts port)))
    (newline)
    (format #t "; read bandwidth = ~a\n" (/ (- (car ending-counts)
                                               (car starting-counts))
                                            (vector-ref result 1)))

    (format #t "; write bandwidth = ~a\n" (/ (- (cdr ending-counts)
                                                (cdr starting-counts))
                                             (vector-ref result 1)))
    (vector-ref result 0)))

(defmacro (port-bandwidth port . code)
  `(call-with-port-bandwidth ,port `(lambda () ,@code)))

;;; Documentation access


(define (documentation obj) ;; REVISIT: Should be a generic function
  "Return the documentation string associated with the object <obj>.
   Returns #f for objects with no documentation string (including
   objects that cannot have documentation strings)."
  (cond ((traced-procedure? obj)
         (aif (documentation (traced-procedure? obj))
              (format #f "~a THIS PROCEDURE IS CURRENTLY TRACED AND DOES NOT ELIMIMINATE TAIL CALLS." it)
              #f))
        ((procedure? obj)
         (aif (assq 'documentation (%property-list obj))
              (if it (cdr it) #f)
              #f))
        ((macro? obj)
         (aif (documentation (%macro-transformer obj))
              it
              #f))
        (#t #f)))


(define (dump-frame frame frame-no)
  "Display the specified environment <frame> to standard
   output. It is displayed with the frame number <frame-no>."
  (map (lambda (var value)
         (format (current-debug-port) "F[~a]: ~s = ~s\n" frame-no var value))
       (car frame) (cdr frame)))

(define (dump-environment env)
  "Display the specified environment <env> to standard output."
  (define (loop rest frame-no)
    (unless (null? rest)
      (dump-frame (car rest) frame-no)
      (loop (cdr rest) (- frame-no 1))))
  (display "Environment---\n" (current-debug-port))
  (loop env 0)
  (display "--------------\n" (current-debug-port)))

(defmacro (watch-locals)
  "Establish a watch on the topmost environment frame. The frame
   will be displayed to standard output at each execution of the
   watch."
  `(dump-frame (car (the-environment))))

(defmacro (watch-environment)
  "Establish a watch on the entire environment. The environment
   will be displayed to standard output at each execution of the
   watch."
  `(dump-environment (the-environment)))

(defmacro (watch . exprs)
  "Establish a watch on each of the expressions in <exprs>. Each
   expression will be evaluated and printed at each execution
   of the watch."
  `(begin
     ,@(map (lambda (expr)
              `(format #t "WATCH: ~s = ~s\n" ',expr ,expr))
            exprs)))

(define *disassemble-show-fast-op-addresses* #f)

(define (disassemble . functions)
  (define (print-closure-code code port)
    (let recur ((code code))
      (trace-indent port)
      (when *disassemble-show-fast-op-addresses*
        (format port "~@ " code))
      (cond ((compiler::fast-op? code)
             (mvbind (opcode args) (compiler::parse-fast-op code)
               (case opcode
                 ((:global-ref :local-ref :literal)
                  (format port "~s ~s\n" opcode (car args)))
                 ((:global-set! :local-set!)
                  (format port "~s ~s\n" opcode (car args))
                  (in-trace-level
                   (recur (cadr args))))
                 ((:closure)
                  (format port "~s ~s {\n" opcode (car args))
                  (in-trace-level
                   (recur (cadr args)))
                  (trace-indent port)
                  (format port "}\n"))
                 ((:apply-global)
                  (format port "~s ~s\n" opcode (car args))
                  (in-trace-level
                   (dolist (arg (cadr args))
                     (recur arg))))
                 ((:apply)
                  (format port "~s\n" opcode)
                  (in-trace-level
                   (dolist (arg (cons (car args) (cadr args)))
                     (recur arg))))
                 (#t
                  (format port "~s\n" opcode)
                  (in-trace-level
                   (dolist (arg args)
                     (recur arg)))))))
            (#t
             (format port "~s\n" code)))))
  (define (print-closure-disassembly closure)
    (format  (current-debug-port) "; disassemble\n; ----------------\n")
    (format  (current-debug-port) "; l-list: ~s\n" (car (%closure-code closure)))
    (format  (current-debug-port) "; env   : ~s\n" (%closure-env closure))
    (format  (current-debug-port) "; p-list: ~s\n" (%property-list closure))
    (print-closure-code (cdr (%closure-code closure)) (current-debug-port)))
  (dynamic-let ((*print-readably* #f))
    (dolist (f functions)
      (let ((f (if (symbol? f) (symbol-value f) f)))
        (cond ((generic-function? f)
               (format  (current-debug-port) "generic function disassembly:\n\n")
               (dolist (method (generic-function-methods f))
                 (format  (current-debug-port) "\nmethod disassemble ~s:\n" (car method))
                 (print-closure-disassembly (cdr method))
                 (newline (current-debug-port))))
              ((closure? f)
               (print-closure-disassembly f))
              (#t
               (error "Cannot disassemble: ~s" f)))))))

;;; Printer support for fast-ops

(define-method (print-object (obj fast-op) port machine-readable? shared-structure-map)
  (mvbind (op-name args) (compiler::parse-fast-op obj)
     (scheme::print-unreadable-object obj port
       (print op-name port machine-readable? shared-structure-map)
       (dolist (arg args)
         (write-strings port " ")
         (print arg port machine-readable? shared-structure-map)))))

;;; The function tracer

(define *trace-level* 0)

(defmacro (in-trace-level . code)
  `(dynamic-let ((*trace-level* (+ 1 *trace-level*)))
     ,@code))

(define (trace-indent :optional (port (current-output-port)))
  (fresh-line port)
  (indent *trace-level* #\space port))

(define (indent column
                :optional
                (indent-character #\space)
                (port (current-output-port)))
  "Indents text port <port> so that the next character printed will
   appear in column <column>. If the port is already beyond <column>,
   signals 'beyond-requested-column with the current column position,
   and makes no changes to the output port. <indent-character> allows
   characters other than #\\space to be used to indent."
  (check char? indent-character)
  (let ((current-column (cdr (port-location port))))
    (cond ((< column 0)
           (error "Column index must be non-negative: ~s." column))
          ((<= current-column column)
           (repeat (- column current-column)
                   (display indent-character port)))
          (#t
           (signal 'beyond-requested-column current-column)))
    port))

(define (traced-procedure? fn)
  "Given a procedure <fn>, determine if it is currently traced. The return
   value, if true, is the procedure being traced."
  (and (closure? fn)
       (get-property fn '%traced-procedure)))

(define (traced-procedure fn :optional (write-returns? #t))
  "Return a traced version of the procedure <fn>.  This version of the procedure
   will display messages on entry and on exit. The transformation that makes this
   happen breaks proper tail recursion. If the optional argument <write-returns?>
   if #f, return values are printed as :not-displayed."
  (unless (closure? fn)
    (error "Traces can only be established on closures: ~a" fn))
  (if (traced-procedure? fn)
      fn
      (let* ((fn-name (aif (procedure-name fn) it fn))
             (traced-procedure
              (lambda args
                (in-trace-level
                 (trace-indent (current-debug-port))
                 (format (current-debug-port) " > TRACE ~s" (cons fn-name args))
                 (let ((normal-return #f))
                   (unwind-protect
                    (lambda ()
                      (let ((rc (apply fn args)))
                        (when write-returns?
                          (trace-indent (current-debug-port))
                          (format  (current-debug-port) " < TRACE-RETURNS=~s" rc))
                        (set! normal-return #t)
                        rc))
                    (lambda ()
                      (unless normal-return
                        (trace-indent (current-debug-port))
                        (format  (current-debug-port) " <<< TRACE-ESCAPING"))
                      )))))))
        (set-property! traced-procedure `%traced-procedure fn)
        traced-procedure)))


(define (untraced-procedure fn)
  (unless (closure? fn)
    (error "Traces can only be established on closures: ~a" fn))
  (aif (get-property fn '%traced-procedure) it fn))

(define *traced-procedure-list* ())

(define (establish-trace-on-function function-name :optional (write-returns? #t))
  (info "Establishing a new trace on: ~s" function-name)
  (set-symbol-value! function-name (traced-procedure (symbol-value function-name)
                                                     write-returns?)))

(define (trace-record-for function-name)
  (find (lambda (record) (eq? (car record) function-name)) *traced-procedure-list*))

(define (trace-global-function function-name  :optional (write-returns? #t))
  (unless (trace-record-for function-name)
    (establish-trace-on-function function-name write-returns?)
    (push! (cons function-name write-returns?) *traced-procedure-list*))
  (map car *traced-procedure-list*))

(define (untrace-global-function function-name)
  (when (trace-record-for function-name)
    (set-symbol-value! function-name (untraced-procedure (symbol-value function-name)))
    (set! *traced-procedure-list* (remove (lambda (record)
                                            (eq? (car record) function-name))
                                          *traced-procedure-list*)))
  (map car *traced-procedure-list*))


(define *global-define-hook* ())

(define (trap-global-define-handler trapno frp symbol new-definition)
  (invoke-hook '*global-define-hook* symbol new-definition))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_DEFINE trap-global-define-handler))

(defmacro (trace . function-names) ;; REVISIT: Can't watch anonymous functions
  `(begin ,@(map (lambda (function-name)
                   `(trace-global-function ',function-name))
                 function-names)))

(defmacro (trace/no-returns . function-names)
  `(begin ,@(map (lambda (function-name)
                   `(trace-global-function ',function-name #f))
                 function-names)))

(defmacro (untrace . function-names)
  `(begin ,@(map (lambda (function-name)
                   `(untrace-global-function ',function-name))
                 function-names)))

(define (retrace-if-necessary symbol new-definition)
  (awhen (trace-record-for symbol)
    (establish-trace-on-function (car it) (cdr it))))

(add-hook-function! '*global-define-hook* 'retrace-if-necessary)

(define (untrace-all)
  (for-each untrace-global-function (map car *traced-procedure-list*)))


(push! '(:t trace) *repl-abbreviations*)
(push! '(:tnr trace/no-returns) *repl-abbreviations*)
(push! '(:ut untrace) *repl-abbreviations*)
(push! '(:uta untrace-all) *repl-abbreviations*)

(define (display-packages :optional (display-symbols? #f))

  (define (sort-package-list packages)
    (qsort packages string< package-name))

  (define (display-symbols package)
    (call-with-package *package*
                       #L0(dohash (symbol-text symbol-binding (sort-package-list (%package-bindings package)))
                                  (format (current-debug-port) "   ; ~s -> ~s\n" symbol-text symbol-binding))))

  (format  (current-debug-port) "\n; *package* = ~s\n" *package*)
  (dolist (package (sort-package-list (list-all-packages)))
    (format  (current-debug-port) "; ~s -> ~s\n" package (%package-use-list package))
    (when display-symbols?
      (display-symbols package)))
  (newline (current-debug-port))
  (values))

(push! '(:dp display-packages) *repl-abbreviations*)

(define (stable xs :optional (port (current-output-port)))
  "Displays the list <xs> in tabular form and returns nil. Does
   not display a line number marker for each line."
  (dolist (x xs)
    (display (format #f " ~s" x) port)
    (newline port)))

(define (table xs :optional (indent-by #f) (port (current-output-port)))
  "Displays <xs> in tabular form and returns nil. <xs> can be either
   a vector, list, or hash table."
  (typecase xs
    ((vector)
     (table (vector->list xs) indent-by port))
    ((hash)
     (table (hash->a-list xs) indent-by port))
    (#t
     (let ((i 0))
       (dolist (x xs)
         ;; format to string is much faster than format to certain ports
         (when indent-by
           (indent indent-by " " port))
         (display (format #f "~a> ~s" i x) port)
         (newline port)
         (incr! i))
       ()))))


(define (thru-table xs :optional (label ""))
  "Displays the list <xs> in tabular form and returns <xs>. The table is
   labled with the text <label>."
  (format #t "---------- ~a\n" label)
  (table xs)
  xs)

(define *default-width* 72)

(define (apropos-description obj)
  "Describes the object <obj> by returning the associated documentation string."
  (define (description-lambda-list obj)
    (mvbind (lambda-list source-lambda-list) (procedure-lambda-list obj)
      (if source-lambda-list  source-lambda-list  lambda-list)))
  (define (macro-lambda-list obj)
    (let ((transformer (%macro-transformer obj)))
      (let ((macro-name (get-property transformer 'macro-name #f))
            (macro-formals (get-property transformer 'macro-formals #f)))
        (if macro-name
            (cons macro-name macro-formals)
            "<unknown>"))))
  (define (print-value? val)
    (member (type-of val) '(symbol character fixnum flonum nil boolean)))
  (aif (traced-procedure? obj)
       (format #f "Traced ~a" (apropos-description it))
       (cond
        ((primitive? obj)
         (format #f "Primitive function: ~s" (procedure-name obj)))
        ((procedure? obj)
         (let ((pkg (aif (procedure-name obj)
                         (symbol-package it)
                         *package*)))
           (dynamic-let ((*package* pkg))
             (format #f "~aFunction lambda: ~s" (if (primitive? obj) "Primitive " "")
                     (description-lambda-list obj)))))
        ((macro? obj)
         (format #f "Macro: ~a ~a" (macro-lambda-list obj)))
        ((print-value? obj)
         (format #f "~a (~a)" (type-of obj) obj))
        (#t
         (format #f "~a" (type-of obj))))))

(define (%apropos search-for :optional (search-package *package*))
  "Prints a list describing each symbol apropos of all of the search
   terms in <search-for>. The symbols are restricted to public
   symbols in <search-package>, unless <search-package> is #f, in
   which case all packages are searched."
  (define (symbol-search-text symbol)
    (check symbol? symbol)
    (if (not (symbol-bound? symbol))
        ""
        (string-append (symbol-name symbol)
                       (aif (documentation (symbol-value symbol)) it ""))))
  (dynamic-let ((*info* #f))
    (let ((accum '())
          (search-for (map ->string search-for)))
      (display "--------------------------------\n")
      (dolist (sym (qsort (filter (lambda (sym)
                                    (string-contains-all-of?
                                     search-for (symbol-search-text sym)))
                                  (if search-package
                                      (all-package-symbols search-package)
                                      (all-symbols)))
                          string<
                          symbol-name))
        (let ((val (symbol-value sym)))
          (format #t "** ~a - ~a\n"
                  sym (apropos-description val))
          (awhen (documentation val)
            (dolist (line (break-lines it *default-width*))
              (format #t "  ~a\n" line))
            (newline))))))
  (values))

(define (apropos . search-for)
  "Prints a list describing each symbol public to the current package that
   is apropos of all of the search terms in <search-for>."
  (%apropos search-for)
  (values))

(define (apropos-any-package . search-for)
  "Prints a list describing each symbol visible in any package that is
   apropos of all of the search terms in <search-for>."
  (%apropos search-for #f)
  (values))

(push! '(:a apropos  :quote) *repl-abbreviations*)
(push! '(:A apropos-any-package  :quote) *repl-abbreviations*)

(define (referred-symbols fn)
  "Returns a list of all the symbols referred to by the code of function
   <fn>.  If <fn> is a symbol itself, it is taken to be the name of a
   function to be analyzed. Non-functions have no code to refer to symbols,
   refer to no symbols, and so this function returns () when called on a
   non-function."
  (cond ((closure? fn)
         (filter symbol? (set-union (flatten (cdr (%closure-code fn))))))
        ((and (symbol? fn) (not (keyword? fn)) (symbol-bound? fn))
         (referred-symbols (symbol-value fn)))
        (#t
         ())))

(define (referred-symbol-grep grep-for)
  "Returns a list of all symbols bound to functions that refer to
   the symbol <grep-for> within their code."
  (filter (lambda (sym)
            (member grep-for (referred-symbols sym)))
          (all-symbols)))

(push! '(:rsg referred-symbol-grep :quote) *repl-abbreviations*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; short-lambda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro (shorter-lambda . form)
  "Defines a function with an implicit lambda list of (_). This
   is intended to be used with a readsharp handler that shortens
   the syntax."
  `(lambda (_) ,@form))

(defmacro (short-lambda args . form)
  "Defines a function with an implicit lambda list of
   (take '(_0 _1 _2 _3 _4 _5) args). This is intended to be
   used with a readsharp handler that shortens the syntax."
  (check (and exact? (>= 0) (< 6)) args)
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-char-syntax! *readsharp-syntax* #\L 'read-short-lambda)

  (set-char-syntax! *readsharp-syntax* #\" 'read-string-quasiquote))

(define (%get-free-cell)
  "Return a pointer to an unallocated free cell on the heap."
  (let* ((x (cons :soon-to-be :free-cell))
         (x-addr (+ 17 (%obaddr x)))) ; The +17 is to fool the conservative GC
    (set! x ())
    (gc)
    (%sysob (- x-addr 17))))


;;;; An auto watch facility that evaluates and prints watch expressions after
;;;; each interactive form.

(define *repl-auto-watches* ())

(define (repl-auto-watch . forms)
  (set! *repl-auto-watches* (append *repl-auto-watches* forms))
  (values))

(define (repl-auto-unwatch-all)
  (set! *repl-auto-watches* ())
  (values))

(define (repl-auto-unwatch)
  (catch 'none-chosen
    (if (null? *repl-auto-watches*)
        (info "no watches current")
        (let ((form (repl-choose *repl-auto-watches* "Form to unwatch")))
          (set! *repl-auto-watches*
                (delete form *repl-auto-watches* eq?)))))
  (values))

(define (show-repl-auto-watches)
  (dolist (watch *repl-auto-watches*)
    (catch 'abort-watch-print
      (handler-bind ((runtime-error (lambda (message args)
                                      (format (current-error-port) "; Watch Print Error: ~I" message args)
                                      (throw 'abort-watch-print))))
        (format (current-debug-port) "; ~s " watch)
        (mvbind values (catch-all (eval watch))
          (case (length values)
            ((0) (newline  (current-debug-port)))
            ((1) (format (current-debug-port) "=> ~s\n" (car values)))
            (#t
             (newline (current-debug-port))
             (dolist (value values)
               (format (current-debug-port) ";    => ~s\n" value)))))))))

(add-hook-function! '*repl-post-hook* 'show-repl-auto-watches)

(push! '(:raw repl-auto-watch  :quote) *repl-abbreviations*)
(push! '(:rau repl-auto-unwatch) *repl-abbreviations*)
(push! '(:raua repl-auto-unwatch-all) *repl-abbreviations*)


;; Other useful REPL abbreviations

(push! '(:r require-package!  :quote) *repl-abbreviations*)
(push! '(:l load  :quote) *repl-abbreviations*)
(push! '(:ip in-package!  :quote) *repl-abbreviations*)
(push! '(:use use-package!  :quote) *repl-abbreviations*)
(push! '(:dis disassemble) *repl-abbreviations*)
(push! '(:mx macroexpand  :quote) *repl-abbreviations*)
(push! '(:mx1 macroexpand-1  :quote) *repl-abbreviations*)

;; A utility mechanism that allows a single form to be evaluated
;; in the specified package.

(define *package-restore-proc* #f)

(define (restore-package)
  (when *package-restore-proc*
    (*package-restore-proc*)))

(add-hook-function! '*repl-post-hook* 'restore-package)

(push! '(:ip1 in-package-for-one-form!  :quote) *repl-abbreviations*)

(define (in-package-for-one-form! package)
  (let ((old-package *package*))
    (set! *package-restore-proc*
          (lambda ()
            (set! *package-restore-proc*
                  (lambda ()
                    (in-package! old-package)
                    (set! *package-restore-proc* #f)))))
    (in-package! package)))

;; Memory utilities

(define (type-stats-delta x y)
  (define (lookup-x type)
    (aif (assq type x) (cdr it) 0))
  (map #L(cons (car _) ( - (lookup-x (car _)) (cdr _))) y))

(define (write-type-stats-table ts :optional (op (current-output-port)))
  (define (type-name type)
    (format #f "~s" type))
  (let ((max-type-length (apply max (map #L(length (type-name (car _))) ts))))
    (dolist (entry (qsort ts > cdr))
      (format op "; ~a = ~a cells\n" (pad-to-width (car entry) max-type-length) (cdr entry)))))

(push! '(:sts show-type-stats) *repl-abbreviations*)

(define (annotate-type-stats ts)
  (let ((rv ()))
    (dotimes (ii (length ts))
      (push! (cons (typecode->name ii) (vector-ref ts ii)) rv))
    rv))

(define (show-type-stats)
  (write-type-stats-table (annotate-type-stats (scheme::%heap-cell-count-by-typecode))
                          (current-debug-port))
  (values))

(defmacro (show-type-delta expr)
  (with-gensyms (initial-ts-sym)
    `(let ((,initial-ts-sym (%heap-cell-count-by-typecode)))
       (begin-1
        ,expr
        (let ((final-ts (%heap-cell-count-by-typecode)))
          (format (current-debug-port) "; Type stats delta (note that this reflects GC's)\n;\n")
          (write-type-stats-table (type-stats-delta (annotate-type-stats final-ts)
                                                    (annotate-type-stats ,initial-ts-sym))
                                  (current-debug-port)))))))

(push! '(:std show-type-delta) *repl-abbreviations*)
