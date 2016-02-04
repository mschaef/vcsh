
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


(define (dformat format-str . args)
  "Display a formatted message to the current debug port."
  (apply format
         (current-debug-port)
         format-str
         args))

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
  "Times the execution of a call to the paramaterless function <fn>,
   printing out a message to the debug output port with timing information."
  (let ((result (%time-apply0 fn)))
    (dynamic-let ((*print-addresses* #f)
                  (*flonum-print-precision* *time-flonum-print-precision*))
      (dformat "~&; time = ~a ms (~a gc), ~a cons work, ~a fops, ~a frames\n"
              (* 1000.0 (vector-ref result 1))
              (* 1000.0 (vector-ref result 2))
              (vector-ref result 3)
              (vector-ref result 4)
              (vector-ref result 5)))
    (vector-ref result 0)))

(defmacro (time . code)
  "Times the execution of <code>, printing out a message to the debug
   output port with timing information."
  `(call-with-time-output (lambda () ,@code)))

;;;; Documentation access and symbol search

(define (documentation obj) ;; REVISIT: Should be a generic function
  "Return the documentation string associated with the object <obj>.
   Returns #f for objects with no documentation string (including
   objects that cannot have documentation strings)."
  (cond ((traced-procedure? obj)
         (aif (documentation (traced-procedure? obj))
              (dformat "~a THIS PROCEDURE IS CURRENTLY TRACED AND DOES NOT ELIMIMINATE TAIL CALLS." it)
              #f))
        ((procedure? obj)
         (aif (assoc 'documentation (%property-list obj))
              (if it (cdr it) #f)
              #f))
        ((macro? obj)
         (aif (documentation (%macro-transformer obj))
              it
              #f))
        (#t #f)))

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
    (runtime-check symbol? symbol)
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

(define-repl-abbreviation :a 'apropos)
(define-repl-abbreviation :A 'apropos-any-package)

;;;; Environment watch mechanisms

(define (dump-frame frame frame-no)
  "Display the specified environment <frame> to standard
   output. It is displayed with the frame number <frame-no>."
  (doiterate ((count var-no 0)
              (list var (car frame))
              (list val (cdr frame)))
    (dformat "; F[~a,~a]: ~s = ~s\n" frame-no var-no var val)))

(define (dump-environment env)
  "Display the specified environment <env> to standard output."
  (dformat "; Environment:\n")
  (doiterate ((count frame-no 0)
              (list frame env))
    (dump-frame frame frame-no)))

(defmacro (watch-locals)
  "Establish a watch on the topmost environment frame. The frame
   will be displayed to standard output at each execution of the
   watch."
  `(dump-frame (car (the-environment)) :locals))

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
              `(dformat "; WATCH: ~s = ~s\n" ',expr ,expr))
            exprs)))

;;;; The fast-op disassembler

(define (disassemble . functions)
  (define (print-closure-code code)

    (define (emit format-str . args)
      (trace-indent)
      (dformat  "~I\n" format-str args))

    (let recur ((code code))
      (mvbind (opcode opname actuals next-op) (compiler::parse-fast-op code)
        (cond ((not opname)
               (emit "<INVALID-OPCODE: ~s>" opcode))
              ((eq? :closure opname)
               (emit "~s ~s {" opname (car actuals))
               (in-trace-level
                (recur (cadr actuals)))
               (emit "}"))
              ((memq opname '(:literal :local-ref :local-set! :global-ref :global-set!))
               (emit "~s ~s" opname (car actuals)))
              
              (#t
               (let ((formals (compiler::fop-name->formals opname)))
                 (if (null? formals)
                     (emit "~s" opname)
                     (emit "~s (" opname))
                 (in-trace-level
                  (doiterate ((list formal formals)
                              (list actual actuals))
                    (case formal
                      ((:fast-ops)
                       (dolist (op actual)
                         (recur op)))
                      ((:fast-op)
                       (recur actual))
                      (#t
                       (emit " ~s" actual)))))
                 (unless (null? formals)
                   (emit ")"))))))
      (unless (null? next-op)
        (emit "=>")
        (recur next-op))))
  
  (define (print-closure-disassembly closure)
    (dformat ";;;; Disassembly of ~s\n" closure)
    (dformat ";; args: ~s\n" (car (%closure-code closure)))
    (dformat ";; lenv: ~s\n" (%closure-env closure))
    (dolist (property-binding (%property-list closure))
      (dbind (prop-name . prop-value) property-binding
        (dformat ";; prop ~s: ~s\n" prop-name prop-value)))
    (print-closure-code (cdr (%closure-code closure))))
  (dynamic-let ((*print-readably* #f))
    (dolist (f functions)
      (let ((f (if (symbol? f) (symbol-value f) f)))
        (cond ((generic-function? f)
               (dformat  "generic function disassembly:\n\n")
               (dolist (method (generic-function-methods f))
                 (dformat "\nmethod disassemble ~s:\n" (car method))
                 (print-closure-disassembly (cdr method))
                 (dformat "\n")))
              ((closure? f)
               (print-closure-disassembly f))
              (#t
               (error "Cannot disassemble: ~s" f)))))))

;;;; Printer support for fast-ops

(define-method (print-object (obj fast-op) port machine-readable? shared-structure-map)
  (mvbind (op-code op-name args) (compiler::parse-fast-op obj)
     (scheme::print-unreadable-object obj port
       (print op-name port machine-readable? shared-structure-map)
       (dolist (arg args)
         (write-strings port " ")
         (print arg port machine-readable? shared-structure-map)))))

;;;; The global define hook

(define *global-define-hook* ())

(define (trap-global-define-handler trapno frp symbol new-definition)
  (invoke-hook '*global-define-hook* symbol new-definition))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%set-trap-handler! system::TRAP_DEFINE trap-global-define-handler))

;;;; The function tracer

(define *trace-level* 0)
(define *spaces-per-trace-level* 2)

(defmacro (in-trace-level . code)
  `(dynamic-let ((*trace-level* (+ 1 *trace-level*)))
     ,@code))

(define (trace-indent :optional (port (current-debug-port)))
  (fresh-line port)
  (indent (* *spaces-per-trace-level* *trace-level*) #\space port))

(define (indent column
                :optional
                (indent-character #\space)
                (port (current-output-port)))
  "Indents text port <port> so that the next character printed will
   appear in column <column>. If the port is already beyond <column>,
   signals 'beyond-requested-column with the current column position,
   and makes no changes to the output port. <indent-character> allows
   characters other than #\\space to be used to indent."
  (runtime-check char? indent-character)
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
                 (trace-indent)
                 (dformat " > TRACE ~s" (cons fn-name args))
                 (let ((normal-return #f))
                   (unwind-protect
                    (lambda ()
                      (let ((rc (apply fn args)))
                        (when write-returns?
                          (trace-indent)
                          (dformat " < TRACE-RETURNS=~s" rc))
                        (set! normal-return #t)
                        rc))
                    (lambda ()
                      (unless normal-return
                        (trace-indent)
                        (dformat " <<< TRACE-ESCAPING")))))))))
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

(define-repl-abbreviation :t trace)
(define-repl-abbreviation :tnr trace/no-returns)
(define-repl-abbreviation :ut untrace)
(define-repl-abbreviation :uta untrace-all)

(define (display-packages :optional (display-symbols? #f))

  (define (sort-package-list packages)
    (qsort packages string< package-name))

  (define (display-symbols package)
    (call-with-package *package*
                       #L0(dohash (symbol-text symbol-binding  (%package-bindings package))
                            (dformat "   ; ~s -> ~s\n" symbol-text symbol-binding))))

  (dformat "\n; *package* = ~s\n" *package*)
  (dolist (package (sort-package-list (list-all-packages)))
    (dformat "; ~s -> ~s\n" package (%package-use-list package))
    (when display-symbols?
      (display-symbols package)))
  (dformat "\n")
  (values))

(define-repl-abbreviation :dp display-packages)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-char-syntax! *readsharp-syntax* #\L 'read-short-lambda))

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
  (set-char-syntax! *readsharp-syntax* #\" 'read-string-quasiquote))

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
                                      (dformat "; Watch Print Error: ~I" message args)
                                      (throw 'abort-watch-print))))
        (dformat "; ~s " watch)
        (mvbind values (catch-all (eval watch))
          (case (length values)
            ((0) (dformat "\n"))
            ((1) (dformat "=> ~s\n" (car values)))
            (#t
             (dformat "\n")
             (dolist (value values)
               (dformat ";    => ~s\n" value)))))))))

(add-hook-function! '*repl-post-hook* 'show-repl-auto-watches)

(define-repl-abbreviation :raw 'repl-auto-watch)
(define-repl-abbreviation :rau repl-auto-unwatch)
(define-repl-abbreviation :raua repl-auto-unwatch-all)

;; Other useful REPL abbreviations

(define-repl-abbreviation :r 'require-package!)
(define-repl-abbreviation :l 'load)
(define-repl-abbreviation :ip 'in-package!)
(define-repl-abbreviation :use 'use-package!)
(define-repl-abbreviation :dis disassemble)
(define-repl-abbreviation :mx 'macroexpand)
(define-repl-abbreviation :mx1 'macroexpand-1)

;; A utility mechanism that allows a single form to be evaluated
;; in the specified package.

(define *package-restore-proc* #f)

(define (restore-package)
  (when *package-restore-proc*
    (*package-restore-proc*)))

(add-hook-function! '*repl-post-hook* 'restore-package)

(define-repl-abbreviation :ip1 'in-package-for-one-form!)

(define (in-package-for-one-form! package)
  (let ((old-package *package*))
    (set! *package-restore-proc*
          (lambda ()
            (set! *package-restore-proc*
                  (lambda ()
                    (in-package! old-package)
                    (set! *package-restore-proc* #f)))))
    (in-package! package)))

;;;; Memory utilities

(define (type-stats-delta x y)
  (define (lookup-x type)
    (aif (assoc type x) (cdr it) 0))
  (map #L(cons (car _) ( - (lookup-x (car _)) (cdr _))) y))

(define (write-type-stats-table ts)
  (define (type-name type)
    (format #f "~s" type))
  (let ((max-type-length (apply max (map #L(length (type-name (car _))) ts))))
    (dolist (entry (qsort ts > cdr))
      (dformat "; ~a = ~a cells\n" (pad-to-width (car entry) max-type-length) (cdr entry)))))

(define-repl-abbreviation :sts show-type-stats)

(define (annotate-type-stats ts)
  (let ((rv ()))
    (dotimes (ii (length ts))
      (awhen (typecode->name ii)
        (push! (cons it (vector-ref ts ii)) rv)))
    rv))

(define (show-type-stats)
  (write-type-stats-table (annotate-type-stats (scheme::%heap-cell-count-by-typecode)))
  (values))

(defmacro (show-type-delta expr)
  (with-gensyms (initial-ts-sym)
    `(let ((,initial-ts-sym (%heap-cell-count-by-typecode)))
       (begin-1
        ,expr
        (let ((final-ts (%heap-cell-count-by-typecode)))
          (dformat "; Type stats delta (note that this reflects GC's)\n;\n")
          (write-type-stats-table
           (type-stats-delta (annotate-type-stats final-ts)
                             (annotate-type-stats ,initial-ts-sym))))))))

(define-repl-abbreviation :std show-type-delta)

