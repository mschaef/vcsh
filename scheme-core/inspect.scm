;;;; inspect.scm
;;;; December 8th, 2007
;;;; Mike Schaeffer
;;;
;;; An interactive object inspector.

(define-generic-function (inspect-analyze-object obj)
  "Splits <obj> into a list of component pieces.  Each piece is represented
   in the result list as a description paired with the object itself."
  (list (cons "atom" obj)))

(define-method (inspect-analyze-object (obj values-tuple))
  (let loop ((ii 0) (remaining (%values->list obj)) (desc ()))
    (if (null? remaining)
        (reverse desc)
        (loop (+ ii 1)
              (cdr remaining)
              (cons (cons "()" (car remaining))
                    desc)))))

(define-method (inspect-analyze-object (obj cons))
  (cond ((list? obj)
         (let loop ((ii 0) (remaining obj) (desc ()))
           (if (null? remaining)
               (reverse desc)
               (loop (+ ii 1)
                     (cdr remaining)
                     (cons (cons "()" (car remaining))
                           desc)))))
        (#t
         (list (cons "car" (car obj))
               (cons "cdr" (cdr obj))))))

(define-method (inspect-analyze-object (obj symbol))
  (list (cons "name" (symbol-name obj))
        (cons "home" (aif (symbol-package obj) it "<uninterned-symbol>"))
        (cons "p-list" (%property-list obj))
        (cons "value" (if (symbol-bound? obj)
                          (symbol-value obj)
                          :unbound))))

(define-method (inspect-analyze-object (obj closure))
  (list (cons "l-list" (car (%closure-code obj)))
        (cons "code" (cdr (%closure-code obj)))
        (cons "env" (%closure-env obj))
        (cons "p-list" (%property-list obj))))

(define-method (inspect-analyze-object (obj macro))
  (list (cons "transformer" (%macro-transformer obj))
        (cons "p-list" (%property-list obj))))

(define-method (inspect-analyze-object (obj package))
  (list (cons "name" (package-name obj))
        (cons "bindings" (%package-bindings obj))
        (cons "use-list" (%package-use-list obj))))

(define-method (inspect-analyze-object (obj fast-op))
  (values-bind (parse-fast-op obj)  (opcode args)
    (cons (cons "opcode" opcode)
          (map #L(cons "[]" _) args))))

(define-method (inspect-analyze-object (obj structure))
  (map (lambda (slot-name)
         (cons slot-name (structure-slot-by-name obj slot-name)))
       (structure-slots obj)))

(define-method (inspect-analyze-object (obj vector))
  (let loop ((ii 0)
             (elements ()))
    (cond ((>= ii (length obj))
           (reverse elements))
          (#t
           (loop (+ ii 1) (cons (cons "[]" (vector-ref obj ii))
                                elements))))))

(define-method (inspect-analyze-object (obj instance))
  (define (proto)
    (if (instance? obj)
        (instance-proto obj)
        :none))
  (cons* (cons "proto" (proto))
         (map #L(cons (format #f "~s" _) (slot-ref obj _))
              (direct-instance-slots obj))))


(define-method (inspect-analyze-object (obj hash))
  (cons* (cons "type" (hash-type obj))
         (let ((ii 0))
           (reverse
            (fold
             (lambda (element rest)
               (begin-1
                (cons (cons (format #f "v[~a]" ii) (cdr element))
                      (cons (cons (format #f "k[~a]" ii) (car element))
                            rest))
                (incr! ii)))
             ()
             (hash->a-list obj))))))


(define (inspect-print context)
  (let loop ((ii 0) (context context))
    (unless (null? context)
      (format #t "~a:~a> ~s\n" ii (caar context) (cdar context))
      (loop (+ ii 1) (cdr context)))))

(define (inspect-prompt)
  (format #t "\nINSPECT> "))

(define (inspect-eval input obj context)
  (cond ((list? input)
         (eval input)
         obj)
        ((not (number? input))
         (format #t "Invalid inspect command: ~s\n" input)
         (throw 'inspect-error-escape))
        ((or (< input 0) (>= input (length context)))
         (format #t "Invalid object index: ~a\n" input)
         (throw 'inspect-error-escape))
        (#t
         (cdr (list-ref context input)))))

(define (inspect-exit)
  "Exits the topmost inspector."
  (throw 'inspect-exit))

(define *inspect-abbreviations*  ())

(push! '(:x inspect-exit) *inspect-abbreviations*)
(push! '(:X exit) *inspect-abbreviations*)
(push! '(:top toplevel) *inspect-abbreviations*)
(push! '(:a apropos :quote) *inspect-abbreviations*)
(push! '(:A apropos-any :quote) *inspect-abbreviations*)

(define (inspect-print-object-summary obj)
  "Prints a brief summary of <obj> used at the beginning of each inspector
   prompt."
  (format #t "\n; Inspecting a ~a.\n" (type-of obj))
  (dynamic-let ((*print-length* 3)
                (*print-length* 3))
    (repl-print obj)))

(define (inspect-read)
  (inspect-prompt)
  (repl-read *inspect-abbreviations*))

(define (inspect-repl obj)
  (let retry ((context (inspect-analyze-object obj)))
    (inspect-print-object-summary obj)
    (inspect-print context)
    (catch 'inspect-error-escape
      (let ((input (inspect-read)))
        (inspect-repl (inspect-eval input obj context))
        (retry context)))))


(define (inspect obj)
  "Inspects <obj>. The inspector is a variant of a REPL that is used
   to inspect objects. It maintains a pointer to an object, prints that
   object's component pieces in a user-readable form, and then allows
   selection of the 'next' object to inspect."
  (catch 'inspect-exit
    (inspect-repl obj))
   (values))

(push! '(:i inspect) *repl-abbreviations*)
