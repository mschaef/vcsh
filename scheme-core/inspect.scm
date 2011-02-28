5;;;; inspect.scm
;;;; December 8th, 2007
;;;; Mike Schaeffer
;;;
;;; An interactive object inspector.

(in-package! "scheme")

;;;; Inspector object analysis

(define *no-key* (gensym "no-key"))

(define-structure inspect-ctx
  obj
  (desc :default #f)
  (items :default ())
  (lenv :default #f))

(define-structure inspect-item
  (desc :default #f)
  (key :default *no-key*)
  (setter :default #f)
  value)

(define (inspect-item-has-key? elem)
  (not (eq? (inspect-item-key elem) *no-key*)))

(define-generic-function (object->inspect-ctx obj)
  "Splits <obj> into a list of component pieces.  Each piece is represented
   in the result list as a description paired with the object itself."
  (make-inspect-ctx
   :obj obj
   :items `(,(make-inspect-item :value obj))))

(define-method (object->inspect-ctx (obj inspect-item))
  (make-inspect-ctx
   :obj obj
   :desc "key/value pair from a hash table"
   :items `(,(make-inspect-item :desc "key" :value (inspect-item-key obj))
            ,(make-inspect-item :desc "value" :value (inspect-item-value obj)))))

(define-method (object->inspect-ctx (obj values-tuple))
  (make-inspect-ctx
   :obj obj
   :items (let loop ((ii 0) (remaining (%values->list obj)) (desc ()))
            (if (null? remaining)
                (reverse desc)
                (loop (+ ii 1)
                      (cdr remaining)
                      (cons (make-inspect-item :value (car remaining))
                            desc))))))

(define-method (object->inspect-ctx (obj cons))
  (if (list? obj)
      (make-inspect-ctx :obj obj
                        :desc (format #f "~s element list" (length obj))
                        :items (let loop ((ii 0) (remaining obj) (desc ()))
                                 (if (null? remaining)
                                     (reverse desc)
                                     (loop (+ ii 1)
                                           (cdr remaining)
                                           (cons (make-inspect-item :value (car remaining))
                                                 desc)))))
      (make-inspect-ctx :obj obj
                        :desc "cons cell"
                        :items  `(,(make-inspect-item :desc "car" :value (car obj))
                                  ,(make-inspect-item :desc "cdr" :value (cdr obj))))))

(define-method (object->inspect-ctx (obj symbol))
  (make-inspect-ctx
   :obj obj
   :items `(,(make-inspect-item :desc "name" :value (symbol-name obj))
            ,(make-inspect-item :desc "home" :value (aif (symbol-package obj)
                                                               it
                                                               "<uninterned-symbol>"))
            ,(make-inspect-item :desc "p-list" :value (%property-list obj))
            ,(make-inspect-item :desc "value"  :value (if (symbol-bound? obj)
                                                              (symbol-value obj)
                                                              :unbound)))))

(define-method (object->inspect-ctx (obj closure))
  (make-inspect-ctx
   :obj obj
   :lenv (%closure-env obj)
   :items `(,(make-inspect-item :desc "l-list" :value (car (%closure-code obj)))
            ,(make-inspect-item :desc "code"   :value (cdr (%closure-code obj)))
            ,(make-inspect-item :desc "env"    :value (%closure-env obj))
            ,(make-inspect-item :desc "p-list" :value (%property-list obj)))))

(define-method (object->inspect-ctx (obj macro))
  (make-inspect-ctx
   :obj obj
   :items `(,(make-inspect-item :desc "transformer" :value (%macro-transformer obj))
            ,(make-inspect-item :desc "p-list"      :value (%property-list obj)))))

(define-method (object->inspect-ctx (obj package))
  (make-inspect-ctx
   :obj obj
   :items `(,(make-inspect-item :desc "name"     :value (package-name obj))
            ,(make-inspect-item :desc "bindings" :value (%package-bindings obj))
            ,(make-inspect-item :desc "use-list" :value (%package-use-list obj)))))


(define-method (object->inspect-ctx (obj fast-op))
  (mvbind (opcode args) (compiler::parse-fast-op obj)
    (make-inspect-ctx
     :obj obj
     :items (cons (make-inspect-item :desc "opcode" :value opcode)
                  (map #L(make-inspect-item :value _) args)))))

(define-method (object->inspect-ctx (obj structure))
  (make-inspect-ctx
   :obj obj
   :desc (format #f "structure of type ~a" (type-of obj))
   :items (map #L(make-inspect-item
                  :key _
                  :value (structure-slot-by-name obj _))
               (structure-slots obj))))

(define-method (object->inspect-ctx (obj vector))
  (make-inspect-ctx
   :obj obj
   :desc (format #f "~a element vector" (length obj))
   :items (let loop ((ii 0)
                     (elements ()))
            (cond ((>= ii (length obj))
                   (reverse elements))
                  (#t
                   (loop (+ ii 1)
                         (cons (make-inspect-item :desc "elem"
                                                  :value (vector-ref obj ii))
                               elements)))))))

(define-method (object->inspect-ctx (obj instance))
  (define (proto)
    (if (instance? obj)
        (instance-proto obj)
        :none))
  (make-inspect-ctx
   :obj obj
   :items (cons* (make-inspect-item :desc "proto" :value (proto))
                 (map #L(make-inspect-item :desc "slot"
                                           :key _
                                           :value  (slot-ref obj _))
                      (direct-instance-slots obj)))))


(define-method (object->inspect-ctx (obj hash))
  (make-inspect-ctx
   :obj obj
   :desc (format #f "~a hash table" (hash-type obj))
   :items  (fold
            (lambda (element rest)
              (cons (make-inspect-item :desc "elem"
                                       :key (car element)
                                       :value (cdr element))
                    rest))
            ()
            (hash->a-list obj))))

;;;; The main inspector command loop

(define (inspect-prompt)
  (format #t "\nINSPECT:~a> " (package-name *package*)))

(define (inspect-item ctx elem-no)
  (let ((ctx-items (inspect-ctx-items ctx)))
    (cond ((not (number? elem-no))
           (format #t "Invalid inspect element index: ~s\n" elem-no)
           (throw 'inspect-error-escape))
          ((or (< elem-no 0) (>= elem-no (length ctx-items)))
           (format #t "Inspector element index out of range: ~a\n" elem-no)
           (throw 'inspect-error-escape))
          (#t
           (list-ref ctx-items elem-no)))))

;; REVISIT: Add lenv support to the inspector.
;;
;; The idea behind this feature is that the inspector should make it possible
;; to evaluate expressions in the local environment of a closure being inspected.
;; There are a couple possible implementation strategies for this, both with issues.
;;
;; 1) Fix inspect-eval to pass the lenv into repl-eval, assuming the context has a
;;    lenv. This evaluates all inspector forms in the lenv, including inspector
;;    commands. This might be confusing to the user, and if the lenv happens to contain
;;    a binding for an inspector command, it allows the inspector to be disabled.
;;    Probably a bad idea to do this, unless some kind of safeguard can be taken. It
;;    _might_ be simple enough to stick the inspector into its own package and
;;    ensure that all the inspector commands are bound to symbols local to that package.
;;   This still has the underlying problem, but it's simple to implement, and you'd
;;   have to work pretty hard to screw yourself up.
;;
;; 2) Add an :eval command to the inspector that allows forms to be explicitly
;;    evaluated in the closure's lenv. This fixes the user confusion issue, at the
;;    expense of requiring the user to type in :eval for every evaluation. The downside
;;    to this is that is that the :eval command itself would be a repl abbreviation
;;    that would have to have some way to refer to the lenv of the current inspector
;;    context. The repl abbreviation mechanism is not set up to do this... the only
;;    thing I can think of is that the inspector creates a lenv that contains
;;    bindings that refer to the current inspector context. (This could possibly
;;    be inspect-eval's lenv, for that matter).  The repl abbreviation could then
;;    expand to a form that contains references to bindings in that lenv. This
;;    gives the repl abbreviation access to the local context and therefore any
;;    lenv defined in that context. (This lenv would introduce name conflict issues
;;    similar to those mentioned in option 1).
;;
;; 3) Implement option 2, but forget trying to reuse the repl abbreviation mechanism.
;;    This is simple, but we lose the help infrastructure, etc. that we get by
;;    default as a part of that mechanism.

(define (inspect-eval input obj ctx)
  (if (number? input)
      (let ((elem (inspect-item ctx input)))
        (if (inspect-item-has-key? elem)
            elem
            (inspect-item-value elem)))
        (apply repl-print (repl-eval input))))

(define (inspect-exit)
  "Exits the topmost inspector."
  (throw 'inspect-exit))

(define *inspect-abbreviations*  ())

(push! '(:x inspect-exit) *inspect-abbreviations*)
(push! '(:X exit) *inspect-abbreviations*)
(push! '(:top toplevel) *inspect-abbreviations*)
(push! '(:a apropos :quote) *inspect-abbreviations*)
(push! '(:A apropos-any :quote) *inspect-abbreviations*)

;; REVISIT: Add a :set command that allows the inspector to set items too... this will
;; need a way of getting the inspector context down into the inspect-set! function,
;; which faces the same problems as the would-be :eval command.

(define (inspect-read)
  (inspect-prompt)
  (repl-read *inspect-abbreviations*))

(defmacro (with-inspect-printing . code)
  `(dynamic-let ((*print-length* 3)
                (*print-length* 3))
    ,@code))

(define (inspect-print-object-summary ctx)
  "Prints a brief summary of <obj> used at the beginning of each inspector
   prompt."
  (let ((obj (inspect-ctx-obj ctx)))
    (format #t "\n; Inspecting a ~a.\n" (aif (inspect-ctx-desc ctx)
                                             it
                                             (type-of obj)))
    (with-inspect-printing
     (repl-print obj))))


(define (inspect-print ctx)
  (inspect-print-object-summary ctx)
  (with-inspect-printing
   ;; REVISIT: Add a paging capability, in case the list of context items is large.
   (doiterate ((:count ii 0)
               (:list obj-elem (inspect-ctx-items ctx)))
     (unless (null? ctx)
       (format #t "~s" ii)
       (awhen (inspect-item-desc obj-elem)
         (format #t "{~a}" it)) ;; REVISIT: May be possible to make this a bit more readable, by using fixed-width columns.
       (when (inspect-item-has-key? obj-elem)
         (format #t ":~s -" (inspect-item-key obj-elem)))
       (format #t "> ~s\n" (inspect-item-value obj-elem))))))

(define (inspect-repl obj)
  (let retry ((ctx (object->inspect-ctx obj)))
    (inspect-print ctx)
    (catch 'inspect-error-escape
      (let ((input (inspect-read)))
        (inspect-repl (inspect-eval input obj ctx))
        (retry ctx)))))

(define (inspect obj)
  "Inspects <obj>. The inspector is a variant of a REPL that is used
   to inspect objects. It maintains a pointer to an object, prints that
   object's component pieces in a user-readable form, and then allows
   selection of the 'next' object to inspect."
  (catch 'inspect-exit
    (inspect-repl obj))
   (values))

(push! '(:i inspect) *repl-abbreviations*)
