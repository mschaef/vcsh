;;;; procedure.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; Basic support for procedure definitions

(define (valid-lambda-list? lambda-list)
  (or (symbol? lambda-list)
      (null? lambda-list)
      (and (pair? lambda-list)
           (symbol? (car lambda-list))
           (valid-lambda-list? (cdr lambda-list)))))

(define (valid-variable-list? vars)
  (or (null? vars)
      (and (pair? vars)
           (symbol? (car vars))
           (valid-variable-list? (cdr vars)))))

(define (parse-code-body code)
  "Parses the code body <code> into a tuple of documentation, declarations, and code. If
   there is no documentation or no declarations, then those are returned as #f."
  (let ((code code)
        (doc-string #f)
        (declarations #f))
    (when (and (string? (car code)) (not (null? (cdr code))))
      (set! doc-string (car code))
      (pop! code))
    (values doc-string declarations code)))

(define (parse-lambda-list l-list)
  "Parse the lambda list <l-list>, returning four values: the list
   of normal arguments, optional arguments, keyword arguments,
   and the name of the rest argument, if any. Syntax errors are
   thrown as erors."

  (define (ensure-valid-name arg)
    (if (and (symbol? arg) (not (keyword? arg)))
        arg
        (error "Invalid argument name ~s in lambda list: ~s." arg l-list)))

  (define (parse-optional o-arg)
    (cond ((symbol? o-arg)
           `(,(ensure-valid-name o-arg) ()))
          ((and (list? o-arg) (length=2? o-arg))
           (ensure-valid-name (car o-arg))
           o-arg)
          (#t
           (error "Invalid optional argument ~s in lambda list: ~s" o-arg l-list))))

  (define (parse-keyword k-arg)
    "Validates keyword binding <k-arg>, and normalizes it into the
   form: (<name> <keyword> <init-form>). Any of the following input
   forms are permissible:

    * <name>
    * (<name>)
    * (<keyword> <name>)
    * (<name> <init-form>)
    * (<keyword> <name> <init-form>).

   <name> is the name of the variable to be bound. <keyword> is the name
   of the binding's keyword, if it is unspecified, it is the result of
   applying intern-keyword! to <name>. <init-form> defaults  to ()
   if unspecified."
    (let retry ((k k-arg))
      (cond ((symbol? k)
             (retry `(,k-arg)))
            ((not (and (list? k) (symbol? (car k))))
             (error "Invalid keyword binding, must be a list or a symbol: ~s" k-arg))
            ((not (keyword? (car k)))
             (retry (cons (intern-keyword! (car k)) k)))
            ((> (length k) 3)
             (error "Invalid keyword binding ~s in lambda list ~s. Too long."
                    k-arg l-list))
            (#t
             `(,(ensure-valid-name (cadr k) ) ,(car k) ,(caddr k))))))

  (let ((explicit-args (take-until-dot l-list))
        (rest-arg (if (atom? l-list) l-list (cdr (last-pair l-list)))))
    (awhen (find #L(and (keyword? _) (not (memq _ '(:optional :keyword)))) explicit-args)
           (error "Unknown argument type ~s in lambda list: ~s" it explicit-args))
    (mvbind (n-args o/k-args) (span #L(not (memq _ '(:optional :keyword))) explicit-args)
       (mvbind (o-args k-args) (span #L(not (eq? _ :keyword)) o/k-args)
            (when (memq :optional k-args)
                  (error ":optional arguments may not follow keyword arguments in lambda list: ~s"
                         explicit-args))
            (values (map ensure-valid-name n-args)
                    (map parse-optional (cdr o-args))
                    (map parse-keyword (cdr k-args))
                    (if (null? rest-arg)
                        rest-arg
                        (cons (ensure-valid-name rest-arg)))
                    (find keyword? explicit-args))))))


(defmacro (accept-optional! r-arg-name default-form)
  (check symbol? r-arg-name)
  `(if (null? ,r-arg-name)
       ,default-form
       (pop! ,r-arg-name)))

(define (keyword-val key p-list missing-thunk)
  (let loop ((p-list p-list))
    (cond ((null? p-list)
           (missing-thunk))
          ((eq? key (car p-list))
           (cadr p-list))
          (#t
           (loop (cddr p-list))))))

(defmacro (accept-keyword r-arg-name keyword default-form)
  (check symbol? r-arg-name)
  `(keyword-val ,keyword ,r-arg-name `(lambda () ,default-form)))

(define (check-keywords arg-list valid-keywords)
  (p-list-fold (lambda (k v rest)
                 (unless (memq k valid-keywords)
                  (error "Invalid keyword argument: ~s, valid choices: ~s"
                         k valid-keywords)))
               ()
               arg-list))

(defmacro (%lambda/mutable-rest p-list l-list . code)
  (let ((n-args (take-until-dot l-list))
        (r-arg (if (atom? l-list) l-list (cdr (last-pair l-list)))))
    (if (null? r-arg)
        `(%lambda ,p-list ,l-list ,@code)
        (with-gensyms (immutable-rest)
          (mvbind (doc-string decls code) (parse-code-body code)
             `(%lambda ,p-list (,@n-args . ,immutable-rest)
                ,@(if doc-string `(,doc-string) ())
                ,@(if decls `(,decls) ())
                (let ((,r-arg ,immutable-rest))
                  ,@code)))))))

(define (canonicalize-code-body code-forms)
  "Ensures that the code body consists of precisely one form."

  (case (length code-forms)
    ((0) '((values)))

;;     ((1) code-forms)
;;     (#t `((begin ,@code-forms)))))
    (#t code-forms)))

(defmacro (named-lambda name l-list . code)

  (let ((p-list (list (cons 'lambda-list l-list))))
    (unless (or (eq? name #f) (null? name))
      (push! `(name . ,name) p-list))
    (mvbind (n-args o-args k-args r-args special?) (parse-lambda-list l-list)
      (awhen (duplicates? (append n-args (map car o-args) (map car k-args) r-args))
        (error "Duplicate formal arguments ~s in lambda list ~s" it l-list))
      (let ((rest-arg (cond ((not (null? r-args))
                             (car r-args))
                            ((not (and (null? o-args) (null? k-args)))
                             (gensym "implied-rest-arg"))
                            (#t
                             ()))))
        (define (o-arg-binding-forms)
          (map (lambda (o-arg)
                 `(,(car o-arg) (accept-optional! ,rest-arg ,(cadr o-arg))))
               o-args))
        (define (k-arg-binding-forms)
          (map (lambda (k-arg)
                 `(,(car k-arg) (accept-keyword ,rest-arg ,(cadr k-arg) ,(caddr k-arg))))
               k-args))
        (define (k-arg-strict-check-form)
          (if (or (not (null? r-args)) ; explicit rest-arg -> no keyword check
                  (null? k-args))
              ()
              `((check-keywords ,rest-arg ',(map second k-args)))))

        (mvbind (doc-string decls code) (parse-code-body code)
          (when doc-string
            (push! `(documentation . ,doc-string) p-list))

        (if (not special?)
            `(%lambda ,p-list ,l-list ,@(canonicalize-code-body code))

            `(,(if (null? o-args) '%lambda '%lambda/mutable-rest)
              ,p-list (,@n-args . ,rest-arg)

              (let* (,@(o-arg-binding-forms)
                     ,@(k-arg-binding-forms))
                ,@(k-arg-strict-check-form)
                ,@(canonicalize-code-body code)))))))))

(defmacro (lambda arglist . code)
  `(named-lambda #f ,arglist ,@code))

(defmacro (define name . defn)
  (cond ((symbol? name)
         `(%define ,name ,(first defn)))
        ((and (pair? name) (symbol? (car name)))
         `(%define ,(car name)
                   (named-lambda ,(car name) ,(cdr name)
                                 ,@defn)))
        (#t
         (error "Invalid syntax for define: ~a" (cons name defn)))))

(define (%primitive-kind subr)
  (check primitive? subr)
  (case (%subr-type-code subr)
    ((0) :subr-0)
    ((1) :subr-1)
    ((2) :subr-2)
    ((3) :subr-2n)
    ((4) :subr-3)
    ((5) :subr-4)
    ((6) :subr-5)
    ((7) :subr-6)
    ((8) :subr-argc)
    ((9) :subr-n)
    (#t :???)))

(define (procedure-lambda-list procedure)
  "Returns two values, the first is the run time lambda list of <procedure>, the
   second value is the original source lambda list, if available. If the source
   lambda list is unknown, it is returned as #f."
  (cond ((closure? procedure)
         (values (car (%closure-code procedure))
                 (aif (assq 'lambda-list (%property-list procedure)) (cdr it) #f))) ; REVISIT: use procedure-property ?
        ((primitive? procedure)
         (values (case (%primitive-kind procedure)
                   ((:subr-0) '())
                   ((:subr-1) '(arg-0))
                   ((:subr-2) '(arg-0 arg-1))
                   ((:subr-3) '(arg-0 arg-1 arg-2))
                   ((:subr-4) '(arg-0 arg-1 arg-2 arg-3))
                   ((:subr-5) '(arg-0 arg-1 arg-2 arg-3 arg-4))
                   ((:subr-6) '(arg-0 arg-1 arg-2 arg-3 arg-4 arg-5))
                   ((:subr-2n) '(arg-0 arg-1 . args))
                   ((:subr-n) 'args-n)
                   ((:subr-f) 'unevaluated-args-n)
                   ((:subr-macro) 'macro-args-n))
                 #f))
        (#t
         (error "Expected procedure, ~a" procedure))))

(define (lambda-list-arity lambda-list)
  "Returns two values, the first is the arity of <lambda-list>, the second
   is a boolean indicating if <lambda-list> accepts a rest argument."
  (let loop ((arg-count 0) (lambda-list lambda-list))
    (cond ((null? lambda-list) (values arg-count #f))
          ((symbol? lambda-list) (values arg-count #t))
          (#t
           (loop (+ 1 arg-count) (cdr lambda-list))))))

(define (procedure-arity procedure)
  "Returns two values, the first is the arity of <procedure>, the second
   is a boolean indicating if <procedure> takes a rest argument."
  (if (generic-function? procedure)
      (values (get-property procedure 'generic-function-arity -1)
              #f)
      (mvbind (lambda-list) (procedure-lambda-list procedure)
        (lambda-list-arity lambda-list))))

(define (procedure-name procedure)
  "Returns the name of <procedure>. For functions created with define, this is
   the name of the symbol to which the function was originally bound. For an object
   that is not a procedure or macro, or does not have a procedure name, #f is
   returned."
  (typecase procedure
    ((subr)
     (%subr-name procedure))
    ((closure)
     (get-property procedure 'name))
    ((macro)
     (procedure-name (%macro-transformer procedure)))
    (#t
     #f)))

(define (parse-let-variables variables)
  "Parses the a variable binding list as would be found in a let
  form. Two values are returned, the first is a list of symbols naming
  the new bindings, and the second is a list of forms to which those
  symbols should be bound. A runtime error is signaled in the event that
  the variables clause contains an error. Note that this function should
  not normally be used, except in the implementation of macros that
  establish local bindings in the style of let."
  (define (normalize-let-variables variables)
    "Ensure that each let variable has a binding. Bindingless variable names are defaulted to null."
    (map (lambda (variable)
           (if (symbol? variable)
               (list variable '())
               variable))
         variables))
  (let ((variables (normalize-let-variables variables))) ; shadows argument...
    (dolist (variable variables)
      (unless (symbol? (car variable))
        (error "Invalid let variable name, names must be symbols: [ ~a ]" (car variable)))
      (when (> (length variable) 2)
        (error "Invalid let variable binding, binding lists can be at most 2 elements in length: a variable name and a binding form" variable)))
    (values (map car variables) (map cadr variables))))

(defmacro (simple-let variables . forms)
  (mvbind (variable-names variable-binding-forms) (parse-let-variables variables)
    (if (null? variable-names)
        `(begin ,@forms)
        `((%lambda () ,variable-names ,@forms) ,@variable-binding-forms))))

(defmacro (named-let name variables . forms)
  (mvbind (variable-names variable-binding-forms) (parse-let-variables variables)
    `(letrec ((,name (lambda ,variable-names ,@forms)))
       (,name ,@variable-binding-forms))))

(defmacro (let . forms)
  "Establishes local variable bindings, with the syntax (let
  <block-name>? ((<var> <value-form>?) ... <body-form>*). During the
  execution of <forms>, each variable <var> is bound to its result of
  evaluating its corresponding value form <value-form>. The new variable
  bindings are only established after each value form is evaluated. If
  the optional paramater <block-name> is specified, another binding is
  established to <block-name> that allows the body forms to recursively
  call the body of the let."
  (cond ((null? forms)
         (error "Incomplete let"))
        ((symbol? (car forms))
         `(named-let ,(first forms) ,(second forms) ,@(cddr forms)))
        ((list? (car forms))
         `(simple-let ,(first forms) ,@(cdr forms)))
        (#t
         (error "Malformed let, the first argument must either be a symbol (the block name for a named let), or a list (a list of variable bindings). [ ~a ]" (car forms)))))

(defmacro (let* new-bindings . code)
  (if (null? new-bindings)
      `(begin ,@code)
      `(let (,(car new-bindings))
         (let* ,(cdr new-bindings) ,@code))))

(defmacro (letrec new-bindings . code)
  (mvbind (variable-names variable-binding-forms) (parse-let-variables new-bindings)
    `(let ,variable-names
       ,@(map (lambda (new-binding new-value-form)
                `(set! ,new-binding ,new-value-form))
              variable-names
              variable-binding-forms)
       ,@code)))

;;; Utility macros

(defmacro (%protect-symbol-values symbols . code)
  (let ((old-binding-symbols (map gensym symbols)))
    `(let ,(map (lambda (old-binding-symbol symbol)
                  (list old-binding-symbol `(symbol-value ',symbol)))
                old-binding-symbols symbols)
       (unwind-protect
        (lambda () ,@code)
        (lambda ()
          ,@(map (lambda (old-binding-symbol symbol)
                   `(set-symbol-value! ',symbol ,old-binding-symbol))
                 old-binding-symbols symbols))))))

(defmacro (dynamic-let new-bindings . code)
  (let ((new-binding-symbols (map gensym new-bindings)))
    `(%protect-symbol-values ,(map (lambda (binding) (car binding)) new-bindings)
                             (let ,(map (lambda (new-binding-symbol new-binding)
                                          (list new-binding-symbol (cadr new-binding)))
                                        new-binding-symbols new-bindings)
                               ,@(map (lambda (new-binding-symbol new-binding)
                                        `(set! ,(car new-binding) ,new-binding-symbol))
                                      new-binding-symbols new-bindings)
                               ((lambda () ,@code))))))

(define (%subr-by-name name :optional (default :NOT-FOUND-SO-PANIC))
  "Returns a subr named <name>, returning <default> if not found. If
   <default> is not specified, panics if <name> is not found."
  (check string? name)
  (let ((subr (hash-ref (%subr-table) name default)))
    (if (eq? subr :NOT-FOUND-SO-PANIC)
       (%panic (string-append "Undefined SUBR in %subr-by-name: " name))
       subr)))
