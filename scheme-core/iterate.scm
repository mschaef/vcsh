;;;; iterate.scm
;;;; June 4th, 2008
;;;;
;;;; iterate and iterate/r, macros for more involved iteration scenarios.
;;;; The call interface to iterate and iterate/r is borrowed from Scheme48,
;;;; although the underlying implementation and the mechanism for defining
;;;; new sequence types is specific to vcsh.


;; REVISIT: a macro sequence-values that takes a bounded sequence specification and returns it as a list
;; TESTTHIS: iterate needs a test suite...

(define-structure iterate-sequence-expansion
  "This is the result of expanding a iterate sequence clause. It contains of a series
   of sub-forms that are dropped into various spots of a generic loop template."
  (state-vars
   "A list of variable binding forms for variables used to maintain sequence
    state from one iteration to the next. Each of these binding forms is
    a three element list: (<var> <initial> <step>).  <var> is the name of the
    state variable, <initial> is a form that is evaluated before entry into the
    loop to determine the initial value of the variable, and <step> is an expression
    evaluated at the end of the loop to compute the next value of <var>. <step> forms
    are guaranteed not to be evaluated after the <terminate?-form> for the sequence
    has requested termination."
   :default ())
  (body-vars
   "A list of variable binding forms for variables used within the loop body
    to hold computed values. These binding forms are two element lists: (<var>
    <form>). <form> is evaluated at the beginning of the loop body, in the scope
    of the loop's <state-vars> to compute the value to which <var> is bound for
    the loop's body. Loop variable binding forms are not evaluated after the
    <terminate?-form> for the sequence has requested termination."
   :default ())
  (terminate?-form
   "A predicate form that returns true if the sequence is requesting termination
    of the loop."
   :default #f)
  (enclosing-form
   "A optional list that is the beginning of a form that will enclose the body
    of the loop. The loop body is placed in the final position of a list whose first
    elements are the elements of <enclosing-form>. If multiple loop sequences request
    enclosing forms, the first sequence defines the outermost enclosing form and
    subsequent sequences are nested within."
   :default #f))

(define (set-iterate-sequence-expander! seq-type expander)
  "Sets the iterate sequence expander for the sequence type <seq-type> to
   the procedure <expander>. <expander> is a procedure that's
   called on the argument list of the iterate sequence, and returns
   a iterate-sequence-expansion."
  (check symbol? seq-type)
  (check procedure? expander)
  (set-property! seq-type 'iterate-sequence-expander expander))

(defmacro (define-iterate-sequence-expander seq-lambda-list . forms)
  "Defines a new iterate sequence expander for the sequence lambda list
   <seq-lambda-list>. The first element of the sequence lambda list
   names the sequence type and must be a symbol. All of <forms> are places
   within the body of the expander function; The return value of <forms>
   must be an iterate-sequence-expansion."
  (check list? seq-lambda-list)
  (check symbol? (car seq-lambda-list))
  (check (not null?) forms)
  `(set-iterate-sequence-expander! ',(car seq-lambda-list)
                                   (lambda ,(cdr seq-lambda-list) ,@forms)))

(define-iterate-sequence-expander (count var :optional (start 0) (end #f) (step 1))
  "Binds <var> to successive numbers, starting at <start> and stepping by <step>
   on each iteration.  If <end> is specified, the loop stops on the iteration
   in which <var> reaches <end> (If that never happens, the sequence never
   terminates the iteration. <end> and <step> are evaluated only once, at the
   beginning of the loop."
  (unless (symbol? var)
    (error "Count sequence iteration variable must be a symbol: ~s" var))
  (with-gensyms (step-value-sym end-value-sym)
    (make-iterate-sequence-expansion
     :state-vars       `((,var ,start (+ ,var ,step-value-sym))
                         (,step-value-sym ,step ,step-value-sym)
                         ,@(if end
                               `((,end-value-sym ,end ,end-value-sym))
                               ()))
     :terminate?-form   (if end `(>= ,var ,end-value-sym) #f))))

(define-iterate-sequence-expander (list var xs)
  "Binds <var> to successive elements of list <xs>, ending the loop at
   the end of <xs>. <xs> must be a proper, non-dotted, list."
  (unless (symbol? var)
    (error "List sequence iteration variable must be a symbol: ~s" var))
  (with-gensyms (list-pos-sym)
    (make-iterate-sequence-expansion
     :state-vars      `((,list-pos-sym ,xs (cdr ,list-pos-sym)))
     :body-vars       `((,var (car ,list-pos-sym)))
     :terminate?-form `(end-of-list? ,list-pos-sym))))

(define-iterate-sequence-expander (vector var vec)
  "Binds <var> to successive elements of vector <xs>, ending the loop at
   the end of <xs>."
  (unless (symbol? var)
    (error "Vector sequence iteration variable must be a symbol: ~s" var))
  (with-gensyms (ii-sym vec-sym)
    (make-iterate-sequence-expansion
     :state-vars        `((,ii-sym 0 (+ ,ii-sym 1)) (,vec-sym ,vec))
     :body-vars         `((,var (vector-ref ,vec-sym ,ii-sym)))
     :terminate?-form   `(>= ,ii-sym (length ,vec-sym)))))

(define-iterate-sequence-expander (file-lines line-var filename)
  "Binds <line-var> to successive lines of the text file named by <filename>,
   ending the loop at the end of the file."
  (unless (symbol? line-var)
    (error "File-lines sequence iteration variable must be a symbol: ~s" line-var))
  (with-gensyms (file-lines-port-sym)
    (make-iterate-sequence-expansion
     :body-vars         `((,line-var (read-line ,file-lines-port-sym)))
     :enclosing-form    `(with-port ,file-lines-port-sym (open-input-file ,filename))
     :terminate?-form   `(port-at-end? ,file-lines-port-sym))))

(define-iterate-sequence-expander (file-forms line-var filename)
  "Binds <line-var> to successive Lisp forms of the text file named by <filename>,
   ending the loop at the end of the file. No kind of form evaluation is done at
   all, during the read."
  (unless (symbol? line-var)
    (error "file-forms sequence iteration variable must be a symbol: ~s" line-var))
  (with-gensyms (file-forms-port-sym)
    (make-iterate-sequence-expansion
     :body-vars         `((,line-var (read ,file-forms-port-sym)))
     :enclosing-form    `(with-port ,file-forms-port-sym (open-input-file ,filename))
     :terminate?-form   `(port-at-end? ,file-forms-port-sym))))


(define (all-iterate-sequence-types)
  "Returns a list of all sequence types valid for use in iterate, returned
  as a list of lambda-list/doc-string pairs."
  (let ((type-names (filter #L(get-property _ 'iterate-sequence-expander)
                            (all-symbols))))
    (map #L(let ((expander (get-property _ 'iterate-sequence-expander)))
             (cons (cons _ (get-property expander 'lambda-list))
                   (documentation expander)))
         type-names)))

(defmacro (iterate loop-name seqs user-state-vars body-form . maybe-final-form)
  "A named-let with automatic sequence traversal, this is used for iteration
   over various kinds of sequences. <seqs> is a list of  iterate sequence
   specifications that each describe a specific sequence traversal. At the
   beginning of each loop iteration, these are each consulted to determine if
   the loop must terminate, and if not, how each sequence should be advanced to
   the next step. The current list of validate sequence types can be found by
   calling all-iterate-sequence-types."
  (check symbol? loop-name)
  (check list? seqs)
  (mvbind (user-state-var-names user-state-var-init-forms) (parse-let-variables user-state-vars)
   (define (validate-expansion expansion)
      ;; REVISIT: Add expansion validation here, for custom sequence developers
      expansion)
    (define (expand-iterate-sequence-clause sequence-clause)
      (check list? sequence-clause)
      (check symbol? (car sequence-clause))
      
      (aif (get-property (car sequence-clause) 'iterate-sequence-expander)
           (validate-expansion (apply it (cdr sequence-clause)))
           (error "Invalid sequence type ~s in iterate clause ~s."
                  (car sequence-clause) sequence-clause)))
    (define (final-form)
      (cond ((not (null? maybe-final-form))   (car maybe-final-form))
            ((null? user-state-var-names)     ())
            ((length=1? user-state-var-names) (car user-state-var-names))
            (#t                              `(values ,@user-state-var-names))))
    (define (apply-enclosures form enclosures)
      (if (end-of-list? enclosures)
          form
          (append (car enclosures)
                  (list (apply-enclosures form (cdr enclosures))))))
    (let* ((sequence-expansions (map expand-iterate-sequence-clause seqs))
           (seq-state-vars (append-map iterate-sequence-expansion-state-vars sequence-expansions))
           (body-vars (append-map iterate-sequence-expansion-body-vars sequence-expansions))
           (terminate?-forms (map iterate-sequence-expansion-terminate?-form sequence-expansions)))
      (apply-enclosures (with-gensyms (loop-internal-name-sym)
                          `(let ,loop-internal-name-sym (,@(map #L(take _ 2) seq-state-vars)
                                                         ,@user-state-vars)
                                (define (,loop-name ,@user-state-var-names)
                                  (,loop-internal-name-sym ,@(map third seq-state-vars)
                                                           ,@user-state-var-names))
                                (if (or ,@terminate?-forms)
                                    ,(final-form)
                                    (let ,body-vars  ,body-form))))
                        (filter identity (map iterate-sequence-expansion-enclosing-form
                                              sequence-expansions))))))

;(UNIMPLEMENTED call-with-values)

(defmacro (iterate/r seqs user-state-vars body-form . maybe-final-form)
  "A reducing varient of iterate, this automatically loops at the end
   of <body-form>, with <user-state-vars> rebound to the values returned
   by <body-form>."
  (with-gensyms (iterate/r-loop-name)
    `(iterate ,iterate/r-loop-name ,seqs ,user-state-vars
              ,(case (length user-state-vars)
                 ((0) `(begin ,body-form (,iterate/r-loop-name)))
                 ((1) `(,iterate/r-loop-name ,body-form))
                 (#t  `(call-with-values ,body-form ,iterate/r-loop-name)))
              ,@maybe-final-form)))

(defmacro (doiterate seqs . code)
  "An do... form that takes a list of iterate sequences."
  `(iterate/r ,seqs () (begin ,@code)))