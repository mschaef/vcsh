;;;; package.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; Package functions

;; This is really an attribute of the loader, but because package
;; definitions require access to the load hook, it's defined here.
;;
;; REVISIT: This is just a specific sympton of the general problem
;; that the s-core load order is poorly defined. This should be
;; fixed.
(define *finalize-load-hook* ())

(define (list-all-packages)
  (list-copy (%current-package-list)))

(define (intern-keyword! keyword-name)
  "Interns a keyword symbol named <keyword-name>. <keyword-name> can be
   a string or a symbol. If it is a symbol, the keyword's name is taken
   to be the symbol's print name."
  (let ((keyword-name (cond ((string? keyword-name) keyword-name)
                            ((symbol? keyword-name) (symbol-name keyword-name))
                            (#t (error "Invalid keyword name: ~s" keyword-name)))))
    (intern! keyword-name "keyword")))

(define (make-composite-symbol package . names) ;; nee build-symbol
  "Builds a symbol by calling string-append to concatenate <names>,
   and then interning a symbol of that name into <package>."
  (check package? package)
  (intern! (apply string-append names) package))


(define (->package package-spec :optional (not-found-handler (always #f)))
  "Coerces <package-spec> into a package. <package-spec> can be a package name
   in either symbol or string form, or a package itself. If a named package is
   not found, the <not-found-handler> is invoked with the argument <package-spec>,
   and it provides the return value. The default handler always returns false."
  (if (package? package-spec)
      package-spec
      (aif (find-package (name->string package-spec "package"))
           it
           (not-found-handler package-spec))))

(define (->packages package-specs :optional (not-found-handler #f))
  "Coerces <package-specs> into a list of packages. <package-specs>
   can be a package name in symbol or string form, a package, or a list
   of any of the above.  If a named package is not found, the <not-found-handler>
   is invoked with the package specification and it must either return a
   package matching the specification or signal an error."
  (if (atom? package-specs)
      (->packages (list package-specs) not-found-handler)
      (map #L(->package _ not-found-handler) package-specs)))

(define (import-package! packages-to-import re-export? :optional (target-package *package*))
  "Imports all of the exported symbols in the packages listed in <packages-to-import>.
   If <re-export?> is #t, the newly imported symbols are re-exported from the target
   package."
  (let ((target-package (->package target-package #L(error "Target package not found: ~a" _))))
    (dolist (package (->packages packages-to-import #L(error "Package to import not found: ~a" _)))
      (dolist (sym (exported-package-symbols package))
        (import! sym target-package)
        (when re-export?
          (export! sym target-package))))))

(define (use-package! to-use :optional (target *package*))
  "Adds the packages listed in <to-use> to the <target> packages's use
   list. <to-use> and <target> can be specified as a string name, symbol
   name, or as a package itself.  If any named package cannot be found,
   it is reported as an error."
  (define (package-list-union xs ys)
    (fold-right (lambda (x ys)
                  (if (member x ys) ys (cons x ys)))
                ys xs))
  (let ((target-package (->package target #L(error "Target package not found: ~a" _)))
        (packages-to-use (->packages to-use #L(require-package! _))))

    (%set-package-use-list! target-package
                            (package-list-union (%package-use-list target-package)
                                                packages-to-use))))

(define *provided-packages* ()
  "A list of all packages that have successfully been provided by the
  require-package! mechanism.")

(define *loading-packages* ())

(define (package-provided? package-spec)
  "Returns the package if <package-spec> specifies a package successfully
   provided by the require-package! mechanism. Returns #f otherwise."
  (aand (->package package-spec)
        (memq it *provided-packages*)
        it))

(define (provide-package! package-spec)
  "Marks the package specified by <package-spec> as having been provided
   by the require-package! mechanism."
  (let ((package (->package package-spec)))
    (unless (memq package *provided-packages*)
      (push! package *provided-packages*))))

(define (provide-package-on-successful-load! package-spec)
  "Arrange by <package-spec> to be provided by provide-package! if the current
   load terminates successfully."
  (add-hook-function! '*finalize-load-hook* #L0(provide-package! package-spec)))

;; By default, the scheme package is provided...
(push! (find-package "scheme") *provided-packages*)

;(provide-package! "scheme") REVISIT: Why does this cause infinite recursion?

(define (attempt-to-provide-package package-name)
  "Attempts to provide the package named by <package-name>. If the package
   is currently unprovided, it will be loaded from either an internal
   file or an external file with the same name as the package. If the load is
   fully successful, it will be added to the provided package list. Attempts
   to provide a package recursively will result in a circular package
   dependancy error."
  (let ((package-name (name->string package-name "package")))
    (aif (package-provided? package-name)
         it
         (begin
           (when (member package-name *loading-packages*)
             (error "Circular package dependancy on ~a while loading ~a."
                    package-name *loading-packages*))
           (dynamic-let ((*loading-packages* (cons package-name *loading-packages*)))
             (if (find-internal-file package-name)
                 (load-internal package-name)
                 (load (string-append package-name ".scm")))
             (let ((loaded-package (find-package package-name)))
               (when loaded-package
                 (provide-package! package-name))
               loaded-package))))))

(define (require-package! package-name)
  "Requires the presence of the package specified in <package-name>.
   If the package specified has not been provided through the require-package!
   mechanism, then they are loaded  with attempt-to-provide-package."
  (begin-1
   (attempt-to-provide-package package-name)
   (unless (package-provided? package-name)
     (error "Package ~a undefined." package-name))))


(define (local-package-symbols :optional (package *package*))
  "Computes the list all symbols defined in <package>, excluding those defined
   through the use list."
  (map cadr (hash->a-list (%package-bindings (->package package)))))

(define (exported-package-symbols :optional (package *package*))
  "Computes the list of all exported symbols defined in <package>."
  (map cadr (filter cddr (hash->a-list (%package-bindings (->package package))))))

(define (private-package-symbols :optional (package *package*))
  "Computes the list of all non-exported symbols locally defined in <package>."
  (map cadr (remove cddr (hash->a-list (%package-bindings (->package package))))))

(define (bound-private-symbols :optional (package *package*))
  "Computes the list of all non-exported symbols locally defined in <package>
   that are bound to procedures."
  (filter (lambda (sym)
            (procedure? (symbol-value sym)))
          (filter symbol-bound? (private-package-symbols package))))

(define (all-package-symbols package)
  "Computes the list all symbols defined in a package, including those defined through the use list."
  (let ((package (->package package)))
    (append! (local-package-symbols package)
             (append-map! exported-package-symbols (%package-use-list package)))))

(define (ensure-package! p)
  "Returns the package named by <p>. Creates it if it does not already exist."
  (aif (find-package p) it (make-package! p)))

(define (call-with-package p fn)
  "Invokes the procedure <fn> with *package* set to <p>, restoring the old package
   when the function returns.  The return value is the return value returned by <fn>."
  (let ((old-package *package*))
    (unwind-protect (lambda ()
                      (in-package! p)
                      (fn))
                    (lambda ()
                      (in-package! old-package)))))

(defmacro (with-package p . code)
  "Runs <code> with *package* set to <p>, restoring the old package after
   execution ends."
  `(call-with-package ,p (lambda () ,@code)))

(define (read-in-package p :optional (port (current-input-port)))
  "Reads from <port>, with *package* set to <p>. If <port> is not
   specified, it defaults to the result of calling (current-input-port)."
  (with-package p (read port)))

(define (write-qualified form :optional (port (current-output-port)))
  "Writes <form> to <port>, ensuring that all symbols are fully qualified with
   package names. <port> defaults to the result of calling (current-output-port)
   if not specified."
  (with-package #f (write form port)))

(define (find-package name)
  (if (package? name)
      name
      (let loop ((packages (%current-package-list)))
        (if (null? packages)
            #f
            (let ((package (car packages)))
              (unless (package? package)
                (%panic "damaged package list."))
              (if (equal? name (package-name package))
                  package
                  (loop (cdr packages))))))))

(define (make-package! name)
  (check string? name)
  (when (find-package name)
    (error "Duplicate package name: ~a" name))
  (let ((new-package (%packagecons name)))
    (%set-current-package-list! (cons new-package (%current-package-list)))
    new-package))

(define (package-copy old-package)
  "Creates a duplicate, logically equivalent copy of <package>. The resulting
   package is not placed on the current package list."
  (let* ((new-package (%packagecons (package-name old-package)))
         (bindings (%package-bindings new-package)))
    (%set-package-use-list! new-package (list-copy (%package-use-list old-package)))
    (dohash (sym-name sym-rec (%package-bindings old-package))
            (hash-set! bindings sym-name (cons (car sym-rec) (cdr sym-rec))))
    new-package))

(define (rename-package! p new-name)
  "Renames package <p> to <new-name> ensuring that there is no package already
   named <new-name>. If there is such a package, signals an error."
  (let ((package (->package p))
        (new-name (if (symbol? new-name)
                      (symbol-name new-name)
                      new-name)))
    (unless (package? package)
      (error "Cannot rename package ~s, no such package." p))
    (when (find-package new-name)
      (error "Cannot rename package ~s. The new name, ~s, already exists." package new-name))
    (%set-package-name package new-name)))

(define (delete-package! p)
  "Removes package <p> from the system. This entails removing <p> from
   the global package list, uninterning all symbols homed in <p>, and
   removing <p> from all other package use lists. Needless to say, you
   should use caution when doing this."
  (let ((package (->package p)))
    (unless (package? package)
      (error "Cannot delete package ~s, no such package." p))

    ;; 1. Unintern all symbols local to the package
    (map #L(unintern! _ package) (local-package-symbols package))

    ;; 2. Remove the package from all other use lists
    (for-each (lambda (potential-user)
                (%set-package-use-list! potential-user
                                        (delete package
                                                (%package-use-list potential-user)
                                                eq?)))
              (list-all-packages))

    ;; 3. Remove the package from the master package list
    (%set-current-package-list!
     (delete package (%current-package-list) eq?))))


(define (in-package! name)
  "Switch the current package to the package named by <name>, creating it
   if it does not exist."
  (let ((p (ensure-package! name)))
    (set! *package* p)
    p))

(defmacro (define-package package-name . clauses)
  "Begin defining a package named <package-name>. After execution,
   *package* will be bound to a package named <package-name>, as
   in a call to in-package!. Each <clause> is applied to the
   new package, in the appropriate order. Clauses include
   (:uses \"package-name\" ...): use a package, (:exports
   \"symbol-name\" ...): export symbols from the package,
  (:import-from \"package-name\" \"symbol-name\"...): imports
  symbols from the package and (:intern \"symbol-name\"...):
  intern symbols of the specififed name. (:requires \"package-name\"...):
  requires packages of the specified name, loading them if
  necessary. (:loads \"filename\" ...): loads the specified files.
  (:includes \"filename\" ...): includes the specified files."
  (define (all-clauses-of-type clause-type)
    (filter (lambda (clause) (eq? (car clause) clause-type)) clauses))
  (define (validate-clauses clauses)
    (unless (null? clauses)
      (assert (pair? clauses))
      (let ((first-clause (car clauses)))
        (unless (pair? first-clause)
          (error "Invalid clause while defining package: ~a, clauses must be lists: ~a" package-name first-clause))
        (unless (member (car first-clause) '(:exports :uses :intern :requires
                                             :loads :includes :shadows))
          (error "Invalid clause while defining package: ~a, bad clause type: ~a" package-name (car first-clause))))
      (validate-clauses (cdr clauses))))
  (define (expand-uses-clauses)
    (map (lambda (used-package-name)
           `(use-package! ,used-package-name ,package-name))
         (append-map cdr (all-clauses-of-type :uses))))
  (define (expand-requires-clauses)
    (map (lambda (required-package-name)
           `(require-package! ,required-package-name))
         (append-map cdr (all-clauses-of-type :requires))))
  (define (expand-exports-clauses)
    (append-map (lambda (symbol-name)
                  ;; REVISIT: interns in define-package fail to notice if the
                  ;;   symbol is interned in the wrong package (ie: it picks up
                  ;;   a symbol in the use list.) In this case, the export from
                  ;;   the package fails since it can't find that symbol.
                  ;;   This can either  be fixed by warning in this case, or
                  ;;   arranging for the interns to occur before any uses, and
                  ;;   tolerating the shadowing that occurs.
                  ;; REVISIT: fails with (symbol? symbol-name)
                  `((intern! ',symbol-name ,package-name)
                    (export! ',symbol-name ,package-name)))
                (append-map cdr (all-clauses-of-type :exports))))

  (define (expand-loads-clauses)
    (map (lambda (load-spec)
           `(load ,load-spec))
         (append-map cdr (all-clauses-of-type :loads))))
  (define (expand-includes-clauses)
    (map (lambda (include-spec)
           `(include ,include-spec))
         (append-map cdr (all-clauses-of-type :includes))))
  (define (expand-intern-clauses)
    (map (lambda (symbol-name)
           `(intern! ',symbol-name ,package-name))
         (append-map cdr (all-clauses-of-type :intern))))
  (define (expand-shadows-clauses)
    (map (lambda (symbol-name)
           `(shadow-symbol! ',symbol-name ,package-name))
         (append-map cdr (all-clauses-of-type :shadows))))
  (validate-clauses clauses)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
              (ensure-package! ,package-name)
              ,@(expand-requires-clauses)
              ,@(expand-uses-clauses)
              (in-package! ,package-name)
              ,@(expand-shadows-clauses)
              ,@(expand-intern-clauses)
              ,@(expand-exports-clauses)
              ,@(expand-includes-clauses)
              ,@(expand-loads-clauses)
              (provide-package-on-successful-load! ,package-name)))

(define (find-direct-symbol-record sym-spec package require-external?)
  "Looks for the symbol record named by <sym-spec> in the
   package <package>.  The symbol must appear directly
   in the package to be found by this function. If <sym-spec>
   is a symbol, then the symbol in the package must be the same
   as <sym-spec>. If the symbol record is not found, returns #f.
   If <require-external?> is #t, then the symbol must be externally
   visible to be returned."
  (check package? package)
  (let* ((sym-name (name->string sym-spec "symbol"))
         (binding (hash-ref (%package-bindings package) sym-name #f)))
      (if (or (not binding)
              (and (symbol? sym-spec)
                   (not (eq? sym-spec (car binding))))
              (and require-external?
                   (not (cdr binding))))
          #f
          binding)))

(define (find-symbol-record sym-spec package)
  "Looks for the symbol record named by <sym-spec> in the
   package <package>.  The symbol can appear either directly
   in the package or as an external symbol in a package on
   <package>'s use-list.  Two values are returned: the first
   is the symbol record itself, and the second is the package
   in which it was found. If <sym-spec> is a symbol, then the
   symbol in the package must be the same as <sym-spec>. If the
   symbol record is not found, both return values are returned
   as #f."
  (check package? package)
  (let loop ((pkgs (cons package (%package-use-list package))))
    (if (null? pkgs)
        (values #f #f)
        (let ((cpkg (car pkgs)))
          (aif (find-direct-symbol-record sym-spec cpkg
                                          (not (eq? cpkg package)))
               (values it cpkg)
               (loop (cdr pkgs)))))))

(define (find-symbol name :optional (package *package*))
  "Looks for a symbol named <name> in the package <package>. Two
   values are returned: the first is the symbol and the second is
   an indicator that describes how that symbol is visible in the
   package (:internal, :external, or :inherited). If the symbol
   is not found, both values are returned as #f."
  (let ((package (->package package)))
    (mvbind (srec spkg) (find-symbol-record name package)
      (cond ((not srec)
             (values #f #f))
            ((not (eq? spkg package))
             (values (car srec) :inherited))
            (#t
             (values (car srec) (if (cdr srec) :external :internal)))))))

(define (intern! name :optional (package *package*))
  (let ((package (->package package #L(error "Target package for intern! not found: ~a" _))))
    (mvbind (srec spkg) (find-symbol-record name package)
      (if (and srec (or (cdr srec) (eq? spkg package)))
          (car srec)
          (let ((sym (string->uninterned-symbol name)))
            (set-symbol-package! sym package)
            (add-symbol-to-package sym package)
            sym)))))

(define (set-symbol-export-flag! sym-spec package flag)
  (let ((srec (find-direct-symbol-record sym-spec package #f)))
    (unless srec
      (error "Symbol ~s not found in package ~s while setting export flag."
             sym-spec package))
    (set-cdr! srec flag)))

;; TODO: export! should verify that it isn't introducing package inconsistancies
;; prior to actually doing the export. See CLtL2 for details.

(define (export! sym-or-syms :optional (package *package*))
  (let ((package (->package package #L(error "Target package for export! not found: ~a" _))))
    (define (do-export! sym)
      (set-symbol-export-flag! sym package #t))
    (if (list? sym-or-syms)
        (dolist (sym sym-or-syms)
          (do-export! sym))
        (do-export! sym-or-syms))))

(define (unexport! sym-or-syms :optional (package *package*))
  (let ((package (->package package #L(error "Target package for unexport! not found: ~a" _))))
    (define (do-unexport! sym)
      (set-symbol-export-flag! sym package #f))
    (if (list? sym-or-syms)
        (dolist (sym sym-or-syms)
          (do-unexport! sym))
        (do-unexport! sym-or-syms))))

(define (import! sym-or-syms :optional (package *package*))
  (let ((package (->package package #L(error "Target package for import! not found: ~a" _))))
    (define (do-import sym)
      ;; This find is done on the name, rather than the symbol itself,
      ;; since what we're trying to detect is if the symbol in the
      ;; package under that name is the same as the symbol being
      ;; imported. (If not, we signal an error.)
      (aif (find-direct-symbol-record (symbol-name sym) package #f)
           (unless (eq? (car it) sym)
             (error "Symbol name conflict in import: ~s" sym))
           (add-symbol-to-package sym package)))
    (if (list? sym-or-syms)
        (dolist (sym sym-or-syms)
          (do-import sym))
        (do-import sym-or-syms))
    (values)))

(define (exported? symbol)
  "Returns #t if <symbol> is exported from its home package, #f otherwise.
   If <symbol> is not a symbol, throws an error."
  (check symbol? symbol)
  (aif (symbol-package symbol)
       (mvbind (sym status) (find-symbol (symbol-name symbol) it)
         (eq? status :external))
       #f))

(define (unintern! sym :optional (package *package*))
  "Removes <sym> from <package> and sets its home to () if it was
   previously homed in <package>. The resulting symbol becomes an
   uninterned symbol.  If <sym> has been manually imported into another
   package, it will remain accessible through that package, even though
   it might no longer have a home. Throws an error if <sym> is not a
   symbol or <package> is not a package."
  (check symbol? sym)
  (let ((package (->package package #L(error "Target package for unintern not found: ~a" _))))
    (mvbind (psym visibility) (find-symbol sym package)
      (when (and (eq? psym sym)
                 (not (eq? visibility :inherited)))
        (hash-remove! (%package-bindings package) (symbol-name sym))
        (when (eq? (symbol-package sym) package)
          (set-symbol-package! sym ()))))
  (values)))

;; REVISIT: if a symbol is passed into shadow-symbol!, then it should fail if
;; that isn't the exact symbol that would be shadowed.
(define (shadow-symbol! print-name :optional (package *package*))
  "Ensures that <package> has a unique symbol named <print-name>, regardless
   of whether or not one is visible through inheritance."
  (let* ((name (name->string print-name "symbol"))
         (package (->package package #L(error "Package not found: ~s" _)))
         (srec (find-direct-symbol-record name package #f)))
    (if srec
        (car srec)
        (let ((sym (string->uninterned-symbol name)))
          (set-symbol-package! sym package)
          (add-symbol-to-package sym package)
          sym))))

;; TODO: this interacts badly with the compiler
;;
;; (define gensym
;;   (let ((count 0)) ;; this, in particular
;;     (lambda name
;;       (set! count (+ count 1))
;;       (if (or (not (pair? name))
;;               (not (string? (car name))))
;;           (set! name "GS")
;;           (set! name (car name)))
;;       (string->uninterned-symbol
;;        (string-append
;;         name
;;         (number->string count))))))


(define *gensym-count* 0)

(define gensym
  (lambda name
    (set! *gensym-count* (+ *gensym-count* 1))
    (if (or (not (pair? name))
            (not (string? (car name))))
        (set! name "GS")
        (set! name (car name)))
    (string->uninterned-symbol
     (string-append
      name
      "-"
      (number->string *gensym-count*)))))

(defmacro (with-gensyms gensym-names . code)
  (check list? gensym-names)
  `(let ,(map (lambda (gensym-name)
                (check symbol? gensym-name)
                `(,gensym-name (gensym ,(symbol-name gensym-name))))
              gensym-names) ,@code))

(define (build-symbol prefix sym suffix)
  "Builds a symbol by adding a prefix and a suffix. The result symbol
   is in the same package as the input symbol."
  (let ((name (symbol-name sym))
        (pkg (symbol-package sym)))
    (let ((built-name (string-append prefix name suffix)))
      (if pkg
          (intern! built-name pkg)
          (string->uninterned-symbol built-name)))))

(define (name? name)
  "Returns <name> if it is a valid name, #f otherwise. Names are considered
   valid if they can be successfully processed by name->string."
  (or (symbol? name) (string? name)))

(define (name->string name :optional (name-type #f))
  "Returns <name> as a string, throwing an error if <name> is not
   a valid name. <name-type> optionally adds clarifying text to the
   error message indicating the type of the name."
  (cond ((symbol? name) (symbol-name name))
        ((string? name) name)
        (name-type (error "Invalid ~a name: ~s" name-type name))
        (#t (error "Invalid name: ~s" name))))

(define (all-symbols)
  "Returns a list of all interned symbols."
  (apply set-union/eq (map all-package-symbols (list-all-packages))))

(define (local-package-variables :optional (package *package*))
  (filter symbol-bound? (local-package-symbols package)))

(define (all-package-variables :optional (package *package*))
  (filter symbol-bound? (all-package-symbols package)))

;; TODO: split out the lenv and genv versions of all the symbol-value functions
;; TODO: the symbol-value functions use genv==#f as the way to signal 'current environment'. This
;;  is because the compiler depends on this behavior. Fix this.

(define (symbol-bound? sym :optional (lenv ()) (genv #f))
  (check symbol? sym)
  (let ((genv (if (eq? #f genv) (%current-global-environment) genv)))
    (if (or (pair? (env-lookup sym lenv))
            (not (eq? (%global-environment-ref genv (%symbol-index sym))
                      (%unbound-marker))))
        sym
        #f)))

(define (%symbol-value sym :optional (lenv ()) (genv (%current-global-environment)))
  (check symbol? sym)
  (check %global-environment? genv)
  (aif (pair? (env-lookup sym lenv))
       (car it)
       (%global-environment-ref genv (%symbol-index sym))))

(define (symbol-value sym :optional (lenv ()) (genv #f))
  (let* ((genv (if (eq? #f genv) (%current-global-environment) genv))
         (val (%symbol-value sym lenv genv)))
    (if (eq? val (%unbound-marker))
        (trap-unbound-global system::TRAP_UNBOUND_GLOBAL 'symbol-value sym)
        val)))

(define (set-symbol-value! sym val :optional (lenv ()) (genv #f))
  (check (not keyword?) sym)
  (aif (pair? (env-lookup sym lenv))
       (set-car! it val)
       (let ((genv (if (eq? #f genv) (%current-global-environment) genv)))
         (unless (symbol-bound? sym () genv)
           (trap-unbound-global system::TRAP_UNBOUND_GLOBAL 'symbol-value sym))
         (begin
           (%global-environment-set! genv (%symbol-index sym) val)
           val))))

(define (unbind-symbol! sym :optional (genv (%current-global-environment)))
  (check (and symbol? (not keyword?)) sym)
  (let ((idx (%symbol-index sym)))
    (unless (= idx 0)
      (%global-environment-set! genv idx (%unbound-marker))))
  (values))


