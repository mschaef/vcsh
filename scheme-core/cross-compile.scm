
(define (shared-target-symbols)
  (set-union '(compiler::%%begin-load-unit-boundaries
               scheme::%define
               scheme::begin
               scheme::include
               scheme::eval-when)
             '(scheme::free-cell
               scheme::nil
               scheme::boolean
               scheme::cons
               scheme::fixnum
               scheme::flonum
               scheme::character
               scheme::symbol
               scheme::package
               scheme::subr
               scheme::closure
               scheme::macro
               scheme::string
               scheme::vector
               scheme::structure
               scheme::hash
               scheme::port
               scheme::end-of-file
               scheme::values-typle
               scheme::instance
               scheme::unbound-marker
               scheme::trip-wire
               scheme::fast-op)
             '(scheme::*package-list*
               scheme::*provided-packages*)
             '(scheme::iterate-sequence-expander)
             '(scheme::it
               scheme::_)
             '(scheme::and
               scheme::or
               scheme::not
               scheme::>
               scheme::>=
               scheme::<
               scheme::<=
               scheme::= 
               scheme::eq?
               scheme::equal?
               scheme::member)
             (map caar (scheme::all-iterate-sequence-types))
             (compiler::special-form-symbols)))

(define (setup-cross-compiler)
  "Setup for cross compiling using renamed packages."
  (define (package->host/target! package)
    (let ((name (package-name package)))
      (rename-package! package (string-append "host-" name))
      (let ((new-package (make-package! name)))
        (provide-package! new-package)
        (cons package new-package))))
  (format #t "; Configuring for cross compile by renaming packages.\n")
  (let* ((excluded (map find-package '("system" "keyword")))
         (host/targets (map package->host/target! (remove #L(memq _ excluded) (list-all-packages))))
         (host->target (a-list->hash host/targets)))

    ;; 0) Make sure we're providing all the packages we've created
    (dolist (package scheme::*provided-packages*)
      (awhen (hash-ref host->target package #f)
        (provide-package! it)))
    
    (dolist (special-form-sym (shared-target-symbols))
      (let ((target-package (hash-ref host->target (symbol-package special-form-sym))))
        (import! special-form-sym target-package)))

    (dolist (h/t host/targets)
      (dbind (host . target) h/t
        (dolist (host-sym (local-package-symbols host))
          ;; 2) Re-home all of the host package symbols to the target package
          (scheme::set-symbol-package! host-sym target)
    
          ;; 3) Create a separate global binding in the target packages for each host package global binding
          (when  (symbol-bound? host-sym)
            (scheme::%define-global (intern! (symbol-name host-sym) target)
                                    (symbol-value host-sym))))))))



(setup-cross-compiler)

(set! compiler::*package-var* (scheme::intern! "*package*" "scheme"))