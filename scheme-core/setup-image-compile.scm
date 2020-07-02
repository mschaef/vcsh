
;;;; setup-image-compile.scm --
;;;;
;;;; Code loaded when compiling an image to set up the environment
;;;; for the image compile.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;;;; This moves a running image out of the way so that the new definitions
;;;; created by an image compile don't overwrite the running compiler. This
;;;; also uses the host compiler  to build an environment the target
;;;; c ompiler can use to bootstrap itself.

(define-package "compiler-cross-compile-setup"
  (:uses "scheme"
         "compiler"))

(compiler::compiler-trace #t ";;;; Configuring for cross compile of Scheme image.\n")

(define *shared-target-symbols* ;; REVISIT: Better way to build this list? Please?
  (set-union (compiler::toplevel-file-form-symbols)
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
               scheme::hash
               scheme::port
               scheme::end-of-file
               scheme::values-tuple
               scheme::unbound-marker
               scheme::trip-wire
               scheme::fast-op)
             '(scheme::*package-list*
               scheme::*provided-packages*)
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
             (compiler::special-form-symbols)))

;; Exclude packages that are in common between the host and the target.
(define *excluded-packages*  (map find-package '("system" "keyword")))

;; Split all the non-excluded packages into host and target packages.

(define (package->host/target! package)
  (let ((name (package-name package)))
    (rename-package! package (string-append "host-" name))
    (let ((new-package (make-package! name)))
      (provide-package! new-package)
      (cons package new-package))))

(define *host/targets* (map package->host/target! (remove #L(memq _ *excluded-packages*) (list-all-packages))))

(define *host->target* (a-list->hash *host/targets*))

;; Ensure that all the newly created packages are on the provided packages list, so there's
;; no attempt to reload them.
(dolist (package host-scheme::*provided-packages*)
  (awhen (hash-ref *host->target* package #f)
    (provide-package! it)))

;; All symbols shared between the host and the target need to get explicitly imported into
;; the target so that they're available.
(dolist (special-form-sym *shared-target-symbols*)
  (let ((target-package (hash-ref *host->target* (symbol-package special-form-sym))))
    (import! special-form-sym target-package)))

;; Make the host image available to the target for bootstrap purposes.
(dolist (h/t *host/targets*)
  (dbind (host . target) h/t
    (dolist (host-sym (local-package-symbols host))
      ;; a) Re-home all of the host package symbols to the target package
      (host-scheme::set-symbol-package! host-sym target)
    
      ;; b) Create a separate global binding in the target packages for each host package global binding
      (when (symbol-bound? host-sym)
        (host-scheme::%define-global (intern! (symbol-name host-sym) target)
                                     (symbol-value host-sym))))))


;; Images can't be written with load unit boundary protection until there are a few
;; basic definitions that have happened. This defers boundary protection until the image
;; expressly asks for it.
(set! host-compiler::*disable-load-unit-boundaries* #t)

;; Image compiles start out in the target scheme package.
(set! host-compiler::*initial-package* "scheme")

;; The host compiler needs to know about the target compiler's *package* variable,
;; so that incoming forms get read in the correct place.
(set! host-compiler::*package-var* (scheme::intern! "*package*" "scheme"))

