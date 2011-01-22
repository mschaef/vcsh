;;; setup-image-compile.scm
;;;
;;; Set up for compiling a scheme image. This moves a running image out of the
;;; way so that the new definitions created by an image compile don't overwrite
;;; the running compiler. This also uses the host compiler  to build an environment
;;; the target compiler can use to bootstrap itself.

(define-package "compiler-cross-compile-setup"
  (:uses "scheme"
         "compiler"))

;; TODO: Use compiler messaging facilities.
(format (current-error-port) ";;;; Configuring for explicit bootstrap cross compile of Scheme image.\n")

(define *shared-target-symbols* ;; TODO: Better way to build this list? Please?
  (set-union '(scheme::eval-when
               scheme::*package-list*)
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
    (import! special-form-sym target-package)

    ;(host-scheme::set-symbol-package! special-form-sym (hash-ref *host->target* (symbol-package special-form-sym)))

    ))

;; Images can't be written with load unit boundary protection until there are a few
;; basic definitions that have happened. This defers boundary protection until the image
;; expressly asks for it.
(set! host-compiler::*disable-load-unit-boundaries* #t)

;; Image compiles start out in the target scheme package.
(set! host-compiler::*initial-package* "scheme")

;; The host compiler needs to know about the target compiler's *package* variable,
;; so that incoming forms get read in the correct place.
(set! host-compiler::*package-var* (host-scheme::intern! "*package*" "scheme"))


