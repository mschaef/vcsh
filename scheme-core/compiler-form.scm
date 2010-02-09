;;;; compiler-form.scm
;;;;
;;;; The form compiler entry points.

(define (compile-closure form :optional (genv #f) (at-toplevel? #f))
  (assemble-closure
   (expanded-form-meaning
    (expand-form form genv at-toplevel?) () genv at-toplevel?)))

(define (compile-toplevel-form->closure form :optional (genv #f) (at-toplevel? #f))
  (compile-closure `(%toplevel-lambda ,form) genv at-toplevel?))

(define (compile-form form :optional (genv #f) (at-toplevel? #f))
  ;; REVISIT: Do we need to apply in genv? Currently, we apply in the caller's global environment
  (apply (compile-toplevel-form->closure form genv at-toplevel?)))
