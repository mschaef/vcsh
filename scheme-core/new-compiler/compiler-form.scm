;;;; compiler-form.scm
;;;;
;;;; The form compiler entry points.

(define (compile-closure form :optional (genv #f) (at-toplevel? #f))
  (assemble-closure
   (expanded-form-meaning 
    (expand-form form genv at-toplevel?) () genv at-toplevel?)))

(define (compile-form form :optional (genv #f) (at-toplevel? #f))
  (apply (compile-closure `(%toplevel-lambda ,form) genv at-toplevel?)))

