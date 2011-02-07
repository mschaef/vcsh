;;;; macro.scm
;;;; October 27th, 2009
;;;; Mike Schaeffer
;;;
;;; Macro support

;; (define-structure %cmacro
;;   transformer)

;; (define (is-macro? m)
;;   (or (%cmacro? m)
;;       (macro? m)))

;; (define (macro-transformer m)
;;   (cond ((%cmacro? m) (%cmacro-transformer m))
;;         ((macro? m) (scheme::%macro-transformer m))
;;         (#t (error "Invalid macro: ~s" m))))

;;; Constructor for Common Lisp style non-hygenic macros:

(defmacro (defmacro lambda-list . macro-body) ;; REVISIT: Macros with :optional arguments
  (check valid-lambda-list? lambda-list)
  (let ((macro-body macro-body) ; shadowed the argument to give us something settable.
        (macro-name (car lambda-list))
        (macro-formals (cdr lambda-list))
        (macro-documentation #f))
    (when (and (string? (car macro-body)) (> (length macro-body) 1))
      (set! macro-documentation (car macro-body))
      (set! macro-body (cdr macro-body)))
    `(define ,macro-name (%macro (%lambda (,@(if macro-documentation
                                                      `((documentation . ,(normalize-whitespace macro-documentation)))
                                                      ())
                                                (macro-name . ,macro-name)
                                                (macro-formals . ,macro-formals))
                                               (form env)
                                               (dbind ,macro-formals (cdr form)
                                                      ,@macro-body))))))