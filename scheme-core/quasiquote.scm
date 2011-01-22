;;;; quasiquote.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; Quasiquote
;;;
;;; This is a simplified transcription of Guy Steele's backquote
;;; into Scheme. It does not include any form of simplification
;;; or the ,. destructive backquote operator.

(defmacro (quasiquote l)
  (define (qq-expand x)
    (define (bracket x)
      (cond
       ((atom? x) (list 'list (qq-expand x)))
       ((eq? (car x) 'unquote) (list 'list (cadr x)))
       ((eq? (car x) 'unquote-splicing) (cadr x))
       (#t (list 'list (qq-expand x)))))
    (cond
     ;; TODO: Structure support for quasiquote
     ;; TODO: Instance support for quasiquote
     ((vector? x) (list 'list->vector (qq-expand (vector->list x))))
     ((hash? x) (list 'list->hash (qq-expand (hash->list x))))
     ((atom? x) (list 'quote x))
     ((eq? (car x) 'quasiquote) (qq-expand (cadr x)))
     ((eq? (car x) 'unquote) (cadr x))
     ((eq? (car x) 'unquote-splicing) (error "Unquote splicing immediately after quasiquote: ~s" x))
     (#t
      (let loop ((p x) (q ()))
        (cond ((atom? p)
               (cons 'append (append (reverse q) (list (list 'quote p)))))
              ((eq? (car p) 'unquote)
               (unless (null? (cddr p))
                 (error "Malformed unquote" p))
               (cons 'append (append (reverse q) (list (cadr p)))))
              (#t
               (loop (cdr p) (cons (bracket (car p)) q))))))))
  (qq-expand l))








