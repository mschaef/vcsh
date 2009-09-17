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
     ((vector? x) (list 'list->vector (qq-expand (vector->list x))))
     ((hash? x) (list 'list->hash (qq-expand (hash->list x))))
     ((atom? x) (list 'quote x))
     ((eq? (car x) 'quasiquote) (qq-expand (cadr x)))
     ((eq? (car x) 'unquote) (cadr x))
     ((eq? (car x) 'unquote-splicing) (error ",@ after ` [ ~a ]" (cadr x)))
     (#t
      (block
       (let ((p x)
             (q '()))
         (while (not (atom? p))
                (when (eq? (car p) 'unquote)
                  (unless (null? (cddr p)) (error "Malformed unquote" p))
                  (return (cons 'append (append (reverse q) (list (cadr p))))))
                (let ((old-p p))
                  (set! p (cdr old-p))
                  (set! q (cons (bracket (car old-p)) q))))
         (cons 'append (append (reverse q) (list (list 'quote p)))))))))
  (qq-expand l))









