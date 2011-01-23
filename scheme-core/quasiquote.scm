;;;; quasiquote.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; Quasiquote
;;;
;;; This is a simplified transcription of Guy Steele's backquote
;;; into Scheme. It does not include any form of simplification
;;; or the ,. destructive backquote operator.

(define *qq-quasiquote* 'quasiquote)
(define *qq-unquote* 'unquote)
(define *qq-unquote-splicing* 'unquote-splicing)

(define *qq-quote* 'quote)
(define *qq-list* 'list)
(define *qq-append* 'append)
(define *qq-list->vector* 'list->vector)
(define *qq-list->hash* 'list->hash)

(defmacro (quasiquote l)
  (define (qq-expand x)
    (define (bracket x)
      (cond
       ((atom? x) (list *qq-list* (qq-expand x)))
       ((eq? (car x) *qq-unquote*) (list *qq-list* (cadr x)))
       ((eq? (car x) *qq-unquote-splicing*) (cadr x))
       (#t (list *qq-list* (qq-expand x)))))
    (cond
     ;; TODO: Structure support for quasiquote
     ;; TODO: Instance support for quasiquote
     ((vector? x) (list *qq-list->vector* (qq-expand (vector->list x))))
     ((hash? x) (list *qq-list->hash* (qq-expand (hash->list x))))
     ((atom? x) (list *qq-quote* x))
     ((eq? (car x) *qq-quasiquote*) (qq-expand (cadr x)))
     ((eq? (car x) *qq-unquote*) (cadr x))
     ((eq? (car x) *qq-unquote-splicing*)
      (error "Unquote splicing immediately after quasiquote: ~s" x))
     (#t
      (let loop ((p x) (q ()))
        (cond ((atom? p)
               (cons *qq-append* (append (reverse q) (list (list *qq-quote* p)))))
              ((eq? (car p) *qq-unquote*)
               (unless (null? (cddr p))
                 (error "Malformed unquote" p))
               (cons *qq-append* (append (reverse q) (list (cadr p)))))
              (#t
               (loop (cdr p) (cons (bracket (car p)) q))))))))
  (qq-expand l))

(define (read-quasiquote port)
  (read-char port)
  (list *qq-quasiquote* (read port #t)))

(define (read-unquote port)
  ;; This is used as a default handler for a syntax map, so
  ;; the usual read process has already consumed the leading comma.
  ;; (read-char port)
  (list *qq-unquote* (read port #t)))

(define (read-unquote-splicing port)
  (read-char port)
  (list *qq-unquote-splicing* (read port #t)))

(define *read-unquote-syntax* (make-syntax-table :name 'unquote-syntax))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-char-syntax! *read-syntax* #\` 'read-quasiquote)
  (set-char-syntax! *read-syntax* #\, *read-unquote-syntax*)
  (set-char-syntax! *read-unquote-syntax* #\@ 'read-unquote-splicing)

  (set-default-syntax! *read-unquote-syntax*  'read-unquote))
