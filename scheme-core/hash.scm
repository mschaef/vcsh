
;;;; hash.scm --
;;;;
;;;; Scheme support for hash tables.
;;;;
;;;; (C) Copyright 2001-2015 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (hash-keys hash)
  (map car (hash->a-list hash)))

(define (hash-keys/t hash)
  (map car (filter cdr (hash->a-list hash))))

(define (hash-push! hash key value)
  "Push <value> onto the list in hash table <hash>, referred to by <key>."
  (hash-set! hash key (cons value (hash-ref hash key ()))))

(define (hash-for-each fn hash)
  (dovec (k/v (%hash-binding-vector hash))
    (unless (null? k/v)
      (dbind (k . v) k/v
        (fn k v)))))

(defmacro (dohash head . body)
  (unless (list? head)
    (error "dolist requires a list for <head>" head))
  (let ((k-var (first head)) ; TODO: switch to dbind
        (v-var (second head))
        (hash-form (third head))
        (result-form (fourth head)))
    (unless (symbol? k-var)
      (error "dohash requires a symbol for a key variable binding" k-var))
    (unless (symbol? v-var)
      (error "dohash requires a symbol for a key variable binding" v-var))
    `(begin
       (hash-for-each (lambda (,k-var ,v-var) ,@body) ,hash-form)
       ,result-form)))

(define (a-list->hash a-list)
  (let ((hash (make-hash)))
    (dolist (k/v (minimal-alist a-list))
      (dbind (k . v) k/v
        (hash-set! hash k v)))
    hash))

(define (list->hash kvs)
  (hash-set-multiple! (make-hash) kvs))

(define (list->identity-hash kvs)
  (hash-set-multiple! (make-identity-hash) kvs))

(define (identity-hash . kvs)
  (list->identity-hash kvs))

(define (hash . kvs)
  (list->hash kvs))


