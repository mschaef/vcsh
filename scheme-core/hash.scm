;;;; hash.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; Hash table utilities

(define (key-list->hash keys hash-type default-value)
  "Returns a hash table of type <hash-type> with a key for each element
   in <keys>. <default-value> is bound to each key."
  (let ((h (make-hash hash-type)))
    (dolist (key keys h) ; !! fold would be perfect for this.
      (hash-set! h key default-value))))

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
  (let ((k-var (first head))
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

(define (a-list->hash a-list :optional (hash-type :equal))
  (let ((hash (make-hash hash-type)))
    (dolist (k/v (minimal-alist a-list (case hash-type
                                         ((:equal) assoc)
                                         ((:eq) assq)
                                         (#t (error "Invalid hash-type: ~a" hash-type)))))
      (dbind (k . v) k/v
        (hash-set! hash k v)))
    hash))