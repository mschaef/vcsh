;;;; hash-tests.scm
;;;; 
;;;; Some simple hash table diagnostics.

(define (keys->hash keys :optional (hash-type :equal))
  "Returns a hash of type <hash-type> populated with keys taken
   from the list <keys>. The value for each key is the key itself."
  (fold (lambda (key table)
          (hash-set! table key key))
        (make-hash hash-type)
        keys))

(define (file->hash filename :optional (hash-type :equal))
  "Returns a hash of type <hash-type> populated with keys taken 
   from the first object read from the file named by <filename>."
  (with-port p (open-file filename)
    (keys->hash (read p) hash-type)))

(define (%hash-binding-list hash)
  (vector->list (%hash-binding-vector hash)))

(define (hash-load-factor hash)
  "Returns the fraction of hash table entries to hash table buckets."
  (let ((bindings (%hash-binding-list hash)))
    (/ (length (remove null? bindings))
       (length bindings))))

(define (hash-collision-ratio hash)
  "Returns the fraction of hash keys stored in bins other than their hash value."
  (let ((bindings (%hash-binding-vector hash)))
    (let loop ((ii 0)
               (total-keys 0)
               (total-lookups 0))
      (if (= ii (length bindings))
          (/ total-lookups total-keys)
          (let ((binding (vector-ref bindings ii)))
            (cond ((pair? binding)
                   (loop (+ ii 1) 
                         (+ total-keys 1)
                         (+ total-lookups
                            (if (= ii (sxhash (car binding) hash))
                                0
                                1))))
                  (#t
                   (loop (+ ii 1) total-keys total-lookups))))))))
               

(define (hash-stats hash)
  (dynamic-let ((*info* #f))
    (format #t "; load factor = ~a\n" (hash-load-factor hash))
    (format #t "; collisions  = ~a\n" (hash-collision-ratio hash))))

(define b ())

(begin (set! b (file->hash "benchmarks/corncob_lowercase.sxp" :equal)) ())