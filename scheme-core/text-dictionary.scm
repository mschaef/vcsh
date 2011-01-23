;;;; text-dictionary.scm
;;;;
;;;; A dictionary for standard strings:

(define *text-dictionary* (make-hash :eq))

(define (set-text! id string)
  "Updates the global text dictionary to associate the text <string>
   with <id>. <id> must be a symbol, <string> must be a string. If
   the <id> already has an association, it is overwritten."
  (check symbol? id)
  (check string? string)
  (hash-set! *text-dictionary* id string)
  string)

(define (get-text id :optional (default #f))
  "Retrieves the string associated with <id> from the global text dictionary.
   <id> must be a symbol. If there is no such id, <default> is returned
   if it was specified. If there is no <default>, an error is thrown."
  (check symbol? id)
  (aif (hash-ref *text-dictionary* id default)
       it
       (error "String ~s not found in global text dictionary." id)))

(defmacro (define-text . defs)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(p-list-fold (lambda (id string rest)
                      (cons `(set-text! ',id ,string) rest))
                    ()
                    defs)))

(define (->text spec)
  "Coerces <spec> into text. If <spec> is a string, it is returned
   as-is. If it is anything else, it is looked up in the global text
   dictionary."
 (if (string? spec)
     spec
     (get-text spec)))
