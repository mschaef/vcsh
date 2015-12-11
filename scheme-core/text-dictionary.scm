
;;;; text-dictionary.scm --
;;;;
;;;; A dictionary for standard strings.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define *text-dictionary* (make-identity-hash))

(define (set-text! id string)
  "Updates the global text dictionary to associate the text <string>
   with <id>. <id> must be a symbol, <string> must be a string. If
   the <id> already has an association, it is overwritten."
  (runtime-check symbol? id)
  (runtime-check string? string)
  (hash-set! *text-dictionary* id string)
  string)

(define (get-text id :optional (default #f))
  "Retrieves the string associated with <id> from the global text dictionary.
   <id> must be a symbol. If there is no such id, <default> is returned
   if it was specified. If there is no <default>, an error is thrown."
  (runtime-check symbol? id)
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
