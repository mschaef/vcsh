;;;; properties.scm
;;;; Mike Schaeffer
;;;
;;; Basic support for procedure properties.

(define (get-property obj key :optional (default #f))
  "Given <procdure>, determine the value of the property named by <key>. If the
   named property does not exist, return <default>."
  (if (macro? obj)
      (get-property (%macro-transformer obj) key default)
      (aif (assq key (%property-list obj))
           (cdr it)
           default)))

(define (set-property! obj key value)
  "Sets the value of the <key> property on <obj> to <value>. If the property
   already exists, the existing binding is replaced."
  (if (macro? obj)
      (set-property! (%macro-transformer obj) key value)
      (begin
        (let ((props (%property-list obj)))
          (aif (assoc key props)
               (set-cdr! it value)
               (%set-property-list! obj (alist-cons key value props))))
        obj)))

(define (remove-property! obj key)
  "Removes the property named <key> from <obj>. If the property does
   not exist, no change is made to <obj>'s property list."
  (if (macro? obj)
      (remove-property! (%macro-transformer obj) key)
      (begin
        (%set-property-list! obj (remove #L(eq? (car _) key) (%property-list obj)))
        obj)))

(define (properties obj)
  "Returns a list of the names of the properties associated with <obj>."
  (if (macro? obj)
      (properties (%macro-transformer obj))
      (map car (%property-list obj))))
