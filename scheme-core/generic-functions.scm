
;;;; generic-functions.scm --
;;;;
;;;; A CLOS-like (but very reduced) implementation of generic functions
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;;; Generic Functions

(define (generic-function? obj)
  "Returns #t if <obj> is a generic function."
  (and (closure? obj)
       (aif (assq 'generic-function? (%property-list obj))
            (cdr it)
            #f)))

(define *generic-function-method-list-cache* (make-hash))

(define (invalidate-method-list-cache!)
  "%Invalidiates the method list cache."
  (set! *generic-function-method-list-cache* (make-hash)))

(add-hook-function! '*class-graph-update-hook*
                    'invalidate-method-list-cache!)

(define (generic-function-methods generic-function)
  (check generic-function? generic-function)
  (get-property generic-function 'method-table ()))

(define (compute-methods-for-signature generic-function signature)
  "Computes the methods defined on <generic-function> that apply to <signature>."
  (let ((method-list (get-property generic-function 'method-table ())))
    (map cdr (filter (lambda (method-list-entry)
                       (classes<=? signature (car method-list-entry)))
                     method-list))))

(define (methods-for-signature generic-function signature)
  "Returns the methods defined on  <generic-function> that apply to <signature>."
  (let ((sig-key (cons generic-function signature)))
    (aif (hash-ref *generic-function-method-list-cache* sig-key #f)
         it
         (let ((method-list (compute-methods-for-signature generic-function signature)))
           (hash-set! *generic-function-method-list-cache* sig-key method-list)
           method-list))))


(define (%generic-function gf-name arity doc-string)
  (letrec ((new-gf (lambda gf-args
                     (let ((current-methods (methods-for-signature new-gf (map type-of gf-args))))
                       (assert (not (null? current-methods)))
                       (letrec ((call-next-method (lambda ()
                                                    (let ((next-fn (pop! current-methods)))
                                                      (apply next-fn call-next-method gf-args)))))
                         (call-next-method))))))

    (set-property! new-gf 'name gf-name)
    (set-property! new-gf 'generic-function? #t)
    (set-property! new-gf 'documentation (aif doc-string it "Generic Function"))
    (set-property! new-gf 'method-table ())
    (set-property! new-gf 'generic-function-arity arity)

    new-gf))

(define (parse-method-spec method-spec)
  (check list? method-spec  "Method specifiers must be proper lists: ~a")
  
  (dbind (method-name . lambda-list) method-spec
    (check symbol? method-name "Method names must be symbols.")

    (let ((lambda-list (canonicalize-method-lambda-list lambda-list)))
      (values method-name
              (map car lambda-list)
              (map cadr lambda-list)))))

(define (parse-generic-function-spec gf-spec)
  (check list? gf-spec  "Generic function specifiers must be proper lists: ~a")
  
  (dbind (gf-name . lambda-list) gf-spec
    (check symbol? gf-name "Generic function names must be symbols.")

    (unless (every? symbol? lambda-list)
      (error "Generic function argument names must be symbols: ~a" lambda-list))

    (values gf-name
            lambda-list)))

(defmacro (define-generic-function gf-spec . code)

  (mvbind (gf-name lambda-list) (parse-generic-function-spec gf-spec)
    
    (mvbind (doc-string declarations code) (parse-code-body code)

      `(begin

         (define ,gf-name
           (%generic-function ',gf-name ,(length lambda-list) ,doc-string))

         (define-method (,gf-name ,@(map #L(list _ #t) (cdr gf-spec)))
           ,(if (null? code)
                `(error "generic function ~a undefined on arguments of type ~a" ',gf-name
                        (list ,@(map (lambda (arg-name) `(type-of ,arg-name)) lambda-list)))
                `(begin ,@code)))
         
         ,gf-name))))

(define (generic-function-signatures function)
  "Returns a list of the defined signatures of generic function <function>."
  (map car (get-property function 'method-table ())))

(define (%update-method-list! method-list method-signature method-closure)
  "Updates <method-list> to include <method-closure> as the definition
   for <method-signature>. If <method-signature> is already defined in the
   method list, then the existing definition is replaced."
  (aif (find (lambda (method-list-entry)
               (classes=? (car method-list-entry) method-signature))
             method-list)
       (begin
         (set-cdr! it method-closure)
         method-list)
       (insert-ordered method-list (cons method-signature method-closure) classes<=? car)))

(define (%extend-generic-function generic-function arg-types method-closure)
  "Adds the method defined by <method-closure>, with the signature <arg-types>,
   to <generic-function>"

  (check generic-function? generic-function)
  (check closure? method-closure)
  (check list? arg-types)

  (let ((gf-arity (get-property generic-function 'generic-function-arity -1))
        (gf-name (get-property generic-function 'name))
        (method-arity (length arg-types)))
    (unless (= gf-arity method-arity)
      (error "Arity mismatch in method definition for ~a, generic function expects ~a arguments, method expects ~a."
             gf-name
             gf-arity
             method-arity)))

  (invalidate-method-list-cache!)
  (set-property! generic-function 'method-table
                 (%update-method-list! (get-property generic-function 'method-table)
                                       arg-types
                                       method-closure)))


(define (valid-type-name? type-name)
  (or (symbol? type-name)
      (eq? type-name #t)))

(define (canonicalize-method-lambda-list lambda-list)
  (check list? lambda-list "Method lambda lists must be proper lists.")
  (let ((canonical-lambda-list (map #L(if (atom? _) (list _ #t) _) lambda-list)))
    (dolist (formal canonical-lambda-list)
      (check (and list? length=2?) formal "Method arguments must be two element lists or symbols.")
      (check symbol? (first formal) "Method argument names must be symbols.")
      (check valid-type-name? (second formal)))
    canonical-lambda-list))

(define (parse-method-spec method-spec)
  (check list? method-spec  "Method specifiers must be proper lists: ~a")
  
  (dbind (method-name . lambda-list) method-spec
    (check symbol? method-name "Method names must be symbols.")

    (let ((lambda-list (canonicalize-method-lambda-list lambda-list)))
      (values method-name
              (map car lambda-list)
              (map cadr lambda-list)))))

(defmacro (define-method method-spec . code)
  (mvbind (gf-name arg-names arg-types) (parse-method-spec method-spec)
    `(%extend-generic-function ,gf-name ',arg-types
                               (lambda (call-next-method ,@arg-names)
                                 ,@code))))
