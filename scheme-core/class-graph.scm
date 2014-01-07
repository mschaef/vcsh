
;;;; class-graph.scm --
;;;;
;;;; Code to maintain and analyze the type hierarchy.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (typecode->name tc)
  "Given a numeric typecode <tc>, return the symbolic name for the
type. If there is no valid typecode of that name, returns #f."
  (case tc
    ((#.system::TC_FREE_CELL)      'free-cell)  
    ((#.system::TC_NIL)            'nil)  
    ((#.system::TC_BOOLEAN)        'boolean)  
    ((#.system::TC_CONS)           'cons)  
    ((#.system::TC_FIXNUM)         'fixnum)  
    ((#.system::TC_FLONUM)         'flonum)  
    ((#.system::TC_CHARACTER)      'character)  
    ((#.system::TC_SYMBOL)         'symbol)  
    ((#.system::TC_PACKAGE)        'package)  
    ((#.system::TC_SUBR)           'subr)  
    ((#.system::TC_CLOSURE)        'closure) 
    ((#.system::TC_MACRO)          'macro) 
    ((#.system::TC_STRING)         'string) 
    ((#.system::TC_VECTOR)         'vector) 
    ((#.system::TC_STRUCTURE)      'structure) 
    ((#.system::TC_HASH)           'hash) 
    ((#.system::TC_PORT)           'port) 
    ((#.system::TC_END_OF_FILE)    'end-of-file) 
    ((#.system::TC_VALUES_TUPLE)   'values-typle) 
    ((#.system::TC_UNBOUND_MARKER) 'unbound-marker) 
    ((#.system::TC_GC_TRIP_WIRE)   'trip-wire) 
    ((#.system::TC_FAST_OP)        'fast-op)
    ((#.system::TC_FASL_READER)    'fasl-reader)
    (#t #f)))

(define (%representation-of obj)
  (let ((tc (%typecode obj)))
    (if (= tc system::TC_FLONUM)
        (if (imag-part obj #f) 'complex 'flonum)
        (typecode->name (%typecode obj)))))



(define *class-graph* (make-hash))

(define (valid-class-name? maybe-class-name)
  "Determines if <maybe-class-name> is a valid class name (symbol, #t, or #f)."
  (or (symbol? maybe-class-name)
      (null? maybe-class-name)
      (eq? maybe-class-name #t)
      (eq? maybe-class-name #f)))

(define (validate-class-name . class-names)
  (let loop ((class-names class-names))
    (unless (null? class-names)
      (check valid-class-name? (car class-names))
      (loop (cdr class-names)))))

(define (class-superclass class-name)
  "Returns the superclass of <class-name>, #f if there are none defined."
  (validate-class-name class-name)
  (hash-ref *class-graph* class-name #f))

(define (class-superclasses class-name)
  "Returns a list of all superclasses of <class-name>, including <class-name>, in class order."
  (let recur ((class-name class-name))
    (validate-class-name class-name)
    (if (not class-name)
        ()
        (cons class-name (recur (class-superclass class-name))))))

(define (class=? x y)
  "Returns #t if class <x> is equivalent to class <y>."
  (eq? x y))

(define (classes=? xs ys)
  "Returns #t if all of the classes of <xs> are class=? to their
   corresponding classes in <ys>."
  (let loop ((xs xs) (ys ys))
    (cond ((and (null? xs) (null? ys))
           #t)
          ((or (null? xs) (null? ys))
           #f)
          ((class=? (car xs) (car ys))
           (loop (cdr xs) (cdr ys)))
          (#t
           #f))))

(define (class<=? x y)
  "Returns #t if <x> is a strict subclass of <y>."
  (let loop ((x x) (y y))
    (validate-class-name x y)
    (cond ((not x) #t)
          ((not y) #f)
          ((or (eq? y #t) (class=? x y))
           #t)
          ((hash-has? *class-graph* x)
           (loop (class-superclass x) y))
          (#t #f))))

(define (classes<=? xs ys)
  "Returns #t if all of the classes of <xs> are class<=? to the corresponding
   classes of <ys>."
  (let loop ((xs xs) (ys ys))
    (cond ((null? ys) #t)
          ((null? xs) #f)
          ((or (class=? (car xs) (car ys))
               (class<=? (car xs) (car ys)))
           (loop (cdr xs) (cdr ys)))
          (#t
           #f))))

(define *class-graph-update-hook* ())

(define (make-class< sub super)
  "Defined <sub> to be a subclass of <super>."
  (validate-class-name sub super)
  (when (class<=? super sub)
    (error "Adding ~a as a subclass of ~a would introduce a circular inheritance path." sub super))
  (awhen (class-superclass sub)
         (info "Replacing the existing superclass of ~a, ~a, with ~a"
               sub it super))
  (invoke-hook '*class-graph-update-hook*)
  (hash-set! *class-graph* sub super))

(define (all-classes)
  "Returns a list of all currently defined classes in the class graph. These
   classes are returned in an order contstrained by the type graph: subclasses
   preceed superclasses"
  (let ((classes (make-hash :eq)))
    (dohash (sub super *class-graph* (qsort (hash-keys classes) class<=?))
            (hash-set! classes sub #t)
            (hash-set! classes super #t))))

(make-class< 'fixnum 'number)
(make-class< 'flonum 'number)
(make-class< 'complex 'flonum)
(make-class< 'subr 'procedure)
(make-class< 'closure 'procedure)
(make-class< 'nil 'cons)

(define (type-of obj)
  "Returns the type of <obj> as a symbol. For instances, the name of the type
   is the name of the prototype symbol closest up the prototype list. For
   structures, returns the structure type name."
  (cond ((structure? obj)
         (structure-type obj))
        (#t
         (%representation-of obj))))

