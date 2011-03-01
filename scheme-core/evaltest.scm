
;;;; evaltest.scm --
;;;;
;;;; Random form generator - Randomly generates complex forms and
;;;; closures that evaluate to a particular value.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define *value-forms* '())

(defmacro (define-value-form name . code)
  `(a-list-set! *value-forms* ',name (lambda (val make-subform) ,@code) ))

(define (form-with-value val calls-left?)
  (if (= calls-left? 0)
      val
      (let ((generator (cdr (random-list-element *value-forms*))))
        (generator val (lambda (val) (form-with-value val (- calls-left? 1)))))))

(define (closure-with-value val calls-left?)
  (eval `(lambda () ,(form-with-value val calls-left?))))

(define-value-form addition
  (let ((offset (- (random 2000) 1000)))
    `(+ ,(make-subform (- val offset))
        ,(make-subform offset))))

(define-value-form double-negation
  `(- (- ,(make-subform val))))

(define-value-form unwind-protect
  `(unwind-protect (lambda () ,(make-subform val)) (lambda () #f)))

(define-value-form subtraction
  (let ((offset (- (random 2000) 1000)))
    `(- ,(make-subform (+ val offset))
        ,(make-subform offset))))

(define-value-form random-2-way-if
  `(if (= 0 (random 2))
       ,(make-subform val)
       ,(make-subform val)))

(define-value-form if-true
  `(if #t ,(make-subform val)))

(define-value-form if-false
  `(if #f #f ,(make-subform val)))

(define-value-form simple-lambda
  `((lambda () ,(make-subform val))))

(define-value-form simple-lambda-via-argument
  `((lambda (x) x) ,(make-subform val)))

(define-value-form simple-lambda-via-argument/apply
  `(apply (lambda (x) x) (cons ,(make-subform val))))

(define-value-form let-3-branches
  `(let ((x1 ,(make-subform val))
         (x2 ,(make-subform val))
         (x3 ,(make-subform val)))
     (+ x1 (- x2) x3 (- x1 x2))))

(define (test-forms forms-to-test repeat-count nesting-level)
  (repeat forms-to-test
	  (let ((target-value (- (random 100000) 50000)))
	    (let ((form (form-with-value target-value nesting-level)))
	      (repeat repeat-count
		      (unless (= (eval form) target-value)
			(error "This form ~a should've evaluted to this: ~a" form target-value)))))))

(define (test)
  (test-forms 50000 50 18))

