
;;;; image-bootstrap.scm --
;;;;
;;;; Bootstrap definitions. CURRENTLY UNUSED.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.


;;;
;;; These are the initial definitions loaded into an image compile, 
;;; immediately after the constants and subrs are loaded. They're used
;;; to make it easier to boot up an otherwise empty image.
;;;
;;; Code in herxe has to follow a few rules:
;;; 1) The only definitions directly availble are subrs and constants.
;;;
;;; 2) It's okay to use host-scheme:: to get at the host scheme environment,
;;;    but macros here should not expand to host scheme macros. (Host scheme
;;;    macros will likely cause the image to references to host-* packages
;;;    that won't be available at image load time.)

(host-scheme::format (host-scheme::current-error-port) "; Loading bootstrap definitions\n")

(host-scheme::in-package! "scheme")

(host-scheme::define (list . x) x)

(host-scheme::defmacro (%early-named-lambda name l-list . code)
  (if name
      `(%lambda ((scheme::name . ,name)) ,l-list ,@code)
      `(%lambda () ,l-list ,@code)))

(host-scheme::defmacro (%early-lambda arglist . code)
  `(%early-named-lambda #f ,arglist ,@code))

(host-scheme::defmacro (%early-let variables . forms)
  (host-scheme::mvbind (variable-names variable-binding-forms) (host-scheme::parse-let-variables variables)
    `((%early-lambda ,variable-names ,@forms) ,@variable-binding-forms)))

(host-scheme::defmacro (%early-dbind binding value . code)
   (host-scheme::define (binding-forms binding value)
       (host-scheme::cond ((symbol? binding)
                           (list (list binding value)))
                          ((pair? binding)
                           (host-scheme::append (binding-forms (car binding) (list 'car value))
                                                (binding-forms (cdr binding) (list 'cdr value))))
                          ((null? binding)
                           ())
                          (#t
                           (host-scheme::error "Invalid dbind binding: ~s" binding)))) 
   (host-scheme::with-gensyms (value-sym)
     `(%early-let ((,value-sym ,value))
        (%early-let (,@(binding-forms binding value-sym))
          ,@code))))


(host-scheme::defmacro (%early-defmacro macro-lambda-list . macro-body) ;; REVISIT: macros with :optional arguments (possibly in dbind.)
  (host-scheme::let ((macro-body macro-body) ; shadowed the argument to give us something settable.
                     (macro-name (car macro-lambda-list))
                     (macro-formals (cdr macro-lambda-list)))
    `(%define ,macro-name
              (%macro (%lambda () (form env)
                               (%early-dbind ,macro-formals (cdr form)
                                 ,@macro-body))))))

(host-scheme::defmacro (%early-define name . defn)
  (if (symbol? name)
      `(%define ,name ,(car defn))
      (if (and (pair? name) (symbol? (car name)))
          `(%define ,(car name)   (%early-named-lambda ,(car name) ,(cdr name) ,@defn))
          (host-scheme::error "Invalid syntax for %early-define: ~a" (cons name defn)))))

(host-scheme::format (host-scheme::current-error-port) "; Done loading bootstrap definitions\n")
