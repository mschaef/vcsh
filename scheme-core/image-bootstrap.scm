;;; image-bootstrap.scm
;;;
;;; These are the initial definitions loaded into an image compile, 
;;; immediately after the constants and subrs are loaded. They're used
;;; to make it easier to boot up an otherwise empty image.
;;;
;;; Code in here has to follow a few rules:
;;; 1) The only definitions directly availble are subrs and constants.
;;;
;;; 2) It's okay to use host-scheme:: to get at the host scheme environment,
;;;    but macros here should not expand to host scheme macros. (Host scheme
;;;    macros will likely cause the image to references to host-* packages
;;;    that won't be available at image load time.)

(host-scheme::format (host-scheme::current-error-port) "; Loading bootstrap definitions\n")

(host-scheme::define (list . x) x)

(host-scheme::defmacro (%boot-dbind binding value . code)
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
     `(host-scheme::let ((,value-sym ,value))
        (host-scheme::let (,@(binding-forms binding value-sym))
          ,@code))))


(host-compiler::defmacro (%boot-defmacro lambda-list . macro-body) ;; TODO: Macros with :optional arguments
  (host-scheme::let ((macro-body macro-body) ; shadowed the argument to give us something settable.
                     (macro-name (car lambda-list))
                     (macro-formals (cdr lambda-list)))
    `(%define ,macro-name
              (%macro (%lambda () (form env)
                               (%boot-dbind ,macro-formals (cdr form)
                                 ,@macro-body))))))

(%boot-defmacro (%boot-named-lambda name l-list . code)
  (if name
      `(%lambda ((scheme::name . ,name)) ,l-list ,@code)
      `(%lambda () ,l-list ,@code)))

(%boot-defmacro (%boot-lambda arglist . code)
  `(%boot-named-lambda #f ,arglist ,@code))

(%boot-defmacro (%boot-define name . defn)
  (if (symbol? name)
      `(%define ,name ,(car defn))
      (if (and (pair? name) (symbol? (car name)))
          `(%define ,(car name)   (%boot-named-lambda ,(car name) ,(cdr name) ,@defn))
          (host-scheme::error "Invalid syntax for %boot-define: ~a" (cons name defn)))))


(host-scheme::format (host-scheme::current-error-port) "; Done loading bootstrap definitions\n")
