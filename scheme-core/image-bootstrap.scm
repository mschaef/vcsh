
(host-scheme::format (host-scheme::current-error-port) "; Loading bootstrap definitions\n")

(host-scheme::define (list . x) x)

(host-scheme::defmacro (%sdbind binding value . code)
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


(host-compiler::defmacro (%sdefmacro lambda-list . macro-body) ;; TODO: Macros with :optional arguments
  (host-scheme::let ((macro-body macro-body) ; shadowed the argument to give us something settable.
                     (macro-name (car lambda-list))
                     (macro-formals (cdr lambda-list)))
    `(%define ,macro-name
              (%macro (%lambda () (form env)
                               (%sdbind ,macro-formals (cdr form)
                                 ,@macro-body))))))



(host-scheme::format (host-scheme::current-error-port) "; Done loading bootstrap definitions\n")
