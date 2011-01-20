
(format (current-error-port) "; Loading bootstrap definitions\n")

;; (%define %sdefmacro
;;          (%macro
;;           (%lambda () (form env)
;;                    `(%define ,(car (car (cdr form)))
;;                              (%macro (%lambda () (form env)
;;                                               (dbind ,(cdr (car (cdr form))) (cdr form)
;;                                                 ,@(cdr (cdr form)))))))))


;; (host-compiler::defmacro (%sdefmacro lambda-list . macro-body) ;; TODO: Macros with :optional arguments

;;   (host-scheme::let ((macro-body macro-body) ; shadowed the argument to give us something settable.
;;                      (macro-name (host-scheme::car lambda-list))
;;                      (macro-formals (host-scheme::cdr lambda-list)))

;;     `(%define ,macro-name
;;               (%macro (%lambda () (form env)
;;                                (dbind ,macro-formals (cdr form)
;;                                  ,@macro-body))))))

;; (host-compiler::defmacro (%sdefmacro macro-name form-var . macro-body) ;; TODO: Macros with :optional arguments

;;   (host-scheme::let ((macro-body macro-body) ; shadowed the argument to give us something settable.
;;                      (macro-name (host-scheme::car lambda-list))
;;                      (macro-formals (host-scheme::cdr lambda-list)))

;;     `(%define ,macro-name
;;               (%macro (%lambda () (,form-var)
;;                                (dbind ,macro-formals (cdr form)
;;                                  ,@macro-body))))))