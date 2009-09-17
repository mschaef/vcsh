
(defmacro (define2 name . defn)
  (cond ((symbol? name) 
	 `(%define ,name ,@defn))
	((and (list? name) (symbol? (car name)))
	 `(%define ,(car name) (%lambda ((name . ,(car name))) ,(cdr name) ,@defn)))
	(#t
	 (error "bad definition"))))

