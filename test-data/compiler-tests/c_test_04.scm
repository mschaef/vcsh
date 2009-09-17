(defmacro (aand . conditions)
  (cond ((null? conditions) 
	 #t)
	((null? (cdr conditions))
	 (car conditions))
	(#t
	 `(let ((it ,(car conditions)))
	    (if it (aand ,@(cdr conditions)) #f)))))

