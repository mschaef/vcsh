
(defmacro (defbench benchname . code)
  `(set!  ,benchname (lambda ()
		       (let ((x #f))
			 ,@code
			 x))))

(defbench foo (set! x 12))