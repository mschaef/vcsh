(ensure!-package! "compiler-gensym")

(define (compiler-gensym-reader port)
  (cond ((eq? #\: (peek-char port))
	 (with-package "compiler-gensym"
	   (read-char port)
	   (read port)))
	(#t
	 (error "bad reader syntax in compiled file."))))

