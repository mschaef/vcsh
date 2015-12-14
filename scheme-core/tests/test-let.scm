(use-package! "unit-test")

(define-test let
  (let (unbound-var)
    (check (equal? unbound-var '())))
  (let ((closure (let lp ((x 10)) lp)))
    (check (closure? closure)))
  (let ((x (let lp ((x2 10)) x2)))
    (check (equal? x 10)))

  (check
   (runtime-error?
    (let lp ((x 0) (y 10) z)
      (lp))))

  (let lp ((x 0) (y 10) z)
    (check (equal? z '())))

  (let ((side-effect-0 #f)
	(side-effect-1 #f)
	(side-effect-2 #f))
    (let ((x 0)
	  (y 1)
	  (z 2))
      (set! side-effect-0 #t)
      (check (= x 0))
      (check (= y 1))
      (check (= z 2))
      (set! side-effect-1 #t)
      (set! x 10)
      (incr! y)
      (check (= x 10))
      (check (= y 2))
      (check (= z 2))
      (set! side-effect-2 #t))
    (check side-effect-0)
    (check side-effect-1)
    (check side-effect-2))

  (let ((x 10) 
	(y 20))
    (let ((x 100)
	  (y x))
      (check (= x 100))
      (check (= y 10)))))
