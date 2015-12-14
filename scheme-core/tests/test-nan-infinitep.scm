(use-package! "unit-test")

(define-test nan-infinite?
  (check (not (infinite? 0)))
  (check (not (infinite? #\a)))
  (check (not (infinite? 'symbol)))
  (check (not (infinite? '())))
  (check (not (infinite? '(1 2 3))))
  (check (not (infinite? 0.0)))

  (check (not (nan? 0)))
  (check (not (nan? #\a)))
  (check (not (nan? 'symbol)))
  (check (not (nan? '())))
  (check (not (nan? '(1 2 3))))
  (check (not (nan? 0.0)))

  (check (infinite? (/ 12 0)))
  (check (infinite? (/ -12 0)))

  (check (infinite? (make-rectangular (/ 12 0) 12)))
  (check (infinite? (make-rectangular (/ -12 0) 12)))
  (check (infinite? (make-rectangular 12 (/ 12 0))))
  (check (infinite? (make-rectangular 12 (/ -12 0))))
  (check (infinite? (make-rectangular (/ 12 0) (/ 12 0))))
  (check (infinite? (make-rectangular (/ 12 0) (/ -12 0))))
  (check (infinite? (make-rectangular (/ -12 0) (/ 12 0))))
  (check (infinite? (make-rectangular (/ -12 0) (/ -12 0))))

  (check (nan? (/ 0 0)))
  (check (nan? (/ 0 0)))
  (check (nan? (make-rectangular (/ 0 0) 12)))
  (check (nan? (make-rectangular 12 (/ 0 0))))
  (check (nan? (make-rectangular (/ 0 0) (/ 0 0)))))

