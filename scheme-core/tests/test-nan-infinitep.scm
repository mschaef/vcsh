(use-package! "unit-test")

(define-test nan-infinite?
  (test-case (not (infinite? 0)))
  (test-case (not (infinite? #\a)))
  (test-case (not (infinite? 'symbol)))
  (test-case (not (infinite? '())))
  (test-case (not (infinite? '(1 2 3))))
  (test-case (not (infinite? 0.0)))

  (test-case (not (nan? 0)))
  (test-case (not (nan? #\a)))
  (test-case (not (nan? 'symbol)))
  (test-case (not (nan? '())))
  (test-case (not (nan? '(1 2 3))))
  (test-case (not (nan? 0.0)))

  (test-case (infinite? (/ 12 0)))
  (test-case (infinite? (/ -12 0)))

  (test-case (infinite? (make-rectangular (/ 12 0) 12)))
  (test-case (infinite? (make-rectangular (/ -12 0) 12)))
  (test-case (infinite? (make-rectangular 12 (/ 12 0))))
  (test-case (infinite? (make-rectangular 12 (/ -12 0))))
  (test-case (infinite? (make-rectangular (/ 12 0) (/ 12 0))))
  (test-case (infinite? (make-rectangular (/ 12 0) (/ -12 0))))
  (test-case (infinite? (make-rectangular (/ -12 0) (/ 12 0))))
  (test-case (infinite? (make-rectangular (/ -12 0) (/ -12 0))))

  (test-case (nan? (/ 0 0)))
  (test-case (nan? (/ 0 0)))
  (test-case (nan? (make-rectangular (/ 0 0) 12)))
  (test-case (nan? (make-rectangular 12 (/ 0 0))))
  (test-case (nan? (make-rectangular (/ 0 0) (/ 0 0)))))

