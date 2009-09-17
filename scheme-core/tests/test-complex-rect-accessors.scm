(use-package! "unit-test")

(define-test complex-rect-accessors
  (test-case (equal? 3   (real-part 3  )))
  (test-case (equal? 0   (imag-part 3  )))
  (test-case (equal? 3.0 (real-part 3.0)))
  (test-case (equal? 0.0 (imag-part 3.0)))
  (test-case (equal? 3.0 (real-part (make-rectangular 3.0 4.0))))
  (test-case (equal? 4.0 (imag-part (make-rectangular 3.0 4.0)))))

