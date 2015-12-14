(use-package! "unit-test")

(define-test complex-rect-accessors
  (check (equal? 3   (real-part 3)))
  (check (equal? 0   (imag-part 3)))
  (check (equal? 3.0 (real-part 3.0)))
  (check (equal? 0.0 (imag-part 3.0)))
  (check (equal? 3.0 (real-part (make-rectangular 3.0 4.0))))
  (check (equal? 4.0 (imag-part (make-rectangular 3.0 4.0)))))

