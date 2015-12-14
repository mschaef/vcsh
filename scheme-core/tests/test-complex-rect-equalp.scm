(use-package! "unit-test")

(define-test complex-rect-equal?
  (check (equal? (make-rectangular 3.0 4.0) (make-rectangular 3.0 4.0)))
  (check (not (equal? (make-rectangular 4.0 3.0) (make-rectangular 3.0 4.0))))
  (check (not (equal? (make-rectangular 3.0 4.0) (make-rectangular 3.0 5.0))))
  (check (not (equal? (make-rectangular 5.0 4.0) (make-rectangular 3.0 4.0))))
  (check (not (equal? 3.0 (make-rectangular 3.0 4.0))))
  (check (not (equal? 4.0 (make-rectangular 3.0 4.0))))
  (check (not (equal? 5.0 (make-rectangular 3.0 4.0))))
  (check (not (equal? 5   (make-rectangular 3.0 4.0)))))

;; TODO: test cases for magnitude, etc.
