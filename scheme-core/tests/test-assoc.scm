(use-package! "unit-test")

(define-test assoc
  (test-case (runtime-error? (assoc 'x 'x)))
  (test-case (runtime-error? (assoc 'x '((1 . 2) (3 . 4) . x))))

  (let ((xs '((:test . :data)
              ("including" . "non-numeric")
              (() . "including-null")
              ((composite keys) . "are nice too"))))
    (test-case (not (assoc :not-in-xs xs)))
    (test-case (eq? (first xs) (assoc :test xs)))
    (test-case (eq? (second xs) (assoc "including" xs)))
    (test-case (eq? (third xs) (assoc () xs)))
    (test-case (eq? (fourth xs) (assoc '(composite keys) xs)))))