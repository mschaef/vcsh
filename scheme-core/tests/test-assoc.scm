(use-package! "unit-test")

(define-test assoc
  (check (runtime-error? (assoc 'x 'x)))
  (check (runtime-error? (assoc 'x '((1 . 2) (3 . 4) . x))))

  (let ((xs '((:test . :data)
              ("including" . "non-numeric")
              (() . "including-null")
              ((composite keys) . "are nice too"))))
    (check (not (assoc :not-in-xs xs)))
    (check (eq? (first xs) (assoc :test xs)))
    (check (eq? (second xs) (assoc "including" xs)))
    (check (eq? (third xs) (assoc () xs)))
    (check (eq? (fourth xs) (assoc '(composite keys) xs)))))
