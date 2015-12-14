(use-package! "unit-test")

(define-test assq
  (check (runtime-error? (assq 'x 'x)))
  (check (runtime-error? (assq 'x '((1 . 2) (3 . 4) . x))))

  (let ((xs '((:test . :data)
              ("including" . "non-numeric")
              (() . "including-null")
              ((composite keys) . "are nice too"))))
    (check (not (assq :not-in-xs xs)))
    (check (eq? (first xs) (assq :test xs)))
    (check (not (assq "including" xs)))
    (check (eq? (third xs) (assq () xs)))
    (check (not (assq '(composite keys) xs)))
    (check (eq? (fourth xs) (assq (car (cadddr xs)) xs)))))
