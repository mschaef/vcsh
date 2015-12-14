(use-package! "unit-test")

(define-test assv
  (check (runtime-error? (assv 'x 'x)))
  (check (runtime-error? (assv 'x '((1 . 2) (3 . 4) . x))))

  (let ((xs '((:test . :data)
              ("including" . "non-numeric")
              (() . "including-null")
              ((composite keys) . "are nice too"))))
    (check (not (assv :not-in-xs xs)))
    (check (eq? (first xs) (assv :test xs)))
    (check (not (assv "including" xs)))
    (check (eq? (third xs) (assv () xs)))
    (check (not (assv '(composite keys) xs)))
    (check (eq? (fourth xs) (assv (car (cadddr xs)) xs)))))
