(use-package! "unit-test")

(define-test assv
  (test-case (runtime-error? (assv 'x 'x)))
  (test-case (runtime-error? (assv 'x '((1 . 2) (3 . 4) . x))))

  (let ((xs '((:test . :data)
              ("including" . "non-numeric")
              (() . "including-null")
              ((composite keys) . "are nice too"))))
    (test-case (not (assv :not-in-xs xs)))
    (test-case (eq? (first xs) (assv :test xs)))
    (test-case (not (assv "including" xs)))
    (test-case (eq? (third xs) (assv () xs)))
    (test-case (not (assv '(composite keys) xs)))
    (test-case (eq? (fourth xs) (assv (car (cadddr xs)) xs)))

    ))