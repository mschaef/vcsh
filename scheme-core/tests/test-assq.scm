(use-package! "unit-test")

(define-test assq
  (test-case (runtime-error? (assq 'x 'x)))
  (test-case (runtime-error? (assq 'x '((1 . 2) (3 . 4) . x))))

  (let ((xs '((:test . :data)
              ("including" . "non-numeric")
              (() . "including-null")
              ((composite keys) . "are nice too"))))
    (test-case (not (assq :not-in-xs xs)))
    (test-case (eq? (first xs) (assq :test xs)))
    (test-case (not (assq "including" xs)))
    (test-case (eq? (third xs) (assq () xs)))
    (test-case (not (assq '(composite keys) xs)))
    (test-case (eq? (fourth xs) (assq (car (cadddr xs)) xs)))

    ))