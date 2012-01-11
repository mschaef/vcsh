(use-package! "unit-test")

(define-test ass
  (define (fn-1-arg x) (equal? x :foo))
  (define (fn-2-args x y) (equal? x y))
  (define (fn-3-args x y z) (equal? x :foo))
  
  (test-case (runtime-error? (ass 'x 'x equal?)))
  (test-case (runtime-error? (ass 'x '((1 . 2) (3 . 4) . x) equal?)))
  (test-case (runtime-error? (ass 'x 'x fn-2-args)))
  (test-case (runtime-error? (ass 'x '((1 . 2) (3 . 4) . x) fn-2-args)))

  (let ((xs '((:test . :data)
              ("including" . "non-numeric")
              (() . "including-null")
              ((composite keys) . "are nice too"))))

    ;; (test-case (runtime-error? (ass :test xs fn-1-arg))) ; doesn't fault, just ignores the second argument
    ;; (test-case (runtime-error? (ass :test xs fn-3-args))) ; doesn't fault. Bad arity check?
    
    (test-case (not (ass :not-in-xs xs equal?)))
    (test-case (eq? (first xs) (ass :test xs equal?)))
    (test-case (eq? (second xs) (ass "including" xs equal?)))
    (test-case (eq? (third xs) (ass () xs equal?)))
    (test-case (eq? (fourth xs) (ass '(composite keys) xs equal?)))
    (test-case (not (ass :not-in-xs xs fn-2-args)))
    (test-case (eq? (first xs) (ass :test xs fn-2-args)))
    (test-case (eq? (second xs) (ass "including" xs fn-2-args)))
    (test-case (eq? (third xs) (ass () xs fn-2-args)))
    (test-case (eq? (fourth xs) (ass '(composite keys) xs fn-2-args))))
 )