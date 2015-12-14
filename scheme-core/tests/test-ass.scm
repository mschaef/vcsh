(use-package! "unit-test")

(define-test ass
  (define (fn-1-arg x) (equal? x :foo))
  (define (fn-2-args x y) (equal? x y))
  (define (fn-3-args x y z) (equal? x :foo))
  
  (check (runtime-error? (ass 'x 'x equal?)))
  (check (runtime-error? (ass 'x '((1 . 2) (3 . 4) . x) equal?)))
  (check (runtime-error? (ass 'x 'x fn-2-args)))
  (check (runtime-error? (ass 'x '((1 . 2) (3 . 4) . x) fn-2-args)))

  (let ((xs '((:test . :data)
              ("including" . "non-numeric")
              (() . "including-null")
              ((composite keys) . "are nice too"))))

    ;; (check (runtime-error? (ass :test xs fn-1-arg))) ; doesn't fault, just ignores the second argument
    ;; (check (runtime-error? (ass :test xs fn-3-args))) ; doesn't fault. Bad arity check?
    
    (check (not (ass :not-in-xs xs equal?)))
    (check (eq? (first xs) (ass :test xs equal?)))
    (check (eq? (second xs) (ass "including" xs equal?)))
    (check (eq? (third xs) (ass () xs equal?)))
    (check (eq? (fourth xs) (ass '(composite keys) xs equal?)))
    (check (not (ass :not-in-xs xs fn-2-args)))
    (check (eq? (first xs) (ass :test xs fn-2-args)))
    (check (eq? (second xs) (ass "including" xs fn-2-args)))
    (check (eq? (third xs) (ass () xs fn-2-args)))
    (check (eq? (fourth xs) (ass '(composite keys) xs fn-2-args)))))
