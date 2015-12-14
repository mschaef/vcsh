(use-package! "unit-test")

(define-test apply-coverage
  (define (null-result) ())
  (define (atom-result) 1)
  (define (list-result) '(1 2 3 4 5))
  (define (identity xs) xs)
  (define (identity-rest . xs) xs)

  (check (null? (apply null-result '())))
  (check (eq? 1 (apply atom-result '())))
  (check (equal? '(1 2 3 4 5) (apply list-result '())))

  (check (runtime-error? (null? (apply identity '()))))
  (check (null? (apply identity-rest)))
  
  (check (equal? '1 (apply identity '(1))))
  (check (equal? '(1) (apply identity-rest '(1))))
  
  (check (equal? '1 (apply identity '(1 2))))
  (check (equal? '(1 2) (apply identity-rest '(1 2))))

  (check (equal? '(1 2 3) (apply identity-rest 1 '(2 3))))

  (check (equal? '(1 2 3) (apply identity-rest 1 2 '(3))))
  (check (equal? '(1 2 3) (apply identity-rest 1 2 3 '())))

  (check (runtime-error? (apply null-result 2)))
  (check (runtime-error? (apply identity-rest '(1 2 3 4 5 6 7 8 9 10 11 12
                                                    13 14 15 16 17 18 19 20 21
                                                    22 23 24 25 26 27 28 29 30
                                                    31 32 33)))))

  
