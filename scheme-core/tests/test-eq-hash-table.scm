(use-package! "unit-test")

(define-test eq-hash-table

  (let ((h1 (make-identity-hash))
        (h2 (make-hash))
        (a '(1 2 3 4 5))
        (b '(1 2 3 4 5))
        (c '(1 2 3 4 5)))
    (hash-set! h1 a 1)
    (hash-set! h1 b 2)
    (hash-set! h1 c 3)
    
    (hash-set! h2 a 1)
    (hash-set! h2 b 2)
    (hash-set! h2 c 3)
    
    (check (eq? (hash-ref h1 a) 1))
    (check (eq? (hash-ref h1 b) 2))
    (check (eq? (hash-ref h1 c) 3))
    
    (check (eq? (hash-ref h2 a) 3))
    (check (eq? (hash-ref h2 b) 3))
    (check (eq? (hash-ref h2 c) 3))
    

    (hash-remove! h1 a)
    (check (eq? (hash-ref h1 a) #f))
    (check (eq? (hash-ref h1 b) 2))
    (check (eq? (hash-ref h1 c) 3))

    (hash-remove! h1 b)
    (check (eq? (hash-ref h1 a) #f))
    (check (eq? (hash-ref h1 b) #f))
    (check (eq? (hash-ref h1 c) 3))

    (hash-remove! h1 c)
    (check (eq? (hash-ref h1 a) #f))
    (check (eq? (hash-ref h1 b) #f))
    (check (eq? (hash-ref h1 c) #f))))
