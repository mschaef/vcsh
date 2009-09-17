(use-package! "unit-test")

(define-test key-list->hash
  (let ((h (key-list->hash () :eq #f)))
    (test-case (null? (hash-keys h)))
    (test-case (eq? :eq (hash-type h))))

  (let ((h (key-list->hash () :equal #f)))
    (test-case (null? (hash-keys h)))
    (test-case (eq? :equal (hash-type h))))

  (let ((h (key-list->hash '(1 2 3) :eq #f)))
    (test-case (length=3? (hash-keys h)))
    (test-case (hash-has? h 1))
    (test-case (hash-has? h 2))
    (test-case (hash-has? h 3))    
    (test-case (eq? #f (hash-ref h 1)))
    (test-case (eq? #f (hash-ref h 2)))
    (test-case (eq? #f (hash-ref h 3))))

  (let ((h (key-list->hash '(1 2 3) :equal #f)))
    (test-case (length=3? (hash-keys h)))
    (test-case (hash-has? h 1))
    (test-case (hash-has? h 2))
    (test-case (hash-has? h 3))    
    (test-case (eq? #f (hash-ref h 1)))
    (test-case (eq? #f (hash-ref h 2)))
    (test-case (eq? #f (hash-ref h 3))))

  (let ((h (key-list->hash '(1 2 3) :eq :my-funky-test-symbol)))
    (test-case (length=3? (hash-keys h)))
    (test-case (hash-has? h 1))
    (test-case (hash-has? h 2))
    (test-case (hash-has? h 3))    
    (test-case (eq? :my-funky-test-symbol (hash-ref h 1)))
    (test-case (eq? :my-funky-test-symbol (hash-ref h 2)))
    (test-case (eq? :my-funky-test-symbol (hash-ref h 3)))))

   
