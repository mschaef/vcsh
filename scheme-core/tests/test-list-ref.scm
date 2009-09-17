(use-package! "unit-test")

(define-test list-ref
  (test-case (eq? :a (list-ref '(:a :b :c :d) 0)))
  (test-case (eq? :b (list-ref '(:a :b :c :d) 1)))
  (test-case (eq? :c (list-ref '(:a :b :c :d) 2)))
  (test-case (eq? :d (list-ref '(:a :b :c :d) 3)))
  
  (test-case (runtime-error? (list-ref '() 0)))
  (test-case (runtime-error? (list-ref '() 1)))
  (test-case (runtime-error? (list-ref '(:a :b) 2)))
  (test-case (runtime-error? (list-ref '(:a :b) 10)))

  (test-case (runtime-error? (list-ref 'a 0)))
  (test-case (runtime-error? (list-ref 'a -1)))
  (test-case (not (runtime-error? (list-ref '(:a :b :c . :d) 0))))
  (test-case (not (runtime-error? (list-ref '(:a :b :c . :d) 1))))
  (test-case (not (runtime-error? (list-ref '(:a :b :c . :d) 2))))
  (test-case (runtime-error? (list-ref '(:a :b :c . :d) 3))))
