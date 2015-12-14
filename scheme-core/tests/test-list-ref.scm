(use-package! "unit-test")

(define-test list-ref
  (check (eq? :a (list-ref '(:a :b :c :d) 0)))
  (check (eq? :b (list-ref '(:a :b :c :d) 1)))
  (check (eq? :c (list-ref '(:a :b :c :d) 2)))
  (check (eq? :d (list-ref '(:a :b :c :d) 3)))
  
  (check (runtime-error? (list-ref '() 0)))
  (check (runtime-error? (list-ref '() 1)))
  (check (runtime-error? (list-ref '(:a :b) 2)))
  (check (runtime-error? (list-ref '(:a :b) 10)))

  (check (runtime-error? (list-ref 'a 0)))
  (check (runtime-error? (list-ref 'a -1)))
  (check (not (runtime-error? (list-ref '(:a :b :c . :d) 0))))
  (check (not (runtime-error? (list-ref '(:a :b :c . :d) 1))))
  (check (not (runtime-error? (list-ref '(:a :b :c . :d) 2))))
  (check (runtime-error? (list-ref '(:a :b :c . :d) 3))))
