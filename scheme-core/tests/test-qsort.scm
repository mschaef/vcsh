(use-package! "unit-test")

(define-test qsort

  (test-case (runtime-error? (qsort :not-a-list)))
  (test-case (runtime-error? (qsort (list 1 2 3))))
  (test-case (runtime-error? (qsort (list 1 2 3) :not-a-function)))
  (test-case (runtime-error? (qsort (list 1 2 3) < :not-a-function)))
  (test-case (runtime-error? (qsort (cons 1 (cons 2 3)) <)))


  (test-case (equal? () (qsort () <)))
  (test-case (equal? (list 1) (qsort (list 1) <)))
  (test-case (equal? (list 1 2 3) (qsort (list 1 2 3) <)))
  (test-case (equal? (list 1 1 2 3) (qsort (list 1 1 2 3) <)))
  (test-case (equal? (list 3 2 1) (qsort (list 1 2 3) >)))

  (let ((xs (list 54 49 75 69 11 25 66 64 28 18 73 67 42 9 31 61 2 91 81 80)))
    (test-case (equal? (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91)
                   (qsort xs <)))
    (test-case (equal? (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91)
                   (qsort (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91) <)))

    (test-case (equal? (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91)
                   (qsort  (list 91 81 80 75 73 69 67 66 64 61 54 49 42 31 28 25 18 11 9 2) <)))

    (test-case (equal? xs
                   (list 54 49 75 69 11 25 66 64 28 18 73 67 42 9 31 61 2 91 81 80))))

  (let ((xs (list 1 2 3)))
    (test-case (not (eq? xs (qsort xs <)))))

  (let ((xs (list "it" "was" "the" "best" "of" "times"
                  "it" "was" "the" "worst" "of" "times")))
    (test-case (equal? (list  "best" "it" "it" "of" "of" "the" "the"
                          "times" "times" "was" "was" "worst")
                   (qsort xs string<)))

    (test-case (every? #L(memq _ xs) (qsort xs string<))))
  
  (let ((xs (map cons (list "it" "was" "the" "best" "of" "times"
                            "it" "was" "the" "worst" "of" "times"))))
    (test-case (equal? (map cons (list  "best" "it" "it" "of" "of" "the" "the"
                                    "times" "times" "was" "was" "worst"))
                        (qsort xs string< car)))
    
    (test-case (every? #L(memq _ xs) (qsort xs string< car))))
    
  )
    
    


  
           