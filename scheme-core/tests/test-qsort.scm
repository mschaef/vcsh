(use-package! "unit-test")

(define-test qsort

  (check (runtime-error? (qsort :not-a-list)))
  (check (runtime-error? (qsort (list 1 2 3))))
  (check (runtime-error? (qsort (list 1 2 3) :not-a-function)))
  (check (runtime-error? (qsort (list 1 2 3) < :not-a-function)))
  (check (runtime-error? (qsort (cons 1 (cons 2 3)) <)))


  (check (equal? () (qsort () <)))
  (check (equal? (list 1) (qsort (list 1) <)))
  (check (equal? (list 1 2 3) (qsort (list 1 2 3) <)))
  (check (equal? (list 1 1 2 3) (qsort (list 1 1 2 3) <)))
  (check (equal? (list 3 2 1) (qsort (list 1 2 3) >)))

  (let ((xs (list 54 49 75 69 11 25 66 64 28 18 73 67 42 9 31 61 2 91 81 80)))
    (check (equal? (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91)
                   (qsort xs <)))
    (check (equal? (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91)
                   (qsort (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91) <)))

    (check (equal? (list 2 9 11 18 25 28 31 42 49 54 61 64 66 67 69 73 75 80 81 91)
                   (qsort  (list 91 81 80 75 73 69 67 66 64 61 54 49 42 31 28 25 18 11 9 2) <)))

    (check (equal? xs
                   (list 54 49 75 69 11 25 66 64 28 18 73 67 42 9 31 61 2 91 81 80))))

  (let ((xs (list 1 2 3)))
    (check (not (eq? xs (qsort xs <)))))

  (let ((xs (list "it" "was" "the" "best" "of" "times"
                  "it" "was" "the" "worst" "of" "times")))
    (check (equal? (list  "best" "it" "it" "of" "of" "the" "the"
                          "times" "times" "was" "was" "worst")
                   (qsort xs string<)))

    (check (every? #L(memq _ xs) (qsort xs string<))))
  
  (let ((xs (map cons (list "it" "was" "the" "best" "of" "times"
                            "it" "was" "the" "worst" "of" "times"))))
    (check (equal? (map cons (list  "best" "it" "it" "of" "of" "the" "the"
                                    "times" "times" "was" "was" "worst"))
                        (qsort xs string< car)))
    
    (check (every? #L(memq _ xs) (qsort xs string< car)))))
