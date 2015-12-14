(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-comparison ; TODO: need string-comparison tests for case-sensitivity
  (check (runtime-error? (strcmp 123 "test")))
  (check (runtime-error? (strcmp "test" 123)))
  (check (runtime-error? (strcmp 456 123)))
  (check (eq? 0 (strcmp ts10a ts10a)))

  (check (> 0 (strcmp "alpha" "omega")))
  (check (< 0 (strcmp "omega" "alpha")))

  (check (> 0 (strcmp "alpha" "alphaz")))

  (check (eq? #t (string< "alpha" "omega")))
  (check (eq? #f (string< "alpha" "alpha")))
  (check (eq? #f (string< "omega" "alpha")))

  (check (eq? #t (string<= "alpha" "omega")))
  (check (eq? #t (string<= "alpha" "alpha")))
  (check (eq? #f (string<= "omega" "alpha")))

  (check (eq? #f (string= "alpha" "omega")))
  (check (eq? #t (string= "alpha" "alpha")))
  (check (eq? #f (string= "omega" "alpha")))

  (check (eq? #f (string> "alpha" "omega")))
  (check (eq? #f (string> "alpha" "alpha")))
  (check (eq? #t (string> "omega" "alpha")))

  (check (eq? #f (string>= "alpha" "omega")))
  (check (eq? #t (string>= "alpha" "alpha")))
  (check (eq? #t (string>= "omega" "alpha")))

  (check (eq? #t (string!= "alpha" "omega")))
  (check (eq? #f (string!= "alpha" "alpha")))
  (check (eq? #t (string!= "omega" "alpha"))))
