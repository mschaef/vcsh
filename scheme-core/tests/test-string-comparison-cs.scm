(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-comparison ; TODO: need string-comparison tests for case-sensitivity
  (test-case (runtime-error? (strcmp 123 "test")))
  (test-case (runtime-error? (strcmp "test" 123)))
  (test-case (runtime-error? (strcmp 456 123)))
  (test-case (eq? 0 (strcmp ts10a ts10a)))

  (test-case (> 0 (strcmp "alpha" "omega")))
  (test-case (< 0 (strcmp "omega" "alpha")))

  (test-case (> 0 (strcmp "alpha" "alphaz")))

  (test-case (eq? #t (string< "alpha" "omega")))
  (test-case (eq? #f (string< "alpha" "alpha")))
  (test-case (eq? #f (string< "omega" "alpha")))

  (test-case (eq? #t (string<= "alpha" "omega")))
  (test-case (eq? #t (string<= "alpha" "alpha")))
  (test-case (eq? #f (string<= "omega" "alpha")))

  (test-case (eq? #f (string= "alpha" "omega")))
  (test-case (eq? #t (string= "alpha" "alpha")))
  (test-case (eq? #f (string= "omega" "alpha")))

  (test-case (eq? #f (string> "alpha" "omega")))
  (test-case (eq? #f (string> "alpha" "alpha")))
  (test-case (eq? #t (string> "omega" "alpha")))

  (test-case (eq? #f (string>= "alpha" "omega")))
  (test-case (eq? #t (string>= "alpha" "alpha")))
  (test-case (eq? #t (string>= "omega" "alpha")))

  (test-case (eq? #t (string!= "alpha" "omega")))
  (test-case (eq? #f (string!= "alpha" "alpha")))
  (test-case (eq? #t (string!= "omega" "alpha"))))
