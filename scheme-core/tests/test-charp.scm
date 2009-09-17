(use-package! "unit-test")

(define-test char?
  (test-case (char? #\a))
  (test-case (char? #\b))
  (test-case (char? #\Z))
  (test-case (char? #\newline))
  (test-case (char? #\space))
  (test-case (char? #\;))
  (test-case (char? #\@))
  (test-case (char? #\#))
  (test-case (not (char? 1)))
  (test-case (not (char? 'symbol)))
  (test-case (not (char? #t)))
  (test-case (not (char? #(1 2 3 4 5))))
  (test-case (not (char? (or #f 4)))))
  