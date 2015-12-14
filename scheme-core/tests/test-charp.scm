(use-package! "unit-test")

(define-test char?
  (check (char? #\a))
  (check (char? #\b))
  (check (char? #\Z))
  (check (char? #\newline))
  (check (char? #\space))
  (check (char? #\;))
  (check (char? #\@))
  (check (char? #\#))
  (check (not (char? 1)))
  (check (not (char? 'symbol)))
  (check (not (char? #t)))
  (check (not (char? [1 2 3 4 5])))
  (check (not (char? (or #f 4)))))
  
