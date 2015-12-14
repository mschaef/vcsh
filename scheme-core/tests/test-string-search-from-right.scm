(use-package! "unit-test")

(define-test string-search-from-right
  (check (equal? 2  (string-search-from-right "345" "1234567890")))
  (check (equal? #f (string-search-from-right "34567890a" "1234567890")))
  (check (equal? #f (string-search-from-right "hello world" "1234567890")))
  (check (equal? 0  (string-search-from-right "1" "1234567890")))
  (check (equal? 9  (string-search-from-right "0" "1234567890")))
  (check (equal? 6  (string-search-from-right "2" "1234512345")))
  (check (equal? #f (string-search-from-right "2" "1234512345" 0)))
  (check (equal? 1  (string-search-from-right "2" "1234512345" 4)))
  (check (equal? 1  (string-search-from-right "234" "1234512345" 4)))
  (check (equal? #f (string-search-from-right "234" "1234512345" 1)))
  (check (equal? 1  (string-search-from-right "234" "1234512345" 6)))
  (check (equal? #f (string-search-from-right "234" "1234512345" 60)))
  (check (equal? 6  (string-search-from-right "234" "1234512345" 8)))
  (check (equal? 12 (string-search-from-right "a" "abaabaaabaaaab")))
  (check (equal? 12 (string-search-from-right "ab" "abaabaaabaaaab")))
  (check (equal? 10 (string-search-from-right "aaa"  "abaabaaabaaaab")))
  (check (equal? 10 (string-search-from-right "aaab" "abaabaaabaaaab")))

  (check (equal? 1  (string-search-from-right "aaab" "aaaab")))
  (check (equal? #f  (string-search-from-right "aaab" "aaaab" 1)))
  (check (equal? #f  (string-search-from-right "aaab" "aaaab" 2)))

  (check (equal? 0  (string-search-from-right #\1 "1234567890")))
  (check (equal? 9  (string-search-from-right #\0 "1234567890")))
  (check (equal? 6  (string-search-from-right #\2 "1234512345")))
  (check (equal? #f (string-search-from-right #\2 "1234512345" 0)))
  (check (equal? 1  (string-search-from-right #\2 "1234512345" 4)))
  (check (equal? 12 (string-search-from-right #\a "abaabaaabaaaab"))))
