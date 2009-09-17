(use-package! "unit-test")

(define-test format
  (test-case (equal? (format #f "~s" 1) "1"))
  (test-case (equal? (format #f "~s" "hello world") "\"hello world\""))
  (test-case (equal? (format #f "~a" "hello world") "hello world"))
  (test-case (equal? (format #f "~S" "~s" 1) "1"))
  (test-case (equal? (format #f "~S" "~s" "hello world") "\"hello world\""))
  (test-case (equal? (format #f "~S" "~a" "hello world") "hello world"))
  (test-case (equal? (format #f "~S:~S" "~sa~s" 1 2 "~sb~s" 3 4) "1a2:3b4"))
  (test-case (equal? (format #f "~S" "~sa~s" 1 2) "1a2"))
  (test-case (equal? (format #f "~I" "~s ~s" '(1 2)) "1 2"))
  (test-case (equal? (format #f "~I ~I" "~s ~s" '(1 2) "~s ~s" '(3 4)) "1 2 3 4")))
