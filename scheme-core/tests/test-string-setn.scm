(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-set!
  (test-case (runtime-error? (string-set! ts10a 1.0 #\c)))
  (test-case (runtime-error? (string-set! 12 1 #\c)))
  (test-case (runtime-error? (string-set! 'string-set-doesnt-support-symbols 1 #\c)))
  (test-case (runtime-error? (string-set! ts10a -1 #\a)))
  (test-case (runtime-error? (string-set! ts10a 20 #\a)))
  (test-case (runtime-error? (string-set! ts10a 'no-symbols-here-either)))
  (test-case (runtime-error? (string-set! ts10a "nope. no strings")))
  (test-case (runtime-error? (string-set! ts10a 12 "not here either.")))
  (test-case (runtime-error? (string-set! ts10a 3.14159))) 
  (test-case (equal? ts10a "0123456789")) ; ts10a should be unaltered
  
  (let ((ts-set (string-copy "foobar")))
    (test-case (equal? (string-set! (string-copy "*oobar") 0 #\f) ts-set))
    (test-case (equal? (string-set! (string-copy "*oobar") 0 102) ts-set))
    (test-case (equal? "*oobar" (string-set! ts-set 0 #\*)))
    (test-case (equal? "*oobar" ts-set))))
