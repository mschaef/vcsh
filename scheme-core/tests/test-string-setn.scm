(use-package! "unit-test")

(define ts10a "0123456789")
(define ts20a "01234567890123456789")
(define ts40a "0123456789012345678901234567890123456789")
(define ts10b "0123456789")
(define ts20b "01234567890123456789")
(define ts40b "0123456789012345678901234567890123456789")

(define-test string-set!
  (check (runtime-error? (string-set! ts10a 1.0 #\c)))
  (check (runtime-error? (string-set! 12 1 #\c)))
  (check (runtime-error? (string-set! 'string-set-doesnt-support-symbols 1 #\c)))
  (check (runtime-error? (string-set! ts10a -1 #\a)))
  (check (runtime-error? (string-set! ts10a 20 #\a)))
  (check (runtime-error? (string-set! ts10a 'no-symbols-here-either)))
  (check (runtime-error? (string-set! ts10a "nope. no strings")))
  (check (runtime-error? (string-set! ts10a 12 "not here either.")))
  (check (runtime-error? (string-set! ts10a 3.14159))) 
  (check (equal? ts10a "0123456789")) ; ts10a should be unaltered
  
  (let ((ts-set (string-copy "foobar")))
    (check (equal? (string-set! (string-copy "*oobar") 0 #\f) ts-set))
    (check (equal? (string-set! (string-copy "*oobar") 0 102) ts-set))
    (check (equal? "*oobar" (string-set! ts-set 0 #\*)))
    (check (equal? "*oobar" ts-set))))
