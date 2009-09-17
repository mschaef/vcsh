(use-package! "unit-test")

(define-test flush-whitespace-string-input 
  (let ((ip (open-input-string "  123 4 ")))
    (test-case (char=? #\space (peek-char ip)))
    (test-case (char=? #\1 (flush-whitespace ip)))
    (test-case (char=? #\1 (flush-whitespace ip)))
    (test-case (char=? #\1 (peek-char ip)))
    (test-case (char=? #\1 (flush-whitespace ip)))
    (test-case (char=? #\1 (read-char ip)))
    (test-case (char=? #\2 (read-char ip)))
    (test-case (char=? #\3 (flush-whitespace ip)))
    (test-case (char=? #\3 (read-char ip)))
    (test-case (char=? #\4 (flush-whitespace ip)))
    (test-case (char=? #\4 (read-char ip)))
    (test-case (eof-object? (flush-whitespace ip))))

  (let ((ip (open-input-string "  123 4 ")))
    (test-case (char=? #\space (peek-char ip)))
    (test-case (char=? #\1 (flush-whitespace ip #f)))
    (test-case (char=? #\1 (flush-whitespace ip #f)))
    (test-case (char=? #\1 (peek-char ip)))
    (test-case (char=? #\1 (flush-whitespace ip #f)))
    (test-case (char=? #\1 (read-char ip)))
    (test-case (char=? #\2 (read-char ip)))
    (test-case (char=? #\3 (flush-whitespace ip #f)))
    (test-case (char=? #\3 (read-char ip)))
    (test-case (char=? #\4 (flush-whitespace ip #f)))
    (test-case (char=? #\4 (read-char ip)))
    (test-case (eof-object? (flush-whitespace ip #f))))


  (let ((ip (open-input-string "  123 4 ")))
    (test-case (char=? #\space (peek-char ip)))
    (test-case (char=? #\1 (flush-whitespace ip #t)))
    (test-case (char=? #\1 (flush-whitespace ip #t)))
    (test-case (char=? #\1 (peek-char ip)))
    (test-case (char=? #\1 (flush-whitespace ip #t)))
    (test-case (char=? #\1 (read-char ip)))
    (test-case (char=? #\2 (read-char ip)))
    (test-case (char=? #\3 (flush-whitespace ip #t)))
    (test-case (char=? #\3 (read-char ip)))
    (test-case (char=? #\4 (flush-whitespace ip #t)))
    (test-case (char=? #\4 (read-char ip)))
    (test-case (eof-object? (flush-whitespace ip #t))))

  (let ((ip (open-input-string " hello; cruel\n  world")))
    (test-case (char=? #\space (peek-char ip)))
    (test-case (char=? #\h (flush-whitespace ip #t)))
    (test-case (char=? #\h (flush-whitespace ip #t)))
    (test-case (char=? #\h (read-char ip)))
    (test-case (char=? #\e (flush-whitespace ip #t)))

    (test-case (char=? #\e (read-char ip)))
    (test-case (char=? #\l (read-char ip)))
    (test-case (char=? #\l (read-char ip)))
    (test-case (char=? #\o (read-char ip)))
    (test-case (char=? #\; (peek-char ip)))

    (test-case (char=? #\w (flush-whitespace ip #t)))
    (test-case (char=? #\w (flush-whitespace ip #t)))

    (test-case (char=? #\w (read-char ip)))
    (test-case (char=? #\o (read-char ip)))
    (test-case (char=? #\r (read-char ip)))
    (test-case (char=? #\l (read-char ip)))
    (test-case (char=? #\d (read-char ip))))


  (let ((ip (open-input-string " hello; cruel\n  world")))
    (test-case (char=? #\space (peek-char ip)))
    (test-case (char=? #\h (flush-whitespace ip #f)))
    (test-case (char=? #\h (flush-whitespace ip #f)))
    (test-case (char=? #\h (read-char ip)))
    (test-case (char=? #\e (flush-whitespace ip #f)))
    
    (test-case (char=? #\e (read-char ip)))
    (test-case (char=? #\l (read-char ip)))
    (test-case (char=? #\l (read-char ip)))
    (test-case (char=? #\o (read-char ip)))
    (test-case (char=? #\; (peek-char ip)))
    
    (test-case (char=? #\; (flush-whitespace ip #f)))
    (test-case (char=? #\; (flush-whitespace ip #f)))
    
    (test-case (char=? #\; (read-char ip)))
    (test-case (char=? #\space (read-char ip)))
    (test-case (char=? #\c (read-char ip)))
    (test-case (char=? #\r (read-char ip)))
    (test-case (char=? #\u (read-char ip)))
    (test-case (char=? #\e (read-char ip)))
    (test-case (char=? #\l (read-char ip)))
    
    (test-case (char=? #\w (flush-whitespace ip #f)))))




