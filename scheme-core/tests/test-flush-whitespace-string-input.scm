(use-package! "unit-test")

(define-test flush-whitespace-string-input 
  (let ((ip (open-input-string "  123 4 ")))
    (check (char=? #\space (peek-char ip)))
    (check (char=? #\1 (flush-whitespace ip)))
    (check (char=? #\1 (flush-whitespace ip)))
    (check (char=? #\1 (peek-char ip)))
    (check (char=? #\1 (flush-whitespace ip)))
    (check (char=? #\1 (read-char ip)))
    (check (char=? #\2 (read-char ip)))
    (check (char=? #\3 (flush-whitespace ip)))
    (check (char=? #\3 (read-char ip)))
    (check (char=? #\4 (flush-whitespace ip)))
    (check (char=? #\4 (read-char ip)))
    (check (eof-object? (flush-whitespace ip))))

  (let ((ip (open-input-string "  123 4 ")))
    (check (char=? #\space (peek-char ip)))
    (check (char=? #\1 (flush-whitespace ip #f)))
    (check (char=? #\1 (flush-whitespace ip #f)))
    (check (char=? #\1 (peek-char ip)))
    (check (char=? #\1 (flush-whitespace ip #f)))
    (check (char=? #\1 (read-char ip)))
    (check (char=? #\2 (read-char ip)))
    (check (char=? #\3 (flush-whitespace ip #f)))
    (check (char=? #\3 (read-char ip)))
    (check (char=? #\4 (flush-whitespace ip #f)))
    (check (char=? #\4 (read-char ip)))
    (check (eof-object? (flush-whitespace ip #f))))


  (let ((ip (open-input-string "  123 4 ")))
    (check (char=? #\space (peek-char ip)))
    (check (char=? #\1 (flush-whitespace ip #t)))
    (check (char=? #\1 (flush-whitespace ip #t)))
    (check (char=? #\1 (peek-char ip)))
    (check (char=? #\1 (flush-whitespace ip #t)))
    (check (char=? #\1 (read-char ip)))
    (check (char=? #\2 (read-char ip)))
    (check (char=? #\3 (flush-whitespace ip #t)))
    (check (char=? #\3 (read-char ip)))
    (check (char=? #\4 (flush-whitespace ip #t)))
    (check (char=? #\4 (read-char ip)))
    (check (eof-object? (flush-whitespace ip #t))))

  (let ((ip (open-input-string " hello; cruel\n  world")))
    (check (char=? #\space (peek-char ip)))
    (check (char=? #\h (flush-whitespace ip #t)))
    (check (char=? #\h (flush-whitespace ip #t)))
    (check (char=? #\h (read-char ip)))
    (check (char=? #\e (flush-whitespace ip #t)))

    (check (char=? #\e (read-char ip)))
    (check (char=? #\l (read-char ip)))
    (check (char=? #\l (read-char ip)))
    (check (char=? #\o (read-char ip)))
    (check (char=? #\; (peek-char ip)))

    (check (char=? #\w (flush-whitespace ip #t)))
    (check (char=? #\w (flush-whitespace ip #t)))

    (check (char=? #\w (read-char ip)))
    (check (char=? #\o (read-char ip)))
    (check (char=? #\r (read-char ip)))
    (check (char=? #\l (read-char ip)))
    (check (char=? #\d (read-char ip))))


  (let ((ip (open-input-string " hello; cruel\n  world")))
    (check (char=? #\space (peek-char ip)))
    (check (char=? #\h (flush-whitespace ip #f)))
    (check (char=? #\h (flush-whitespace ip #f)))
    (check (char=? #\h (read-char ip)))
    (check (char=? #\e (flush-whitespace ip #f)))
    
    (check (char=? #\e (read-char ip)))
    (check (char=? #\l (read-char ip)))
    (check (char=? #\l (read-char ip)))
    (check (char=? #\o (read-char ip)))
    (check (char=? #\; (peek-char ip)))
    
    (check (char=? #\; (flush-whitespace ip #f)))
    (check (char=? #\; (flush-whitespace ip #f)))
    
    (check (char=? #\; (read-char ip)))
    (check (char=? #\space (read-char ip)))
    (check (char=? #\c (read-char ip)))
    (check (char=? #\r (read-char ip)))
    (check (char=? #\u (read-char ip)))
    (check (char=? #\e (read-char ip)))
    (check (char=? #\l (read-char ip)))
    
    (check (char=? #\w (flush-whitespace ip #f)))))




