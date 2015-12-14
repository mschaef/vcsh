(use-package! "unit-test")

(define-test multiple-read-from-string
  (let ((s "[1 2 3]"))
    (check (equal? [1 2 3] (read-from-string s)))
    (check (equal? [1 2 3] (read-from-string s)))))

(define-test read-line
  (let ((ip (open-input-string "")))
    (check (eof-object? (read-line ip))))

  (let ((ip (open-input-string "1234567")))
    (check (equal? "1234567" (read-line ip)))
    (check (eof-object? (read-line ip))))

  (let ((ip (open-input-string "\n")))
    (check (equal? "" (read-line ip)))
    (check (eof-object? (read-line ip))))

  (let ((ip (open-input-string "\n\n\n\n\n")))
    (check (equal? "" (read-line ip)))
    (check (equal? "" (read-line ip)))
    (check (equal? "" (read-line ip)))
    (check (equal? "" (read-line ip)))
    (check (equal? "" (read-line ip)))
    (check (eof-object? (read-line ip))))

  (let ((ip (open-input-string "123\n456\n789\n012\n345\n")))
    (check (equal? "123" (read-line ip)))
    (check (equal? "456" (read-line ip)))
    (check (equal? "789" (read-line ip)))
    (check (equal? "012" (read-line ip)))
    (check (equal? "345" (read-line ip)))
    (check (eof-object? (read-line ip)))))

