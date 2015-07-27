(use-package! "unit-test")

(define-test multiple-read-from-string
  (let ((s "[1 2 3]"))
    (test-case (equal? [1 2 3] (read-from-string s)))
    (test-case (equal? [1 2 3] (read-from-string s)))))

(define-test read-line
  (let ((ip (open-input-string "")))
    (test-case (eof-object? (read-line ip))))

  (let ((ip (open-input-string "1234567")))
    (test-case (equal? "1234567" (read-line ip)))
    (test-case (eof-object? (read-line ip))))

  (let ((ip (open-input-string "\n")))
    (test-case (equal? "" (read-line ip)))
    (test-case (eof-object? (read-line ip))))

  (let ((ip (open-input-string "\n\n\n\n\n")))
    (test-case (equal? "" (read-line ip)))
    (test-case (equal? "" (read-line ip)))
    (test-case (equal? "" (read-line ip)))
    (test-case (equal? "" (read-line ip)))
    (test-case (equal? "" (read-line ip)))
    (test-case (eof-object? (read-line ip))))

  (let ((ip (open-input-string "123\n456\n789\n012\n345\n")))
    (test-case (equal? "123" (read-line ip)))
    (test-case (equal? "456" (read-line ip)))
    (test-case (equal? "789" (read-line ip)))
    (test-case (equal? "012" (read-line ip)))
    (test-case (equal? "345" (read-line ip)))
    (test-case (eof-object? (read-line ip)))))

