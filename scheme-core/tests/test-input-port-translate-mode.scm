(use-package! "unit-test")

(define (port-chars port peek-count)
  "Read characters from <port> into a list.  Prior to each read-char,
   peek-char is called <peek-count> times. The port's translation mode
   is set to <translate-mode>. If peek-char returns a different value
   from read-char, a message to that effect is encoded in the return
   list."
  (define (loop chars)
    (let ((peek-char-value #f))
      (repeat peek-count 
	      (set! peek-char-value (peek-char port)))
      (let ((ch (read-char port)))
	(cond ((eof-object? ch)
	       (reverse chars))
	      ((and peek-char-value (not (eq? peek-char-value ch)))
	       (loop (cons (format #f "peek-char returned ~a, read-char ~a" peek-char-value ch)
			   chars)))
	      (#t
	       (loop (cons ch chars)))))))
    (loop '()))

(define (string->input-port-chars string peek-count)
  "Invoke port-chars on an input string port created for <string>."
  (port-chars (open-input-string string) peek-count))


(define-test input-port-translate-mode
  ;; Translate-mode=#f
  ;; peek-count=0
  (test-case
   (equal? (string->input-port-chars "\n\n\n\n" 0)
           '(#\newline #\newline #\newline #\newline)))
  (test-case
   (equal? (string->input-port-chars "\r\r\r\r" 0)
           '(#\cr #\cr #\cr #\cr)))
  (test-case
   (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 0)
           '(#\cr #\newline #\cr #\newline #\cr #\newline #\cr #\newline)))
  (test-case
   (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 0)
           '(#\newline #\cr #\newline #\cr #\newline #\cr #\newline #\cr)))

  (test-case
   (equal? (string->input-port-chars "*\n*\n*\n*\n*" 0)
           '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case
   (equal? (string->input-port-chars "*\r*\r*\r*\r*" 0)
           '(#\* #\cr #\* #\cr #\* #\cr #\* #\cr #\*)))
  (test-case
   (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 0)
           '(#\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\*)))
  (test-case
   (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 0)
           '(#\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\*)))

  ;; peek-count=1
  (test-case
   (equal? (string->input-port-chars "\n\n\n\n" 1)
           '(#\newline #\newline #\newline #\newline)))
  (test-case
   (equal? (string->input-port-chars "\r\r\r\r" 1)
           '(#\cr #\cr #\cr #\cr)))
  (test-case
   (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 1)
           '(#\cr #\newline #\cr #\newline #\cr #\newline #\cr #\newline)))
  (test-case
   (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 1)
           '(#\newline #\cr #\newline #\cr #\newline #\cr #\newline #\cr)))
  
  (test-case
   (equal? (string->input-port-chars "*\n*\n*\n*\n*" 1)
           '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case
   (equal? (string->input-port-chars "*\r*\r*\r*\r*" 1)
           '(#\* #\cr #\* #\cr #\* #\cr #\* #\cr #\*)))
  (test-case
   (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 1)
           '(#\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\*)))
  (test-case
   (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 1)
           '(#\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\*)))

  ;; peek-count=2
  (test-case
   (equal? (string->input-port-chars "\n\n\n\n" 2)
           '(#\newline #\newline #\newline #\newline)))
  (test-case
   (equal? (string->input-port-chars "\r\r\r\r" 2)
           '(#\cr #\cr #\cr #\cr)))
  (test-case
   (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 2)
           '(#\cr #\newline #\cr #\newline #\cr #\newline #\cr #\newline)))
  (test-case
   (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 2)
           '(#\newline #\cr #\newline #\cr #\newline #\cr #\newline #\cr)))
  
  (test-case
   (equal? (string->input-port-chars "*\n*\n*\n*\n*" 2)
           '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case
   (equal? (string->input-port-chars "*\r*\r*\r*\r*" 2)
           '(#\* #\cr #\* #\cr #\* #\cr #\* #\cr #\*)))
  (test-case
   (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 2)
           '(#\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\*)))
  (test-case
   (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 2)
           '(#\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\*))))
