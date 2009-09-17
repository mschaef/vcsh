(use-package! "unit-test")

(define (port-chars port peek-count translate-mode)
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
    (set-port-translate-mode! port translate-mode)
    (loop '()))

(define (string->input-port-chars string peek-count translate-mode)
  "Invoke port-chars on an input string port created for <string>."
  (port-chars (open-input-string string) peek-count translate-mode))


(define-test input-port-translate-mode
  ;; Translate-mode=#f
  ;; peek-count=0
  (test-case (equal? (string->input-port-chars "\n\n\n\n" 0 #f)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\r\r\r\r" 0 #f)
                 '(#\cr #\cr #\cr #\cr)))
  (test-case (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 0 #f)
                 '(#\cr #\newline #\cr #\newline #\cr #\newline #\cr #\newline)))
  (test-case (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 0 #f)
                 '(#\newline #\cr #\newline #\cr #\newline #\cr #\newline #\cr)))

  (test-case (equal? (string->input-port-chars "*\n*\n*\n*\n*" 0 #f)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\r*\r*\r*\r*" 0 #f)
                 '(#\* #\cr #\* #\cr #\* #\cr #\* #\cr #\*)))
  (test-case (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 0 #f)
                 '(#\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 0 #f)
                 '(#\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\*)))

  ;; peek-count=1
  (test-case (equal? (string->input-port-chars "\n\n\n\n" 1 #f)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\r\r\r\r" 1 #f)
                 '(#\cr #\cr #\cr #\cr)))
  (test-case (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 1 #f)
                 '(#\cr #\newline #\cr #\newline #\cr #\newline #\cr #\newline)))
  (test-case (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 1 #f)
                 '(#\newline #\cr #\newline #\cr #\newline #\cr #\newline #\cr)))
  
  (test-case (equal? (string->input-port-chars "*\n*\n*\n*\n*" 1 #f)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\r*\r*\r*\r*" 1 #f)
                 '(#\* #\cr #\* #\cr #\* #\cr #\* #\cr #\*)))
  (test-case (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 1 #f)
                 '(#\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 1 #f)
                 '(#\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\*)))

  ;; peek-count=2
  (test-case (equal? (string->input-port-chars "\n\n\n\n" 2 #f)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\r\r\r\r" 2 #f)
                 '(#\cr #\cr #\cr #\cr)))
  (test-case (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 2 #f)
                 '(#\cr #\newline #\cr #\newline #\cr #\newline #\cr #\newline)))
  (test-case (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 2 #f)
                 '(#\newline #\cr #\newline #\cr #\newline #\cr #\newline #\cr)))
  
  (test-case (equal? (string->input-port-chars "*\n*\n*\n*\n*" 2 #f)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\r*\r*\r*\r*" 2 #f)
                 '(#\* #\cr #\* #\cr #\* #\cr #\* #\cr #\*)))
  (test-case (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 2 #f)
                 '(#\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 2 #f)
                 '(#\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\*)))
  
                                        ; Translate-mode=#t
  ;; peek-count=0
  (test-case (equal? (string->input-port-chars "\n\n\n\n" 0 #t)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\r\r\r\r" 0 #t)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 0 #t)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 0 #t)
                 '(#\newline #\newline #\newline #\newline #\newline)))
  
  (test-case (equal? (string->input-port-chars "*\n*\n*\n*\n*" 0 #t)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\r*\r*\r*\r*" 0 #t)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 0 #t)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 0 #t)
                 '(#\* #\newline #\newline #\* #\newline #\newline #\* #\newline #\newline #\* #\newline #\newline #\*)))

  ;; peek-count=1)
  (test-case (equal? (string->input-port-chars "\n\n\n\n" 1 #t)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\r\r\r\r" 1 #t)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 1 #t)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 1 #t)
                 '(#\newline #\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "*\n*\n*\n*\n*" 1 #t)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\r*\r*\r*\r*" 1 #t)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 1 #t)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 1 #t)
                 '(#\* #\newline #\newline #\* #\newline #\newline #\* #\newline #\newline #\* #\newline #\newline #\*)))

  ;; peek-count=2)
  (test-case (equal? (string->input-port-chars "\n\n\n\n" 2 #t)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\r\r\r\r" 2 #t)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 2 #t)
                 '(#\newline #\newline #\newline #\newline)))
  (test-case (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 2 #t)
                 '(#\newline #\newline #\newline #\newline #\newline))
         )
  (test-case (equal? (string->input-port-chars "*\n*\n*\n*\n*" 2 #t)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\r*\r*\r*\r*" 2 #t)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 2 #t)
                 '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (test-case (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 2 #t)
                 '(#\* #\newline #\newline #\* #\newline #\newline #\* #\newline #\newline #\* #\newline #\newline #\*))))
