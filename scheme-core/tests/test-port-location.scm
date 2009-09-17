(use-package! "unit-test")

(define (port-char-locations port char peek-count translate-mode)
  "Read characters from <port>, marking the location of instances of the
   character <char>  into a list.  Prior to each read-char, peek-char is
   called <peek-count> times. The port's translation mode is set to <translate-mode>.
   If peek-char returns a different value from read-char, a message to that effect
   is encoded in the return string."
  (define (loop char-locs)
    (let ((peek-char-value #f))
      (repeat peek-count 
	      (set! peek-char-value (peek-char port)))
      (let ((ch (read-char port)))
	(cond ((eof-object? ch)
	       (reverse char-locs))
	      ((and peek-char-value (not (eq? peek-char-value ch)))
	       (loop (cons (format #f "peek-char returned ~a, read-char ~a" peek-char-value ch)
			   char-locs)))
	      (#t
	       (loop (if (eq? ch char)
			 (cons (port-location port) char-locs)
			 char-locs)))))))
  (set-port-translate-mode! port translate-mode)
  (loop '()))

(define (string->input-port-char-locations string char peek-count translate-mode)
  "Invoke port-chars on an input string port created for <string>."
  (port-char-locations (open-input-string string) char peek-count translate-mode))

(define-test input-port-port-locations
  ;; Translate-mode=#t
  ;; peek-count=0
  (test-case (equal? (string->input-port-char-locations "  *  *   *\n*  *  ****\n*  *\n\n\n*   \n\n*   *" #\* 0 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))

  (test-case (equal? (string->input-port-char-locations "  *  *   *\r*  *  ****\r*  *\r\r\r*   \r\r*   *" #\* 0 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))

  (test-case (equal? (string->input-port-char-locations "  *  *   *\r\n*  *  ****\r\n*  *\r\n\r\n\r\n*   \r\n\r\n*   *" #\* 0 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))

  ;; Translate-mode=#t
  ;; peek-count=1
  (test-case (equal? (string->input-port-char-locations "  *  *   *\n*  *  ****\n*  *\n\n\n*   \n\n*   *" #\* 1 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))

  (test-case (equal? (string->input-port-char-locations "  *  *   *\r*  *  ****\r*  *\r\r\r*   \r\r*   *" #\* 1 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))

  (test-case (equal? (string->input-port-char-locations "  *  *   *\r\n*  *  ****\r\n*  *\r\n\r\n\r\n*   \r\n\r\n*   *" #\* 1 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))


  ;; Translate-mode=#t
  ;; peek-count=2
  (test-case (equal? (string->input-port-char-locations "  *  *   *\n*  *  ****\n*  *\n\n\n*   \n\n*   *" #\* 2 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))

  (test-case (equal? (string->input-port-char-locations "  *  *   *\r*  *  ****\r*  *\r\r\r*   \r\r*   *" #\* 2 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))

  (test-case (equal? (string->input-port-char-locations "  *  *   *\r\n*  *  ****\r\n*  *\r\n\r\n\r\n*   \r\n\r\n*   *" #\* 2 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5)))))

(define (->output-port-locations objs port)
  "Returns a list of the port-locations of port at the end of display'ing each
   object in <objs> to that port."
  (map #L(begin
           (display _ port)
           (port-location port))
       objs))

;; These tests use a string output port as a proxy for all output ports. This is currently
;; the best facsimile of a generic output port, since null ports do not manage port-locations.

(define-test output-port-port-locations
  (let ((port (open-output-string)))
    (set-port-translate-mode! port #f)
    (test-case (equal? (->output-port-locations '("" "foo" "bar" "\n" "foobar" "\ntest1\n") port)
                       '((1 . 0) (1 . 3) (1 . 6) (2 . 0) (2 . 6) (4 . 0)))))

  (let ((port (open-output-string)))
    (set-port-translate-mode! port #t)
    (test-case (equal? (->output-port-locations '("" "foo" "bar" "\n" "foobar" "\ntest1\n") port)
                       '((1 . 0) (1 . 3) (1 . 6) (2 . 0) (2 . 6) (4 . 0))))))

(define-test fresh-line
  (let ((raw-port (open-output-string)))
    (set-port-translate-mode! raw-port #f)

    (test-case (equal? (port-location raw-port) '(1 . 0)))
    (fresh-line raw-port)
    (test-case (equal? (port-location raw-port) '(1 . 0)))
    (fresh-line raw-port)
    (test-case (equal? (port-location raw-port) '(1 . 0)))
    (display "foo" raw-port)
    (fresh-line raw-port)
    (test-case (equal? (port-location raw-port) '(2 . 0))))

  (let ((translate-port (open-output-string)))
    (set-port-translate-mode! translate-port #t)

    (test-case (equal? (port-location translate-port) '(1 . 0)))
    (fresh-line translate-port)
    (test-case (equal? (port-location translate-port) '(1 . 0)))
    (fresh-line translate-port)
    (test-case (equal? (port-location translate-port) '(1 . 0)))
    (display "foo" translate-port)
    (fresh-line translate-port)
    (test-case (equal? (port-location translate-port) '(2 . 0))))
  )

