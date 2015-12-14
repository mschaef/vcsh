(define-package "test-io"
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

(define-test binary-integer-io
  (let ((test-filename (temporary-file-name "sct")))
    (with-port p (open-file test-filename :mode :write :encoding :binary)
      (check (not (runtime-error? (write-binary-fixnum-s8 -128 p)))) ; min - 1s
      (check (not (runtime-error? (write-binary-fixnum-s8 -34 p)))) ; -n - 1s
      (check (not (runtime-error? (write-binary-fixnum-s8 -2 p)))) ; -2 - 1s
      (check (not (runtime-error? (write-binary-fixnum-s8 -1 p)))) ; -1 - 1s
      (check (not (runtime-error? (write-binary-fixnum-s8 0 p)))) ; 0 - 1s
      (check (not (runtime-error? (write-binary-fixnum-s8 1 p)))) ; 1 - 1s
      (check (not (runtime-error? (write-binary-fixnum-s8 2 p)))) ; 2 - 1s
      (check (not (runtime-error? (write-binary-fixnum-s8 34 p)))) ; n - 1s
      (check (not (runtime-error? (write-binary-fixnum-s8 127 p)))) ; max-s - 1s
      (check (not (runtime-error? (write-binary-fixnum-u8 0 p)))) ; 0 - 1u
      (check (not (runtime-error? (write-binary-fixnum-u8 1 p)))) ; 1 - 1u
      (check (not (runtime-error? (write-binary-fixnum-u8 2 p)))) ; 2 - 1u
      (check (not (runtime-error? (write-binary-fixnum-u8 34 p)))) ; n - 1u
      (check (not (runtime-error? (write-binary-fixnum-u8 127 p)))) ; max-s - 1u
      (check (not (runtime-error? (write-binary-fixnum-u8 130 p)))) ; >max-s - 1u
      (check (not (runtime-error? (write-binary-fixnum-u8 255 p)))) ; max-u - 1u
      (check (not (runtime-error? (write-binary-fixnum-s16 -32768 p)))) ; min - 2s
      (check (not (runtime-error? (write-binary-fixnum-s16 -18273 p)))) ; -n - 2s
      (check (not (runtime-error? (write-binary-fixnum-s16 -2 p)))) ; -2 - 2s
      (check (not (runtime-error? (write-binary-fixnum-s16 -1 p)))) ; -1 - 2s
      (check (not (runtime-error? (write-binary-fixnum-s16 0 p)))) ; 0 - 2s
      (check (not (runtime-error? (write-binary-fixnum-s16 1 p)))) ; 1 - 2s
      (check (not (runtime-error? (write-binary-fixnum-s16 2 p)))) ; 2 - 2s
      (check (not (runtime-error? (write-binary-fixnum-s16 12726 p)))) ; n - 2s
      (check (not (runtime-error? (write-binary-fixnum-s16 32767 p)))) ; max-s - 2s
      (check (not (runtime-error? (write-binary-fixnum-u16 0 p)))) ; 0 - 2u
      (check (not (runtime-error? (write-binary-fixnum-u16 1 p)))) ; 1 - 2u
      (check (not (runtime-error? (write-binary-fixnum-u16 2 p)))) ; 2 - 2u
      (check (not (runtime-error? (write-binary-fixnum-u16 12726 p)))) ; n - 2u
      (check (not (runtime-error? (write-binary-fixnum-u16 32767 p)))) ; max-s - 2u
      (check (not (runtime-error? (write-binary-fixnum-u16 35000 p)))) ; >max-s - 2u
      (check (not (runtime-error? (write-binary-fixnum-u16 65535 p)))) ; max-u - 2u
      (check (not (runtime-error? (write-binary-fixnum-s32 -2147483648 p)))) ; min - 4s
      (check (not (runtime-error? (write-binary-fixnum-s32 -23443555 p)))) ; -n - 4s
      (check (not (runtime-error? (write-binary-fixnum-s32 -2 p)))) ; -2 - 4s
      (check (not (runtime-error? (write-binary-fixnum-s32 -1 p)))) ; -1 - 4s
      (check (not (runtime-error? (write-binary-fixnum-s32 0 p)))) ; 0 - 4s
      (check (not (runtime-error? (write-binary-fixnum-s32 1 p)))) ; 1 - 4s
      (check (not (runtime-error? (write-binary-fixnum-s32 2 p)))) ; 2 - 4s
      (check (not (runtime-error? (write-binary-fixnum-s32 34548374 p)))) ; n - 4s
      (check (not (runtime-error? (write-binary-fixnum-s32 2147483647 p)))) ; max-s - 4s
      (check (not (runtime-error? (write-binary-fixnum-u32 0 p)))) ; 0 - 4u
      (check (not (runtime-error? (write-binary-fixnum-u32 1 p)))) ; 1 - 4u
      (check (not (runtime-error? (write-binary-fixnum-u32 2 p)))) ; 2 - 4u
      (check (not (runtime-error? (write-binary-fixnum-u32 34548374 p)))) ; n - 4u
      (check (not (runtime-error? (write-binary-fixnum-u32 2147483647 p)))) ; max-s - 4u
      (check (not (runtime-error? (write-binary-fixnum-u32 2500000000 p)))) ; >max-s - 4u
      (check (not (runtime-error? (write-binary-fixnum-u32 4294967295 p))))) ; max-u - 4u
    (check (file-exists? test-filename))
    (with-port p (open-file test-filename :mode :read :encoding :binary)
      (let ((binary-string (read-binary-string 1000 p)))
        (check (= 112 (length binary-string)))
        (check (equal? binary-string "\200\336\376\377\000\001\002\"\177\000\001\002\"\177\202\377\200\000\270\237\377\376\377\377\000\000\000\001\000\0021\266\177\377\000\000\000\001\000\0021\266\177\377\210\270\377\377\200\000\000\000\376\232G\235\377\377\377\376\377\377\377\377\000\000\000\000\000\000\000\001\000\000\000\002\002\017*\226\177\377\377\377\000\000\000\000\000\000\000\001\000\000\000\002\002\017*\226\177\377\377\377\225\002\371\000\377\377\377\377")))
      (check (eof-object? (read-binary-string 1 p))))
    (with-port p (open-file test-filename :mode :read :encoding :binary)
      ;; exception on write ; <min - 1s
      (check (equal? (read-binary-fixnum-s8 p)  -128)); min - 1s
      (check (equal? (read-binary-fixnum-s8 p)  -34)); -n - 1s
      (check (equal? (read-binary-fixnum-s8 p)  -2)); -2 - 1s
      (check (equal? (read-binary-fixnum-s8 p)  -1)); -1 - 1s
      (check (equal? (read-binary-fixnum-s8 p)  0)); 0 - 1s
      (check (equal? (read-binary-fixnum-s8 p)  1)); 1 - 1s
      (check (equal? (read-binary-fixnum-s8 p)  2)); 2 - 1s
      (check (equal? (read-binary-fixnum-s8 p)  34)); n - 1s
      (check (equal? (read-binary-fixnum-s8 p)  127)); max-s - 1s
      ;; exception on write ; >max-s - 1s
      ;; exception on write ; max-u - 1s
      ;; exception on write ; >max-u - 1s
      ;; exception on write ; <min - 1u
      ;; exception on write ; min - 1u
      ;; exception on write ; -n - 1u
      ;; exception on write ; -2 - 1u
      ;; exception on write ; -1 - 1u
      (check (equal? (read-binary-fixnum-u8 p)  0)); 0 - 1u
      (check (equal? (read-binary-fixnum-u8 p)  1)); 1 - 1u
      (check (equal? (read-binary-fixnum-u8 p)  2)); 2 - 1u
      (check (equal? (read-binary-fixnum-u8 p)  34)); n - 1u
      (check (equal? (read-binary-fixnum-u8 p)  127)); max-s - 1u
      (check (equal? (read-binary-fixnum-u8 p)  130)); >max-s - 1u
      (check (equal? (read-binary-fixnum-u8 p)  255)); max-u - 1u
      ;; exception on write ; >max-u - 1u
      ;; exception on write ; <min - 2s
      (check (equal? (read-binary-fixnum-s16 p)  -32768)); min - 2s
      (check (equal? (read-binary-fixnum-s16 p)  -18273)); -n - 2s
      (check (equal? (read-binary-fixnum-s16 p)  -2)); -2 - 2s
      (check (equal? (read-binary-fixnum-s16 p)  -1)); -1 - 2s
      (check (equal? (read-binary-fixnum-s16 p)  0)); 0 - 2s
      (check (equal? (read-binary-fixnum-s16 p)  1)); 1 - 2s
      (check (equal? (read-binary-fixnum-s16 p)  2)); 2 - 2s
      (check (equal? (read-binary-fixnum-s16 p)  12726)); n - 2s
      (check (equal? (read-binary-fixnum-s16 p)  32767)); max-s - 2s
      ;; exception on write ; >max-s - 2s
      ;; exception on write ; max-u - 2s
      ;; exception on write ; >max-u - 2s
      ;; exception on write ; <min - 2u
      ;; exception on write ; min - 2u
      ;; exception on write ; -n - 2u
      ;; exception on write ; -2 - 2u
      ;; exception on write ; -1 - 2u
      (check (equal? (read-binary-fixnum-u16 p)  0)); 0 - 2u
      (check (equal? (read-binary-fixnum-u16 p)  1)); 1 - 2u
      (check (equal? (read-binary-fixnum-u16 p)  2)); 2 - 2u
      (check (equal? (read-binary-fixnum-u16 p)  12726)); n - 2u
      (check (equal? (read-binary-fixnum-u16 p)  32767)); max-s - 2u
      (check (equal? (read-binary-fixnum-u16 p)  35000)); >max-s - 2u
      (check (equal? (read-binary-fixnum-u16 p)  65535)); max-u - 2u
      ;; exception on write ; >max-u - 2u
      ;; exception on write ; <min - 4s
      (check (equal? (read-binary-fixnum-s32 p)  -2147483648)); min - 4s
      (check (equal? (read-binary-fixnum-s32 p)  -23443555)); -n - 4s
      (check (equal? (read-binary-fixnum-s32 p)  -2)); -2 - 4s
      (check (equal? (read-binary-fixnum-s32 p)  -1)); -1 - 4s
      (check (equal? (read-binary-fixnum-s32 p)  0)); 0 - 4s
      (check (equal? (read-binary-fixnum-s32 p)  1)); 1 - 4s
      (check (equal? (read-binary-fixnum-s32 p)  2)); 2 - 4s
      (check (equal? (read-binary-fixnum-s32 p)  34548374)); n - 4s
      (check (equal? (read-binary-fixnum-s32 p)  2147483647)); max-s - 4s
      ;; exception on write ;; >max-s - 4s
      ;; exception on write ;; max-u - 4s
      ;; exception on write ;; >max-u - 4s
      ;; exception on write ;; <min - 4u
      ;; exception on write ;; min - 4u
      ;; exception on write ;; -n - 4u
      ;; exception on write ;; -2 - 4u
      ;; exception on write ;; -1 - 4u
      (check (equal? (read-binary-fixnum-u32 p)  0)); 0 - 4u
      (check (equal? (read-binary-fixnum-u32 p)  1)); 1 - 4u
      (check (equal? (read-binary-fixnum-u32 p)  2)); 2 - 4u
      (check (equal? (read-binary-fixnum-u32 p)  34548374)); n - 4u
      (check (equal? (read-binary-fixnum-u32 p)  2147483647)); max-s - 4u
      (check (equal? (read-binary-fixnum-u32 p)  2500000000)); >max-s - 4u
      (check (equal? (read-binary-fixnum-u32 p)  4294967295)); max-u - 4u
      ;; exception on write ; >max-u - 4u

      ;; Test-Case end of file state.
      (check (eof-object? (read-binary-fixnum-u8 p)))
      (check (eof-object? (read-binary-fixnum-u16 p)))
      (check (eof-object? (read-binary-fixnum-u32 p)))
      (check (eof-object? (read-binary-fixnum-s8 p)))
      (check (eof-object? (read-binary-fixnum-s16 p)))
      (check (eof-object? (read-binary-fixnum-s32 p)))

      ;; Paramater test-case validation
      (check (runtime-error? (read-binary-fixnum-s8 "foo")))
      (check (runtime-error? (read-binary-fixnum-s16 :baz)))
      (check (runtime-error? (read-binary-fixnum-s32 '()))))
    (check (not (runtime-error? (delete-file test-filename))))))

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

(define-test handler-bind

  (check (equal? (handler-bind ((signal-1 (lambda () :foo))) 12) 12))

  (check
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (checkpoint 1)
            (catch 'error-escape
              (checkpoint 2)
              (signal 'unhandled-signal)
              (checkpoint 3)
              (checkpoint 4)))))

  (check
   (equal? '(1 2 3)
           (checkpoint-order-of
            (handler-bind ((signal-1 (lambda () (checkpoint 2))))
              (checkpoint 1)
              (signal 'signal-1)
              (checkpoint 3)))))

  (check
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (handler-bind ((signal-1 (lambda () (checkpoint 3))))
              (handler-bind ((signal-1 (lambda () (checkpoint 2))))
                (checkpoint 1)
                (signal 'signal-1)
                (checkpoint 4))))))

  (check
   (equal? '(:begin :inner-begin :outer :inner-end :outer :end)
           (checkpoint-order-of
            (handler-bind ((signal-1 (lambda () 
                                       (checkpoint :outer))))
              (handler-bind ((signal-1 (lambda () 
                                         (checkpoint :inner-begin)
                                         (signal 'signal-1)
                                         (checkpoint :inner-end))))
                (checkpoint :begin)
                (signal 'signal-1)
                (checkpoint :end))))))

  (check
   (equal? '(1 2 3 4)
           (checkpoint-order-of
            (handler-bind ((condition-1 (lambda args (checkpoint 2)))
                           (condition-2 (lambda args (checkpoint 3))))
              (checkpoint 1)
              (signal 'condition-1)
              (signal 'condition-2)
              (checkpoint 4)))))
  
  (check
   (equal? '(:begin 
             :outer-handler-established 
             :in-catch 
             :inner-handler-established 
             :outer-test-handler
             :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (handler-bind ((test-handler (lambda () (checkpoint :outer-test-handler))))
              (checkpoint :outer-handler-established)
              (catch 'drop-inner-handler
                (checkpoint :in-catch)
                (handler-bind ((test-handler (lambda () (checkpoint :inner-test-handler))))
                  (checkpoint :inner-handler-established)
                  (throw 'drop-inner-handler)
                  (checkpoint #f)))
              (signal 'test-handler)
              (checkpoint :end)))))

  (check
   (equal? '(:begin 
             :outer-handler-established 
             :in-catch 
             :inner-handler-established 
             :unwinding
             :outer-test-handler
             :end)
           (checkpoint-order-of
            (checkpoint :begin)
            (handler-bind ((test-handler (lambda () (checkpoint :outer-test-handler))))
              (checkpoint :outer-handler-established)
              (catch 'drop-inner-handler
                (checkpoint :in-catch)
                (unwind-protect (lambda ()
                                  (handler-bind ((test-handler (lambda () (checkpoint :inner-test-handler))))
                                    (checkpoint :inner-handler-established)
                                    (throw 'drop-inner-handler)
                                    (checkpoint #f)))
                                (lambda ()
                                  (checkpoint :unwinding))))
              (signal 'test-handler)
              (checkpoint :end))))))

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
  (check
   (equal? (string->input-port-chars "\n\n\n\n" 0)
           '(#\newline #\newline #\newline #\newline)))
  (check
   (equal? (string->input-port-chars "\r\r\r\r" 0)
           '(#\cr #\cr #\cr #\cr)))
  (check
   (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 0)
           '(#\cr #\newline #\cr #\newline #\cr #\newline #\cr #\newline)))
  (check
   (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 0)
           '(#\newline #\cr #\newline #\cr #\newline #\cr #\newline #\cr)))

  (check
   (equal? (string->input-port-chars "*\n*\n*\n*\n*" 0)
           '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (check
   (equal? (string->input-port-chars "*\r*\r*\r*\r*" 0)
           '(#\* #\cr #\* #\cr #\* #\cr #\* #\cr #\*)))
  (check
   (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 0)
           '(#\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\*)))
  (check
   (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 0)
           '(#\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\*)))

  ;; peek-count=1
  (check
   (equal? (string->input-port-chars "\n\n\n\n" 1)
           '(#\newline #\newline #\newline #\newline)))
  (check
   (equal? (string->input-port-chars "\r\r\r\r" 1)
           '(#\cr #\cr #\cr #\cr)))
  (check
   (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 1)
           '(#\cr #\newline #\cr #\newline #\cr #\newline #\cr #\newline)))
  (check
   (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 1)
           '(#\newline #\cr #\newline #\cr #\newline #\cr #\newline #\cr)))
  
  (check
   (equal? (string->input-port-chars "*\n*\n*\n*\n*" 1)
           '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (check
   (equal? (string->input-port-chars "*\r*\r*\r*\r*" 1)
           '(#\* #\cr #\* #\cr #\* #\cr #\* #\cr #\*)))
  (check
   (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 1)
           '(#\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\*)))
  (check
   (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 1)
           '(#\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\*)))

  ;; peek-count=2
  (check
   (equal? (string->input-port-chars "\n\n\n\n" 2)
           '(#\newline #\newline #\newline #\newline)))
  (check
   (equal? (string->input-port-chars "\r\r\r\r" 2)
           '(#\cr #\cr #\cr #\cr)))
  (check
   (equal? (string->input-port-chars "\r\n\r\n\r\n\r\n" 2)
           '(#\cr #\newline #\cr #\newline #\cr #\newline #\cr #\newline)))
  (check
   (equal? (string->input-port-chars "\n\r\n\r\n\r\n\r" 2)
           '(#\newline #\cr #\newline #\cr #\newline #\cr #\newline #\cr)))
  
  (check
   (equal? (string->input-port-chars "*\n*\n*\n*\n*" 2)
           '(#\* #\newline #\* #\newline #\* #\newline #\* #\newline #\*)))
  (check
   (equal? (string->input-port-chars "*\r*\r*\r*\r*" 2)
           '(#\* #\cr #\* #\cr #\* #\cr #\* #\cr #\*)))
  (check
   (equal? (string->input-port-chars "*\r\n*\r\n*\r\n*\r\n*" 2)
           '(#\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\* #\cr #\newline #\*)))
  (check
   (equal? (string->input-port-chars "*\n\r*\n\r*\n\r*\n\r*" 2)
           '(#\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\* #\newline #\cr #\*))))

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
  (check
   (equal? (string->input-port-char-locations "  *  *   *\n*  *  ****\n*  *\n\n\n*   \n\n*   *" #\* 0 #t)
           '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
             (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))


  (check
   (equal? (string->input-port-char-locations "  *  *   *\r\n*  *  ****\r\n*  *\r\n\r\n\r\n*   \r\n\r\n*   *" #\* 0 #t)
           '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
             (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))

  ;; Translate-mode=#t
  ;; peek-count=1
  (check
   (equal? (string->input-port-char-locations "  *  *   *\n*  *  ****\n*  *\n\n\n*   \n\n*   *" #\* 1 #t)
           '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
             (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))


  (check
   (equal? (string->input-port-char-locations "  *  *   *\r\n*  *  ****\r\n*  *\r\n\r\n\r\n*   \r\n\r\n*   *" #\* 1 #t)
           '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
             (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))


  ;; Translate-mode=#t
  ;; peek-count=2
  (check (equal? (string->input-port-char-locations "  *  *   *\n*  *  ****\n*  *\n\n\n*   \n\n*   *" #\* 2 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5))))

  (check (equal? (string->input-port-char-locations "  *  *   *\r\n*  *  ****\r\n*  *\r\n\r\n\r\n*   \r\n\r\n*   *" #\* 2 #t)
                 '((1 . 3) (1 . 6) (1 . 10) (2 . 1) (2 . 4) (2 . 7) (2 . 8) 
                   (2 . 9) (2 . 10) (3 . 1) (3 . 4) (6 . 1) (8 . 1) (8 . 5)))))

(define (->output-port-locations objs port)
  "Returns a list of the port-locations of port at the end of display'ing each
   object in <objs> to that port."
  (map #L(begin
           (display _ port)
           (port-location port))
       objs))

;; These tests use a string output port as a proxy for all output
;; ports. This is currently the best facsimile of a generic output
;; port, since null ports do not manage port-locations.

(define-test output-port-port-locations
  (let ((port (open-output-string)))
    (set-port-translate-mode! port #f)
    (check (equal? (->output-port-locations '("" "foo" "bar" "\n" "foobar" "\ntest1\n") port)
                       '((1 . 0) (1 . 3) (1 . 6) (2 . 0) (2 . 6) (4 . 0)))))

  (let ((port (open-output-string)))
    (set-port-translate-mode! port #t)
    (check (equal? (->output-port-locations '("" "foo" "bar" "\n" "foobar" "\ntest1\n") port)
                       '((1 . 0) (1 . 3) (1 . 6) (2 . 0) (2 . 6) (4 . 0))))))

(define-test fresh-line
  (let ((raw-port (open-output-string)))
    (set-port-translate-mode! raw-port #f)

    (check (equal? (port-row raw-port) 1))
    (check (equal? (port-column raw-port) 0))

    (fresh-line raw-port)

    (check (equal? (port-row raw-port) 1))
    (check (equal? (port-column raw-port) 0))

    (fresh-line raw-port)

    (check (equal? (port-row raw-port) 1))
    (check (equal? (port-column raw-port) 0))

    (display "foo" raw-port)
    (fresh-line raw-port)

    (check (equal? (port-row raw-port) 2))
    (check (equal? (port-column raw-port) 0)))

  (let ((translate-port (open-output-string)))
    (set-port-translate-mode! translate-port #t)

    (check (equal? (port-row translate-port) 1))
    (check (equal? (port-column translate-port) 0))

    (fresh-line translate-port)

    (check (equal? (port-row translate-port) 1))
    (check (equal? (port-column translate-port) 0))

    (fresh-line translate-port)

    (check (equal? (port-row translate-port) 1))
    (check (equal? (port-column translate-port) 0))

    (display "foo" translate-port)
    (fresh-line translate-port)

    (check (equal? (port-row translate-port) 2))
    (check (equal? (port-column translate-port) 0))))

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

(define-test write-strings
  (check (runtime-error? (write-strings)))
  (check (runtime-error? (write-strings :not-a-port)))
  (check (runtime-error? (write-strings (open-null-output-port) :not-a-string)))
  (check (runtime-error? (write-strings (open-null-output-port) "string" "yup, another" :not-a-string)))

  (let ((os (open-output-string)))
    (write-strings os "test")
    (check (equal? "test" (get-output-string os)))
    (write-strings os "case" #\1 "2" "3" "4" "5")
    (check (equal? "testcase12345" (get-output-string os)))

    (check (eq? os (write-strings os)))

    (check (equal? "testcase12345" (get-output-string os)))))


