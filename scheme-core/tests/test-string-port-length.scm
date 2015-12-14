(use-package! "unit-test")

(define-test string-port-length
  (let ((os (open-output-string)))
    (set-port-translate-mode! os #f)

    (check (= 0 (length os)))

    (display "1" os)

    (check (= 1 (length os)))

    (display "2345" os)

    (check (= 5 (length os)))

    (display "\n" os)

    (check (= 6 (length os)))

    (display "67890" os)

    (check (= 11 (length os)))))

(define-test string-port-mode
  (let ((os (open-output-string))
        (is (open-input-string "test string")))
    (check (eq? :output (port-mode os)))
    (check (eq? :input (port-mode is)))))

(define-test port-mode
  (check (runtime-error? (port-mode #f)))
  (check (eq? :input (port-mode (open-null-input-port))))
  (check (eq? :output (port-mode (open-null-output-port))))

  (let ((ip (current-input-port)))
    (check (input-port? ip))
    (check (eq? :input (port-mode ip))))

  (let ((op (current-output-port)))
    (check (output-port? op))
    (check (eq? :output (port-mode op)))))
