(use-package! "unit-test")

(define-test string-port-length
  (let ((os (open-output-string)))
    (set-port-translate-mode! os #f)

    (test-case (= 0 (length os)))

    (display "1" os)

    (test-case (= 1 (length os)))

    (display "2345" os)

    (test-case (= 5 (length os)))

    (display "\n" os)

    (test-case (= 6 (length os)))

    (display "67890" os)

    (test-case (= 11 (length os)))))

(define-test string-port-mode
  (let ((os (open-output-string))
        (is (open-input-string "test string")))
    (test-case (eq? :output (port-mode os)))
    (test-case (eq? :input (port-mode is)))))

(define-test port-mode
  (test-case (runtime-error? (port-mode #f)))
  (test-case (eq? :input (port-mode (open-null-input-port))))
  (test-case (eq? :output (port-mode (open-null-output-port))))

  (let ((ip (current-input-port)))
    (test-case (input-port? ip))
    (test-case (eq? :input (port-mode ip))))

  (let ((op (current-output-port)))
    (test-case (output-port? op))
    (test-case (eq? :output (port-mode op)))))
