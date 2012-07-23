(use-package! "unit-test")

(define-test binary-integer-io
  (let ((test-filename (temporary-file-name "sct")))
    (with-port p (open-file test-filename :mode :write :encoding :binary)
      (test-case (not (runtime-error? (write-binary-fixnum -128 1 #t p)))) ; min - 1s
      (test-case (not (runtime-error? (write-binary-fixnum -34 1 #t p)))) ; -n - 1s
      (test-case (not (runtime-error? (write-binary-fixnum -2 1 #t p)))) ; -2 - 1s
      (test-case (not (runtime-error? (write-binary-fixnum -1 1 #t p)))) ; -1 - 1s
      (test-case (not (runtime-error? (write-binary-fixnum 0 1 #t p)))) ; 0 - 1s
      (test-case (not (runtime-error? (write-binary-fixnum 1 1 #t p)))) ; 1 - 1s
      (test-case (not (runtime-error? (write-binary-fixnum 2 1 #t p)))) ; 2 - 1s
      (test-case (not (runtime-error? (write-binary-fixnum 34 1 #t p)))) ; n - 1s
      (test-case (not (runtime-error? (write-binary-fixnum 127 1 #t p)))) ; max-s - 1s
      (test-case (not (runtime-error? (write-binary-fixnum 0 1 #f p)))) ; 0 - 1u
      (test-case (not (runtime-error? (write-binary-fixnum 1 1 #f p)))) ; 1 - 1u
      (test-case (not (runtime-error? (write-binary-fixnum 2 1 #f p)))) ; 2 - 1u
      (test-case (not (runtime-error? (write-binary-fixnum 34 1 #f p)))) ; n - 1u
      (test-case (not (runtime-error? (write-binary-fixnum 127 1 #f p)))) ; max-s - 1u
      (test-case (not (runtime-error? (write-binary-fixnum 130 1 #f p)))) ; >max-s - 1u
      (test-case (not (runtime-error? (write-binary-fixnum 255 1 #f p)))) ; max-u - 1u
      (test-case (not (runtime-error? (write-binary-fixnum -32768 2 #t p)))) ; min - 2s
      (test-case (not (runtime-error? (write-binary-fixnum -18273 2 #t p)))) ; -n - 2s
      (test-case (not (runtime-error? (write-binary-fixnum -2 2 #t p)))) ; -2 - 2s
      (test-case (not (runtime-error? (write-binary-fixnum -1 2 #t p)))) ; -1 - 2s
      (test-case (not (runtime-error? (write-binary-fixnum 0 2 #t p)))) ; 0 - 2s
      (test-case (not (runtime-error? (write-binary-fixnum 1 2 #t p)))) ; 1 - 2s
      (test-case (not (runtime-error? (write-binary-fixnum 2 2 #t p)))) ; 2 - 2s
      (test-case (not (runtime-error? (write-binary-fixnum 12726 2 #t p)))) ; n - 2s
      (test-case (not (runtime-error? (write-binary-fixnum 32767 2 #t p)))) ; max-s - 2s
      (test-case (not (runtime-error? (write-binary-fixnum 0 2 #f p)))) ; 0 - 2u
      (test-case (not (runtime-error? (write-binary-fixnum 1 2 #f p)))) ; 1 - 2u
      (test-case (not (runtime-error? (write-binary-fixnum 2 2 #f p)))) ; 2 - 2u
      (test-case (not (runtime-error? (write-binary-fixnum 12726 2 #f p)))) ; n - 2u
      (test-case (not (runtime-error? (write-binary-fixnum 32767 2 #f p)))) ; max-s - 2u
      (test-case (not (runtime-error? (write-binary-fixnum 35000 2 #f p)))) ; >max-s - 2u
      (test-case (not (runtime-error? (write-binary-fixnum 65535 2 #f p)))) ; max-u - 2u
      (test-case (not (runtime-error? (write-binary-fixnum -2147483648 4 #t p)))) ; min - 4s
      (test-case (not (runtime-error? (write-binary-fixnum -23443555 4 #t p)))) ; -n - 4s
      (test-case (not (runtime-error? (write-binary-fixnum -2 4 #t p)))) ; -2 - 4s
      (test-case (not (runtime-error? (write-binary-fixnum -1 4 #t p)))) ; -1 - 4s
      (test-case (not (runtime-error? (write-binary-fixnum 0 4 #t p)))) ; 0 - 4s
      (test-case (not (runtime-error? (write-binary-fixnum 1 4 #t p)))) ; 1 - 4s
      (test-case (not (runtime-error? (write-binary-fixnum 2 4 #t p)))) ; 2 - 4s
      (test-case (not (runtime-error? (write-binary-fixnum 34548374 4 #t p)))) ; n - 4s
      (test-case (not (runtime-error? (write-binary-fixnum 2147483647 4 #t p)))) ; max-s - 4s
      (test-case (not (runtime-error? (write-binary-fixnum 0 4 #f p)))) ; 0 - 4u
      (test-case (not (runtime-error? (write-binary-fixnum 1 4 #f p)))) ; 1 - 4u
      (test-case (not (runtime-error? (write-binary-fixnum 2 4 #f p)))) ; 2 - 4u
      (test-case (not (runtime-error? (write-binary-fixnum 34548374 4 #f p)))) ; n - 4u
      (test-case (not (runtime-error? (write-binary-fixnum 2147483647 4 #f p)))) ; max-s - 4u
      (test-case (not (runtime-error? (write-binary-fixnum 2500000000 4 #f p)))) ; >max-s - 4u
      (test-case (not (runtime-error? (write-binary-fixnum 4294967295 4 #f p)))) ; max-u - 4u
      )
    (test-case (file-exists? test-filename))
    (with-port p (open-file test-filename :mode :read :encoding :binary)
      (let ((binary-string (read-binary-string 1000 p)))
	(test-case (= 112 (length binary-string)))
	(test-case (equal? binary-string "\200\336\376\377\000\001\002\"\177\000\001\002\"\177\202\377\000\200\237\270\376\377\377\377\000\000\001\000\002\000\2661\377\177\000\000\001\000\002\000\2661\377\177\270\210\377\377\000\000\000\200\235G\232\376\376\377\377\377\377\377\377\377\000\000\000\000\001\000\000\000\002\000\000\000\226*\017\002\377\377\377\177\000\000\000\000\001\000\000\000\002\000\000\000\226*\017\002\377\377\377\177\000\371\002\225\377\377\377\377")))
      (test-case (eof-object? (read-binary-string 1 p))))
    (with-port p (open-file test-filename :mode :read :encoding :binary)
					; exception on write ; <min - 1s
      (test-case (equal? (read-binary-fixnum 1 #t p)  -128)); min - 1s
      (test-case (equal? (read-binary-fixnum 1 #t p)  -34)); -n - 1s
      (test-case (equal? (read-binary-fixnum 1 #t p)  -2)); -2 - 1s
      (test-case (equal? (read-binary-fixnum 1 #t p)  -1)); -1 - 1s
      (test-case (equal? (read-binary-fixnum 1 #t p)  0)); 0 - 1s
      (test-case (equal? (read-binary-fixnum 1 #t p)  1)); 1 - 1s
      (test-case (equal? (read-binary-fixnum 1 #t p)  2)); 2 - 1s
      (test-case (equal? (read-binary-fixnum 1 #t p)  34)); n - 1s
      (test-case (equal? (read-binary-fixnum 1 #t p)  127)); max-s - 1s
					; exception on write ; >max-s - 1s
					; exception on write ; max-u - 1s
					; exception on write ; >max-u - 1s
					; exception on write ; <min - 1u
					; exception on write ; min - 1u
					; exception on write ; -n - 1u
					; exception on write ; -2 - 1u
					; exception on write ; -1 - 1u
      (test-case (equal? (read-binary-fixnum 1 #f p)  0)); 0 - 1u
      (test-case (equal? (read-binary-fixnum 1 #f p)  1)); 1 - 1u
      (test-case (equal? (read-binary-fixnum 1 #f p)  2)); 2 - 1u
      (test-case (equal? (read-binary-fixnum 1 #f p)  34)); n - 1u
      (test-case (equal? (read-binary-fixnum 1 #f p)  127)); max-s - 1u
      (test-case (equal? (read-binary-fixnum 1 #f p)  130)); >max-s - 1u
      (test-case (equal? (read-binary-fixnum 1 #f p)  255)); max-u - 1u
					; exception on write ; >max-u - 1u
					; exception on write ; <min - 2s
      (test-case (equal? (read-binary-fixnum 2 #t p)  -32768)); min - 2s
      (test-case (equal? (read-binary-fixnum 2 #t p)  -18273)); -n - 2s
      (test-case (equal? (read-binary-fixnum 2 #t p)  -2)); -2 - 2s
      (test-case (equal? (read-binary-fixnum 2 #t p)  -1)); -1 - 2s
      (test-case (equal? (read-binary-fixnum 2 #t p)  0)); 0 - 2s
      (test-case (equal? (read-binary-fixnum 2 #t p)  1)); 1 - 2s
      (test-case (equal? (read-binary-fixnum 2 #t p)  2)); 2 - 2s
      (test-case (equal? (read-binary-fixnum 2 #t p)  12726)); n - 2s
      (test-case (equal? (read-binary-fixnum 2 #t p)  32767)); max-s - 2s
					; exception on write ; >max-s - 2s
					; exception on write ; max-u - 2s
					; exception on write ; >max-u - 2s
					; exception on write ; <min - 2u
					; exception on write ; min - 2u
					; exception on write ; -n - 2u
					; exception on write ; -2 - 2u
					; exception on write ; -1 - 2u
      (test-case (equal? (read-binary-fixnum 2 #f p)  0)); 0 - 2u
      (test-case (equal? (read-binary-fixnum 2 #f p)  1)); 1 - 2u
      (test-case (equal? (read-binary-fixnum 2 #f p)  2)); 2 - 2u
      (test-case (equal? (read-binary-fixnum 2 #f p)  12726)); n - 2u
      (test-case (equal? (read-binary-fixnum 2 #f p)  32767)); max-s - 2u
      (test-case (equal? (read-binary-fixnum 2 #f p)  35000)); >max-s - 2u
      (test-case (equal? (read-binary-fixnum 2 #f p)  65535)); max-u - 2u
					; exception on write ; >max-u - 2u
					; exception on write ; <min - 4s
      (test-case (equal? (read-binary-fixnum 4 #t p)  -2147483648)); min - 4s
      (test-case (equal? (read-binary-fixnum 4 #t p)  -23443555)); -n - 4s
      (test-case (equal? (read-binary-fixnum 4 #t p)  -2)); -2 - 4s
      (test-case (equal? (read-binary-fixnum 4 #t p)  -1)); -1 - 4s
      (test-case (equal? (read-binary-fixnum 4 #t p)  0)); 0 - 4s
      (test-case (equal? (read-binary-fixnum 4 #t p)  1)); 1 - 4s
      (test-case (equal? (read-binary-fixnum 4 #t p)  2)); 2 - 4s
      (test-case (equal? (read-binary-fixnum 4 #t p)  34548374)); n - 4s
      (test-case (equal? (read-binary-fixnum 4 #t p)  2147483647)); max-s - 4s
					; exception on write ; >max-s - 4s
					; exception on write ; max-u - 4s
					; exception on write ; >max-u - 4s
					; exception on write ; <min - 4u
					; exception on write ; min - 4u
					; exception on write ; -n - 4u
					; exception on write ; -2 - 4u
					; exception on write ; -1 - 4u
      (test-case (equal? (read-binary-fixnum 4 #f p)  0)); 0 - 4u
      (test-case (equal? (read-binary-fixnum 4 #f p)  1)); 1 - 4u
      (test-case (equal? (read-binary-fixnum 4 #f p)  2)); 2 - 4u
      (test-case (equal? (read-binary-fixnum 4 #f p)  34548374)); n - 4u
      (test-case (equal? (read-binary-fixnum 4 #f p)  2147483647)); max-s - 4u
      (test-case (equal? (read-binary-fixnum 4 #f p)  2500000000)); >max-s - 4u
      (test-case (equal? (read-binary-fixnum 4 #f p)  4294967295)); max-u - 4u
					; exception on write ; >max-u - 4u

      ; Test-Case end of file state.
      (test-case (eof-object? (read-binary-fixnum 1 #f p)))
      (test-case (eof-object? (read-binary-fixnum 2 #f p)))
      (test-case (eof-object? (read-binary-fixnum 4 #f p)))
      (test-case (eof-object? (read-binary-fixnum 1 #t p)))
      (test-case (eof-object? (read-binary-fixnum 2 #t p)))
      (test-case (eof-object? (read-binary-fixnum 4 #t p)))

      ; Paramater test-case validation
      (test-case (runtime-error? (read-binary-fixnum -1 #f p)))
      (test-case (runtime-error? (read-binary-fixnum 0 #f p)))
      (test-case (runtime-error? (read-binary-fixnum 3 #f p)))
      (test-case (runtime-error? (read-binary-fixnum 30 #f p)))
      (test-case (runtime-error? (read-binary-fixnum -1 #t p)))
      (test-case (runtime-error? (read-binary-fixnum 0 #t p)))
      (test-case (runtime-error? (read-binary-fixnum 3 #t p)))
      (test-case (runtime-error? (read-binary-fixnum 30 #t p)))
      (test-case (runtime-error? (read-binary-fixnum 1 "foo" p)))
      (test-case (runtime-error? (read-binary-fixnum 2 :baz p)))
      (test-case (runtime-error? (read-binary-fixnum 4 #(wooka) p)))
      (test-case (runtime-error? (read-binary-fixnum 1 #t "foo")))
      (test-case (runtime-error? (read-binary-fixnum 2 #t :baz)))
      (test-case (runtime-error? (read-binary-fixnum 4 #t '())))
      )
    (test-case (not (runtime-error? (delete-file test-filename))))))

