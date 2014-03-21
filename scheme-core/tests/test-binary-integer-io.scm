(use-package! "unit-test")

(define-test binary-integer-io
  (let ((test-filename (temporary-file-name "sct")))
    (with-port p (open-file test-filename :mode :write :encoding :binary)
      (test-case (not (runtime-error? (write-binary-fixnum-s8 -128 p)))) ; min - 1s
      (test-case (not (runtime-error? (write-binary-fixnum-s8 -34 p)))) ; -n - 1s
      (test-case (not (runtime-error? (write-binary-fixnum-s8 -2 p)))) ; -2 - 1s
      (test-case (not (runtime-error? (write-binary-fixnum-s8 -1 p)))) ; -1 - 1s
      (test-case (not (runtime-error? (write-binary-fixnum-s8 0 p)))) ; 0 - 1s
      (test-case (not (runtime-error? (write-binary-fixnum-s8 1 p)))) ; 1 - 1s
      (test-case (not (runtime-error? (write-binary-fixnum-s8 2 p)))) ; 2 - 1s
      (test-case (not (runtime-error? (write-binary-fixnum-s8 34 p)))) ; n - 1s
      (test-case (not (runtime-error? (write-binary-fixnum-s8 127 p)))) ; max-s - 1s
      (test-case (not (runtime-error? (write-binary-fixnum-u8 0 p)))) ; 0 - 1u
      (test-case (not (runtime-error? (write-binary-fixnum-u8 1 p)))) ; 1 - 1u
      (test-case (not (runtime-error? (write-binary-fixnum-u8 2 p)))) ; 2 - 1u
      (test-case (not (runtime-error? (write-binary-fixnum-u8 34 p)))) ; n - 1u
      (test-case (not (runtime-error? (write-binary-fixnum-u8 127 p)))) ; max-s - 1u
      (test-case (not (runtime-error? (write-binary-fixnum-u8 130 p)))) ; >max-s - 1u
      (test-case (not (runtime-error? (write-binary-fixnum-u8 255 p)))) ; max-u - 1u
      (test-case (not (runtime-error? (write-binary-fixnum-s16 -32768 p)))) ; min - 2s
      (test-case (not (runtime-error? (write-binary-fixnum-s16 -18273 p)))) ; -n - 2s
      (test-case (not (runtime-error? (write-binary-fixnum-s16 -2 p)))) ; -2 - 2s
      (test-case (not (runtime-error? (write-binary-fixnum-s16 -1 p)))) ; -1 - 2s
      (test-case (not (runtime-error? (write-binary-fixnum-s16 0 p)))) ; 0 - 2s
      (test-case (not (runtime-error? (write-binary-fixnum-s16 1 p)))) ; 1 - 2s
      (test-case (not (runtime-error? (write-binary-fixnum-s16 2 p)))) ; 2 - 2s
      (test-case (not (runtime-error? (write-binary-fixnum-s16 12726 p)))) ; n - 2s
      (test-case (not (runtime-error? (write-binary-fixnum-s16 32767 p)))) ; max-s - 2s
      (test-case (not (runtime-error? (write-binary-fixnum-u16 0 p)))) ; 0 - 2u
      (test-case (not (runtime-error? (write-binary-fixnum-u16 1 p)))) ; 1 - 2u
      (test-case (not (runtime-error? (write-binary-fixnum-u16 2 p)))) ; 2 - 2u
      (test-case (not (runtime-error? (write-binary-fixnum-u16 12726 p)))) ; n - 2u
      (test-case (not (runtime-error? (write-binary-fixnum-u16 32767 p)))) ; max-s - 2u
      (test-case (not (runtime-error? (write-binary-fixnum-u16 35000 p)))) ; >max-s - 2u
      (test-case (not (runtime-error? (write-binary-fixnum-u16 65535 p)))) ; max-u - 2u
      (test-case (not (runtime-error? (write-binary-fixnum-s32 -2147483648 p)))) ; min - 4s
      (test-case (not (runtime-error? (write-binary-fixnum-s32 -23443555 p)))) ; -n - 4s
      (test-case (not (runtime-error? (write-binary-fixnum-s32 -2 p)))) ; -2 - 4s
      (test-case (not (runtime-error? (write-binary-fixnum-s32 -1 p)))) ; -1 - 4s
      (test-case (not (runtime-error? (write-binary-fixnum-s32 0 p)))) ; 0 - 4s
      (test-case (not (runtime-error? (write-binary-fixnum-s32 1 p)))) ; 1 - 4s
      (test-case (not (runtime-error? (write-binary-fixnum-s32 2 p)))) ; 2 - 4s
      (test-case (not (runtime-error? (write-binary-fixnum-s32 34548374 p)))) ; n - 4s
      (test-case (not (runtime-error? (write-binary-fixnum-s32 2147483647 p)))) ; max-s - 4s
      (test-case (not (runtime-error? (write-binary-fixnum-u32 0 p)))) ; 0 - 4u
      (test-case (not (runtime-error? (write-binary-fixnum-u32 1 p)))) ; 1 - 4u
      (test-case (not (runtime-error? (write-binary-fixnum-u32 2 p)))) ; 2 - 4u
      (test-case (not (runtime-error? (write-binary-fixnum-u32 34548374 p)))) ; n - 4u
      (test-case (not (runtime-error? (write-binary-fixnum-u32 2147483647 p)))) ; max-s - 4u
      (test-case (not (runtime-error? (write-binary-fixnum-u32 2500000000 p)))) ; >max-s - 4u
      (test-case (not (runtime-error? (write-binary-fixnum-u32 4294967295 p)))) ; max-u - 4u
      )
    (test-case (file-exists? test-filename))
    (with-port p (open-file test-filename :mode :read :encoding :binary)
      (let ((binary-string (read-binary-string 1000 p)))
	(test-case (= 112 (length binary-string)))
	(test-case (equal? binary-string "\200\336\376\377\000\001\002\"\177\000\001\002\"\177\202\377\000\200\237\270\376\377\377\377\000\000\001\000\002\000\2661\377\177\000\000\001\000\002\000\2661\377\177\270\210\377\377\000\000\000\200\235G\232\376\376\377\377\377\377\377\377\377\000\000\000\000\001\000\000\000\002\000\000\000\226*\017\002\377\377\377\177\000\000\000\000\001\000\000\000\002\000\000\000\226*\017\002\377\377\377\177\000\371\002\225\377\377\377\377")))
      (test-case (eof-object? (read-binary-string 1 p))))
    (with-port p (open-file test-filename :mode :read :encoding :binary)
					; exception on write ; <min - 1s
      (test-case (equal? (read-binary-fixnum-s8 p)  -128)); min - 1s
      (test-case (equal? (read-binary-fixnum-s8 p)  -34)); -n - 1s
      (test-case (equal? (read-binary-fixnum-s8 p)  -2)); -2 - 1s
      (test-case (equal? (read-binary-fixnum-s8 p)  -1)); -1 - 1s
      (test-case (equal? (read-binary-fixnum-s8 p)  0)); 0 - 1s
      (test-case (equal? (read-binary-fixnum-s8 p)  1)); 1 - 1s
      (test-case (equal? (read-binary-fixnum-s8 p)  2)); 2 - 1s
      (test-case (equal? (read-binary-fixnum-s8 p)  34)); n - 1s
      (test-case (equal? (read-binary-fixnum-s8 p)  127)); max-s - 1s
					; exception on write ; >max-s - 1s
					; exception on write ; max-u - 1s
					; exception on write ; >max-u - 1s
					; exception on write ; <min - 1u
					; exception on write ; min - 1u
					; exception on write ; -n - 1u
					; exception on write ; -2 - 1u
					; exception on write ; -1 - 1u
      (test-case (equal? (read-binary-fixnum-u8 p)  0)); 0 - 1u
      (test-case (equal? (read-binary-fixnum-u8 p)  1)); 1 - 1u
      (test-case (equal? (read-binary-fixnum-u8 p)  2)); 2 - 1u
      (test-case (equal? (read-binary-fixnum-u8 p)  34)); n - 1u
      (test-case (equal? (read-binary-fixnum-u8 p)  127)); max-s - 1u
      (test-case (equal? (read-binary-fixnum-u8 p)  130)); >max-s - 1u
      (test-case (equal? (read-binary-fixnum-u8 p)  255)); max-u - 1u
					; exception on write ; >max-u - 1u
					; exception on write ; <min - 2s
      (test-case (equal? (read-binary-fixnum-s16 p)  -32768)); min - 2s
      (test-case (equal? (read-binary-fixnum-s16 p)  -18273)); -n - 2s
      (test-case (equal? (read-binary-fixnum-s16 p)  -2)); -2 - 2s
      (test-case (equal? (read-binary-fixnum-s16 p)  -1)); -1 - 2s
      (test-case (equal? (read-binary-fixnum-s16 p)  0)); 0 - 2s
      (test-case (equal? (read-binary-fixnum-s16 p)  1)); 1 - 2s
      (test-case (equal? (read-binary-fixnum-s16 p)  2)); 2 - 2s
      (test-case (equal? (read-binary-fixnum-s16 p)  12726)); n - 2s
      (test-case (equal? (read-binary-fixnum-s16 p)  32767)); max-s - 2s
					; exception on write ; >max-s - 2s
					; exception on write ; max-u - 2s
					; exception on write ; >max-u - 2s
					; exception on write ; <min - 2u
					; exception on write ; min - 2u
					; exception on write ; -n - 2u
					; exception on write ; -2 - 2u
					; exception on write ; -1 - 2u
      (test-case (equal? (read-binary-fixnum-u16 p)  0)); 0 - 2u
      (test-case (equal? (read-binary-fixnum-u16 p)  1)); 1 - 2u
      (test-case (equal? (read-binary-fixnum-u16 p)  2)); 2 - 2u
      (test-case (equal? (read-binary-fixnum-u16 p)  12726)); n - 2u
      (test-case (equal? (read-binary-fixnum-u16 p)  32767)); max-s - 2u
      (test-case (equal? (read-binary-fixnum-u16 p)  35000)); >max-s - 2u
      (test-case (equal? (read-binary-fixnum-u16 p)  65535)); max-u - 2u
					; exception on write ; >max-u - 2u
					; exception on write ; <min - 4s
      (test-case (equal? (read-binary-fixnum-s32 p)  -2147483648)); min - 4s
      (test-case (equal? (read-binary-fixnum-s32 p)  -23443555)); -n - 4s
      (test-case (equal? (read-binary-fixnum-s32 p)  -2)); -2 - 4s
      (test-case (equal? (read-binary-fixnum-s32 p)  -1)); -1 - 4s
      (test-case (equal? (read-binary-fixnum-s32 p)  0)); 0 - 4s
      (test-case (equal? (read-binary-fixnum-s32 p)  1)); 1 - 4s
      (test-case (equal? (read-binary-fixnum-s32 p)  2)); 2 - 4s
      (test-case (equal? (read-binary-fixnum-s32 p)  34548374)); n - 4s
      (test-case (equal? (read-binary-fixnum-s32 p)  2147483647)); max-s - 4s
					; exception on write ; >max-s - 4s
					; exception on write ; max-u - 4s
					; exception on write ; >max-u - 4s
					; exception on write ; <min - 4u
					; exception on write ; min - 4u
					; exception on write ; -n - 4u
					; exception on write ; -2 - 4u
					; exception on write ; -1 - 4u
      (test-case (equal? (read-binary-fixnum-u32 p)  0)); 0 - 4u
      (test-case (equal? (read-binary-fixnum-u32 p)  1)); 1 - 4u
      (test-case (equal? (read-binary-fixnum-u32 p)  2)); 2 - 4u
      (test-case (equal? (read-binary-fixnum-u32 p)  34548374)); n - 4u
      (test-case (equal? (read-binary-fixnum-u32 p)  2147483647)); max-s - 4u
      (test-case (equal? (read-binary-fixnum-u32 p)  2500000000)); >max-s - 4u
      (test-case (equal? (read-binary-fixnum-u32 p)  4294967295)); max-u - 4u
					; exception on write ; >max-u - 4u

      ; Test-Case end of file state.
      (test-case (eof-object? (read-binary-fixnum-u8 p)))
      (test-case (eof-object? (read-binary-fixnum-u16 p)))
      (test-case (eof-object? (read-binary-fixnum-u32 p)))
      (test-case (eof-object? (read-binary-fixnum-s8 p)))
      (test-case (eof-object? (read-binary-fixnum-s16 p)))
      (test-case (eof-object? (read-binary-fixnum-s32 p)))

      ; Paramater test-case validation
      (test-case (runtime-error? (read-binary-fixnum-s8 "foo")))
      (test-case (runtime-error? (read-binary-fixnum-s16 :baz)))
      (test-case (runtime-error? (read-binary-fixnum-s32 '())))
      )
    (test-case (not (runtime-error? (delete-file test-filename))))))

