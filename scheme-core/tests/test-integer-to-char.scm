(use-package! "unit-test")

(define-test integer->char
  (test-case (runtime-error? (integer->char -1.0)))
  (test-case (runtime-error? (integer->char 65536.0)))
  (test-case (runtime-error? (integer->char #\a)))
  (test-case (runtime-error? (integer->char #(1))))
  (test-case (runtime-error? (integer->char #(1 2 3))))
  (test-case (runtime-error? (integer->char #(1 2 3))))
  (test-case (runtime-error? (integer->char '(1 2 3))))
  (test-case (runtime-error? (integer->char -1)))
  (test-case (runtime-error? (integer->char 256)))
  (test-case (runtime-error? (integer->char 65535)))
  (test-case (runtime-error? (integer->char 65536)))

  (let ((number-of-characters 256))
    (dotimes (n number-of-characters) (test-case (char? (integer->char n))))
    (dotimes (n number-of-characters) (test-case (= n (char->integer (integer->char n)))))
    (dotimes (n number-of-characters) (test-case (char=? (integer->char n) (integer->char n))))))

;
; Vector test cases
;

