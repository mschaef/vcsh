(use-package! "unit-test")

(define-test integer->char
  (check (runtime-error? (integer->char -1.0)))
  (check (runtime-error? (integer->char 65536.0)))
  (check (runtime-error? (integer->char #\a)))
  (check (runtime-error? (integer->char [1])))
  (check (runtime-error? (integer->char [1 2 3])))
  (check (runtime-error? (integer->char [1 2 3])))
  (check (runtime-error? (integer->char '(1 2 3))))
  (check (runtime-error? (integer->char -1)))
  (check (runtime-error? (integer->char 256)))
  (check (runtime-error? (integer->char 65535)))
  (check (runtime-error? (integer->char 65536)))

  (let ((number-of-characters 256))
    (dotimes (n number-of-characters) (check (char? (integer->char n))))
    (dotimes (n number-of-characters) (check (= n (char->integer (integer->char n)))))
    (dotimes (n number-of-characters) (check (char=? (integer->char n) (integer->char n))))))

;
; Vector test cases
;

