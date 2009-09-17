(use-package! "unit-test")


(define (make-all-byte-string)
  (let ((p (open-output-string)))
   (set-port-translate-mode! p #f)
   (let loop ((i 0))
     (cond ((> i 255) (get-output-string p))
           (#t 
            (display (integer->char i) p)
            (loop (+ i 1)))))))
      
(define (make-all-byte-combo-string)
  (let ((p (open-output-string)))
   (set-port-translate-mode! p #f)
   (let loop ((i 0) (j 0))
       (cond ((> i 255) (loop 0 (+ j 1)))
             ((> j 255) (get-output-string p))
             (#t 
               (display (integer->char i) p)
               (display (integer->char j) p)
               (loop (+ i 1) j))))))

(define-test fast-io-strings-and-characters
  (test-case (can-fast-io-round-trip? ""))
  (test-case (can-fast-io-round-trip? "a"))
  (test-case (can-fast-io-round-trip? "\n"))
  (test-case (can-fast-io-round-trip? "\n\r"))
  (test-case (can-fast-io-round-trip? "\r\n"))
  (test-case (can-fast-io-round-trip? "\r"))
  (test-case (can-fast-io-round-trip? (make-all-byte-string)))
  (test-case (can-fast-io-round-trip? (make-all-byte-combo-string)))

  (dotimes (n (char->integer (system-info :most-positive-character))) 
    (test-case (can-fast-io-round-trip? (integer->char n)))))
