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
  (check (can-fast-io-round-trip? ""))
  (check (can-fast-io-round-trip? "a"))
  (check (can-fast-io-round-trip? "\n"))
  (check (can-fast-io-round-trip? "\n\r"))
  (check (can-fast-io-round-trip? "\r\n"))
  (check (can-fast-io-round-trip? "\r"))
  (check (can-fast-io-round-trip? (make-all-byte-string)))
  (check (can-fast-io-round-trip? (make-all-byte-combo-string)))

  (dotimes (n (char->integer (system-info :most-positive-character))) 
    (check (can-fast-io-round-trip? (integer->char n)))))
