

(define (shuffle-list xs)
  (map car
       (qsort (map (lambda (x) (cons x (random))) xs)
              <
              cdr)))

(define (make-test-alist n-elements)
  (shuffle-list
   (map (lambda (x) (cons x (number->string x))) (iseq 0 n-elements))))

(define (time-alist size n-lookups)
  (let ((test-alist (make-test-alist size)))
    (time
     (do ((i   0 (+ i 1))
          (idx 0 (if (> idx size)
                     0
                     (+ idx 1)))
          (answer '() (assoc idx test-alist)))
         ((>= i n-lookups) answer)))))

(define (make-test-hash n-elements)
  (a-list->hash (make-test-alist n-elements)))

(define (time-hash size n-lookups)
  (let ((test-hash (make-test-hash size)))
    (time
     (do ((i   0 (+ i 1))
          (idx 0 (if (> idx size)
                     0
                     (+ idx 1)))
          (answer '() (hash-ref test-hash idx)))
         ((>= i n-lookups) answer)))))


; user> (begin (make-test-alist 500000) ())
; time = 24715.8 ms (4658.76 gc), 207849057 cons work, 796409940 fops, 636816274 frames
