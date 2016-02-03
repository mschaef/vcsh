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


