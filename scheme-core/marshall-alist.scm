(define (my-qsort xs less? :optional (key identity))
  "Sorts the list <xs> into the order imposed by the comparison
   predicate <less?>. For any two elements of <xs>, x and y,
   x will appear before y in the result list if x and y satisfy
   (less? x y). <less?> should never return different values on
   subsequent calls with the same arguments. <key> is an optional
   accessor function used to access the sort key for list elements. If
   specified, the ordering function for two elements x and y is actually
   (less? (key x) (key y)). If <key> is passed as a symbol, it is taken
   to be a slot name suitable for use with slot-ref."
  (let ((key (if (symbol? key) #L(slot-ref _ key) key)))
    (define (sort-step xs)
      (if (null? xs)
          ()
          (let* ((pivot-index (random (length xs)))
                 (pivot-value (list-ref xs pivot-index)))
            (let loop ((less ()) (greater ()) (index 0) (xs xs))
              (cond ((= index pivot-index)
                     (loop less greater (+ 1 index) (cdr xs)))
                    ((null? xs)
                     (nconc (sort-step less) (cons pivot-value (sort-step greater))))
                    ((less? (key (car xs)) (key pivot-value))
                     (loop (cons (car xs) less) greater (+ index 1) (cdr xs)))
                    (#t
                     (loop less (cons (car xs) greater) (+ index 1) (cdr xs))))))))
    (runtime-check list? xs)
    (runtime-check procedure? less?)
    (runtime-check procedure? key)
    (sort-step xs)))


(define (shuffle-list xs)
  (map car
       (my-qsort (map (lambda (x) (cons x (random))) xs)
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


;; user> (begin (make-test-alist 500000) ())
;; time = 24715.8 ms (4658.76 gc), 207849057 cons work, 796409940 fops, 636816274 frames

;; time = 22663.8 ms (3749.64 gc), 193182639 cons work, 752237131 fops, 600320808 frames


