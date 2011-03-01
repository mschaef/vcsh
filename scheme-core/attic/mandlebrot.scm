

                                        ;g(z) = z^2 + c


(define (mandel ul lr :optional (res 50) (op (current-output-port)))
  (define (in-mandlebrot-set pt :optional (limit 1000))
    "Determines if <pt> is a point in the nandlebrot set. Returns #t
   if the point is in the set, returns the count of iterations it
   took to discover the point was not, otherwise."
    (let loop ((z 0)
               (count 0))
      (cond ((>= count limit)
             #t)
            ((> (magnitude z) 2.0)
             count)
            (#t
             (loop (+ (* z z) pt) (+ 1 count))))))
  (let ((real-step (/ (- (real-part lr) (real-part ul)) res))
        (imag-step (/ (- (imag-part lr) (imag-part ul)) res)))
    (let loop ((real-count 0)
               (imag-count 0))
      (cond ((> imag-count res)
             )
            ((> real-count res)
             (newline)
             (loop 0 (+ imag-count 1)))
            (#t
             (if (eq? (in-mandlebrot-set (make-rectangular (+ (real-part ul) (* real-count real-step))
                                                           (+ (imag-part ul) (* imag-count imag-step))))
                      #t)
                 (display "*")
                 (display " "))
             (loop (+ real-count 1) imag-count))))))



(define (a)
  (dynamic-let ((*info* #f))
    (enlarge-heap 50)
    (mandel -2+1i 1-1i)))