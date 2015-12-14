
(use-package! "unit-test")

(define-test do
  (check (equal? (do ((vec (make-vector 5))
                      (i 0 (+ i 1)))
                     ((= i 5) vec)
                   (vector-set! vec i i))
                 [0 1 2 3 4]))
  (check (equal? (let ((x '(1 3 5 7 9)))
                   (do ((x x (cdr x))
                        (sum 0 (+ sum (car x))))
                       ((null? x) sum)))
                 25)))
