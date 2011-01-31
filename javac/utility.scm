;;;; utility.lisp - Some utility functions used in the compiler.
;;;;
;;;; Michael Schaeffer (CS375, Hartmann)

;;;; Compiler error handling functions

(define (compiler-error phase lineno error-message :optional paramaters)
  "Standard compiler error handler"
  (error "~a Error in line ~a: ~a ~a~%" phase lineno error-message paramaters))

;;;; Code for manipulating Byte queues
;
; A byte queue is a Lisp list of numerical byte values.

;; TODO: Add debug code for validating byte values

(define (make-bq . bytes)
    (let ((q (make-queue)))
      (dolist (b bytes)
        (q-enqueue! b q))
      q))

(define (pad-bq bq desired-length)
  (labels
      ((pad-bq-1 (bq remaining-bytes)
         (if (>= 0 remaining-bytes)
             bq
             (prepend-byte 0 (pad-bq-1 bq (1- remaining-bytes))))))
    (pad-bq-1 bq (- desired-length (length bq)))))

(define (make-bytes-from-short short)
  (list
   (quotient short 256)
   (remainder short 256)))

(define (make-bytes-from-char char)
  (list (char->integer char)))


(define (make-bytes-from-string string)
  (let (bytes)
    (do ((i (1- (length string)) (- i 1)))
	((= i -1))
      (setf bytes (cons (char-code (char string i)) bytes)))
    bytes))

(define (char-digit char)
  (if (char-numeric? char)
      (- (char->integer char) (char->integer #\0) )
      (+ 10 (- (char->integer (char-downcase char)) (char->integer #\a)))))

(define (make-fixnum-from-string string :keyword (base 10))
  (let ((num 0))
    (do ((i 0 (+ i 1)))
	((= i (length string)))
      (setf num (+ (char-digit (char string i)) (* base num))))
    num))

(define (make-bytes-from-fixnum num)
  (let ((remaining num)
        bytes)
    (while (/= 0 remaining)
      (setf bytes (cons (mod remaining 256) bytes))
      (setf remaining (floor (/ remaining 256))))
    bytes))

(define (prepend-byte byte bq)
  (cons byte bq))

(define (append-to-bq bq . bytes)
  (concatenate-bqs bq bytes))

(defmacro (concatenate-bqs . bq) 
  `(append ,@bqs))




