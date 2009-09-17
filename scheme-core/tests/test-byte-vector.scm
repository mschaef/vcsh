(use-package! "unit-test")

(define-test byte-vector
  (test-case (runtime-error? (vector->byte-vector ())))
  (test-case (runtime-error? (vector->byte-vector 123)))
  (test-case (runtime-error? (vector->byte-vector '(1 2 3))))

  (test-case (runtime-error? (byte-vector->vector ())))
  (test-case (runtime-error? (byte-vector->vector 123)))
  (test-case (runtime-error? (byte-vector->vector '(1 2 3))))

  (test-case (runtime-error? (vector->byte-vector #(-1))))
  (test-case (runtime-error? (vector->byte-vector #(256))))
  
  (test-case (runtime-error? (vector->byte-vector #(1.2))))
  (test-case (runtime-error? (vector->byte-vector #(()))))
  (test-case (runtime-error? (vector->byte-vector #(#\a))))
  (test-case (runtime-error? (vector->byte-vector #(:symbol))))
  
  (let* ((vec #(0 1 2 3 126 127 128 254 255))
         (bytevec (vector->byte-vector vec)))
    (test-case (eq? 'byte-vector (type-of bytevec)))
    (test-case (byte-vector? bytevec))
    (test-case (vector? (byte-vector->vector bytevec)))
    (test-case (equal? vec (byte-vector->vector bytevec)))
    (test-case (equal? #(0 1 2 3 126 127 128 254 255) (byte-vector->vector bytevec)))
    (test-case (not (eq? vec (byte-vector->vector bytevec))))))

