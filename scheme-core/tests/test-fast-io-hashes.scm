(use-package! "unit-test")

(define-proto point-2-fast-io-hash-test
  'x 3
  'y 4)

(define-proto (point-3-fast-io-hash-test point-2-fast-io-hash-test)
  'z 5)

(define-test fast-io-hashes
  (test-case (can-fast-io-round-trip? #h(:eq)))
  (test-case (can-fast-io-round-trip? #h(:eq a 1)))
  (test-case (can-fast-io-round-trip? #h(:eq a 1 b 2)))
  (test-case (can-fast-io-round-trip? #h(:eq a 1 b 2 c 3 d 4)))

  (test-case (can-fast-io-round-trip? #h(:equal)))
  (test-case (can-fast-io-round-trip? #h(:equal a 1)))
  (test-case (can-fast-io-round-trip? #h(:equal a 1 b 2)))
  (test-case (can-fast-io-round-trip? #h(:equal a 1 b 2 c 3 d 4))))

(define-test fast-io-instance
  (test-case (can-fast-io-round-trip? (make-instance #f 'x 1 'y 2 'z 3)))
    
  (test-case (can-fast-io-round-trip? point-2-fast-io-hash-test))
  (test-case (can-fast-io-round-trip? point-3-fast-io-hash-test))

  (test-case (can-fast-io-round-trip? (make-instance point-2-fast-io-hash-test)))
  (test-case (can-fast-io-round-trip? (make-instance point-3-fast-io-hash-test)))

  (test-case (can-fast-io-round-trip? (make-instance 'point-2-fast-io-hash-test 'x 1 'y 2)))
  (test-case (can-fast-io-round-trip? (make-instance 'point-3-fast-io-hash-test 'z 12 'x 1))))


