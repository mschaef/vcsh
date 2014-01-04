(use-package! "unit-test")

(define-test fast-io-hashes
  (test-case (can-fast-io-round-trip? #h(:eq)))
  (test-case (can-fast-io-round-trip? #h(:eq a 1)))
  (test-case (can-fast-io-round-trip? #h(:eq a 1 b 2)))
  (test-case (can-fast-io-round-trip? #h(:eq a 1 b 2 c 3 d 4)))

  (test-case (can-fast-io-round-trip? #h(:equal)))
  (test-case (can-fast-io-round-trip? #h(:equal a 1)))
  (test-case (can-fast-io-round-trip? #h(:equal a 1 b 2)))
  (test-case (can-fast-io-round-trip? #h(:equal a 1 b 2 c 3 d 4))))



