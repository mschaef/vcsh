(use-package! "unit-test")

(define-test fast-io-subrs
  (test-case (can-fast-io-round-trip? car)))

