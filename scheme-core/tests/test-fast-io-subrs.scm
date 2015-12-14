(use-package! "unit-test")

(define-test fast-io-subrs
  (check (can-fast-io-round-trip? car)))

