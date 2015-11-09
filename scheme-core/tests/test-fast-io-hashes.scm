(use-package! "unit-test")

(define-test fast-io-hashes
  (test-case (can-fast-io-round-trip? (identity-hash)))
  (test-case (can-fast-io-round-trip? (identity-hash :a 1)))
  (test-case (can-fast-io-round-trip? (identity-hash :a 1 :b 2)))
  (test-case (can-fast-io-round-trip? (identity-hash :a 1 :b 2 :c 3 :d 4)))

  (test-case (can-fast-io-round-trip? {}))
  (test-case (can-fast-io-round-trip? '{a 1}))
  (test-case (can-fast-io-round-trip? '{a 1 b 2}))
  (test-case (can-fast-io-round-trip? '{a 1 b 2 c 3 d 4})))



