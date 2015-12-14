(use-package! "unit-test")

(define-test fast-io-hashes
  (check (can-fast-io-round-trip? (identity-hash)))
  (check (can-fast-io-round-trip? (identity-hash :a 1)))
  (check (can-fast-io-round-trip? (identity-hash :a 1 :b 2)))
  (check (can-fast-io-round-trip? (identity-hash :a 1 :b 2 :c 3 :d 4)))

  (check (can-fast-io-round-trip? {}))
  (check (can-fast-io-round-trip? '{a 1}))
  (check (can-fast-io-round-trip? '{a 1 b 2}))
  (check (can-fast-io-round-trip? '{a 1 b 2 c 3 d 4})))



