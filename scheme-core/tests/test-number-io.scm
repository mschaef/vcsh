(use-package! "unit-test")

(define-test number-io
  (check (can-read/write-round-trip? #iposinf))
  (check (can-read/write-round-trip? #ineginf))
  (check (can-read/write-round-trip? #inan))
  (check (can-read/write-round-trip? 1))
  (check (can-read/write-round-trip? 100))
  (check (can-read/write-round-trip? -100))
  (check (can-read/write-round-trip? 1.0))
  (check (can-read/write-round-trip? 100.0))
  (check (can-read/write-round-trip? -100.0))
  (check (can-read/write-round-trip? 1e1))
  (check (can-read/write-round-trip? 100e1))
  (check (can-read/write-round-trip? -100e1)))
