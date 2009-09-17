(use-package! "unit-test")

(define (can-ieee-754-round-trip? x) 
  (let ((x2 (ieee-754-bits-> (->ieee-754-bits x))))
    (or (and (nan? x) (nan? x2)) ; #inan isn't = to itself, but for our purposes here, it is.
        (= x x2))))


(define-test ieee-754-bits

  ;; Test-Case for error detection
  (test-case (runtime-error? (->ieee-754-bits 'symbol)))
  (test-case (runtime-error? (->ieee-754-bits '())))
  (test-case (runtime-error? (->ieee-754-bits '(1 2))))
  (test-case (runtime-error? (->ieee-754-bits 2+3i)))
  (test-case (not (runtime-error? (->ieee-754-bits 2.0))))
  (test-case (not (runtime-error? (->ieee-754-bits 2))))

  (test-case (runtime-error? (ieee-754-bits-> 'symbol)))
  (test-case (runtime-error? (ieee-754-bits-> '())))
  (test-case (runtime-error? (ieee-754-bits-> '(1 2))))
  (test-case (runtime-error? (ieee-754-bits-> 2+3i)))
  (test-case (runtime-error? (ieee-754-bits-> 2.0)))
  (test-case (not (runtime-error? (ieee-754-bits-> 2))))

  ;; The functions should allow symmetric conversion
  ;; IEEE-754 special cases
  (test-case (can-ieee-754-round-trip? #iposinf))
  (test-case (can-ieee-754-round-trip? #ineginf))
  (test-case (can-ieee-754-round-trip? #inan))
  (test-case (can-ieee-754-round-trip? 0))

   ; Test a few specific bit patterns.
  (test-case (can-ieee-754-round-trip? 3.2))                      ; bits < 0
  (test-case (can-ieee-754-round-trip? -2.0349165139403850e+236)) ; bits = 0xFFFFFFFFFFFFFFF0
  (test-case (can-ieee-754-round-trip? 1.9656826079092814e-236))  ; bits = 0x000000000000000F
  (test-case (can-ieee-754-round-trip? -3.5986963492447479e+230)) ; bits = 0x0123456789ABCDEF
  (test-case (can-ieee-754-round-trip? 1.1806583595659977e-230))  ; bits = 0xFEDCBA9876543210

   ; This just a collection of numbers from (random 100.0)
   ; It might make sense to go for more specific coverage
   ; of bit patterns, etc.
  (test-case (can-ieee-754-round-trip? 4.965132293258917))
  (test-case (can-ieee-754-round-trip? 46.40493760481937))
  (test-case (can-ieee-754-round-trip? 72.7860442109276))
  (test-case (can-ieee-754-round-trip? 67.9922265633868))
  (test-case (can-ieee-754-round-trip? 37.21126898863471))
  (test-case (can-ieee-754-round-trip? 4.998198715036920))
  (test-case (can-ieee-754-round-trip? 16.95575203463039))
  (test-case (can-ieee-754-round-trip? 31.05708149360221))
  (test-case (can-ieee-754-round-trip? 21.24732026946482))
  (test-case (can-ieee-754-round-trip? 48.46499380512628))
  (test-case (can-ieee-754-round-trip? 43.98653520642457))
  (test-case (can-ieee-754-round-trip? 60.5949662258802))
  (test-case (can-ieee-754-round-trip? 40.10726869116283))
  (test-case (can-ieee-754-round-trip? 84.2649606841290))
  (test-case (can-ieee-754-round-trip? 62.3321449964607))
  (test-case (can-ieee-754-round-trip? 5.34839733070545))
  (test-case (can-ieee-754-round-trip? 86.3537194080220))
  (test-case (can-ieee-754-round-trip? 30.64051580674995))
  (test-case (can-ieee-754-round-trip? 2.672157439097909))
  (test-case (can-ieee-754-round-trip? 39.11748848773306))
  )

