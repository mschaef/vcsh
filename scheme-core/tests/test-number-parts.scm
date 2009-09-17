(use-package! "unit-test")

; Test cases generated with this expression:
;
; (dynamic-let ((*flonum-print-precision* 3))
;   (stable (map (lambda (x)
;                  `(test-case (= ,(eval x) ,x)))
;                (list-combinations '((round truncate floor ceiling)
;                                     (-20 -1 0 1 20
;                                          -20.9 -20.6 -20.5 -20.4 -20.1 -20
;                                          -1.9 -1.6 -1.5 -1.4 -1.1 -1.0
;                                          -0.9 -0.6 -0.5 -0.4 -0.1 -0.0
;                                          20.9 20.6 20.5 20.4 20.1 20
;                                          1.9 1.6 1.5 1.4 1.1 1.0
;                                          0.9 0.6 0.5 0.4 0.1))))))

(define-test number-parts
  (test-case (runtime-error? (round :non-number)))
  (test-case (runtime-error? (ceiling :non-number)))
  (test-case (runtime-error? (floor :non-number)))
  (test-case (runtime-error? (truncate :non-number)))
        
  (test-case (= -20 (round -20)))
  (test-case (= -1 (round -1)))
  (test-case (= 0 (round 0)))
  (test-case (= 1 (round 1)))
  (test-case (= 20 (round 20)))
  (test-case (= -21.000 (round -20.900)))
  (test-case (= -21.000 (round -20.600)))
  (test-case (= -21.000 (round -20.500)))
  (test-case (= -20.000 (round -20.400)))
  (test-case (= -20.000 (round -20.100)))
  (test-case (= -20 (round -20)))
  (test-case (= -2.000 (round -1.900)))
  (test-case (= -2.000 (round -1.600)))
  (test-case (= -2.000 (round -1.500)))
  (test-case (= -1.000 (round -1.400)))
  (test-case (= -1.000 (round -1.100)))
  (test-case (= -1.000 (round -1.000)))
  (test-case (= -1.000 (round -0.900)))
  (test-case (= -1.000 (round -0.600)))
  (test-case (= -1.000 (round -0.500)))
  (test-case (= -0.000 (round -0.400)))
  (test-case (= -0.000 (round -0.100)))
  (test-case (= -0.000 (round -0.000)))
  (test-case (= 21.000 (round 20.900)))
  (test-case (= 21.000 (round 20.600)))
  (test-case (= 21.000 (round 20.500)))
  (test-case (= 20.000 (round 20.400)))
  (test-case (= 20.000 (round 20.100)))
  (test-case (= 20 (round 20)))
  (test-case (= 2.000 (round 1.900)))
  (test-case (= 2.000 (round 1.600)))
  (test-case (= 2.000 (round 1.500)))
  (test-case (= 1.000 (round 1.400)))
  (test-case (= 1.000 (round 1.100)))
  (test-case (= 1.000 (round 1.000)))
  (test-case (= 1.000 (round 0.900)))
  (test-case (= 1.000 (round 0.600)))
  (test-case (= 1.000 (round 0.500)))
  (test-case (= 0.000 (round 0.400)))
  (test-case (= 0.000 (round 0.100)))
  (test-case (= -20 (truncate -20)))
  (test-case (= -1 (truncate -1)))
  (test-case (= 0 (truncate 0)))
  (test-case (= 1 (truncate 1)))
  (test-case (= 20 (truncate 20)))
  (test-case (= -20.000 (truncate -20.900)))
  (test-case (= -20.000 (truncate -20.600)))
  (test-case (= -20.000 (truncate -20.500)))
  (test-case (= -20.000 (truncate -20.400)))
  (test-case (= -20.000 (truncate -20.100)))
  (test-case (= -20 (truncate -20)))
  (test-case (= -1.000 (truncate -1.900)))
  (test-case (= -1.000 (truncate -1.600)))
  (test-case (= -1.000 (truncate -1.500)))
  (test-case (= -1.000 (truncate -1.400)))
  (test-case (= -1.000 (truncate -1.100)))
  (test-case (= -1.000 (truncate -1.000)))
  (test-case (= -0.000 (truncate -0.900)))
  (test-case (= -0.000 (truncate -0.600)))
  (test-case (= -0.000 (truncate -0.500)))
  (test-case (= -0.000 (truncate -0.400)))
  (test-case (= -0.000 (truncate -0.100)))
  (test-case (= -0.000 (truncate -0.000)))
  (test-case (= 20.000 (truncate 20.900)))
  (test-case (= 20.000 (truncate 20.600)))
  (test-case (= 20.000 (truncate 20.500)))
  (test-case (= 20.000 (truncate 20.400)))
  (test-case (= 20.000 (truncate 20.100)))
  (test-case (= 20 (truncate 20)))
  (test-case (= 1.000 (truncate 1.900)))
  (test-case (= 1.000 (truncate 1.600)))
  (test-case (= 1.000 (truncate 1.500)))
  (test-case (= 1.000 (truncate 1.400)))
  (test-case (= 1.000 (truncate 1.100)))
  (test-case (= 1.000 (truncate 1.000)))
  (test-case (= 0.000 (truncate 0.900)))
  (test-case (= 0.000 (truncate 0.600)))
  (test-case (= 0.000 (truncate 0.500)))
  (test-case (= 0.000 (truncate 0.400)))
  (test-case (= 0.000 (truncate 0.100)))
  (test-case (= -20 (floor -20)))
  (test-case (= -1 (floor -1)))
  (test-case (= 0 (floor 0)))
  (test-case (= 1 (floor 1)))
  (test-case (= 20 (floor 20)))
  (test-case (= -21.000 (floor -20.900)))
  (test-case (= -21.000 (floor -20.600)))
  (test-case (= -21.000 (floor -20.500)))
  (test-case (= -21.000 (floor -20.400)))
  (test-case (= -21.000 (floor -20.100)))
  (test-case (= -20 (floor -20)))
  (test-case (= -2.000 (floor -1.900)))
  (test-case (= -2.000 (floor -1.600)))
  (test-case (= -2.000 (floor -1.500)))
  (test-case (= -2.000 (floor -1.400)))
  (test-case (= -2.000 (floor -1.100)))
  (test-case (= -1.000 (floor -1.000)))
  (test-case (= -1.000 (floor -0.900)))
  (test-case (= -1.000 (floor -0.600)))
  (test-case (= -1.000 (floor -0.500)))
  (test-case (= -1.000 (floor -0.400)))
  (test-case (= -1.000 (floor -0.100)))
  (test-case (= -0.000 (floor -0.000)))
  (test-case (= 20.000 (floor 20.900)))
  (test-case (= 20.000 (floor 20.600)))
  (test-case (= 20.000 (floor 20.500)))
  (test-case (= 20.000 (floor 20.400)))
  (test-case (= 20.000 (floor 20.100)))
  (test-case (= 20 (floor 20)))
  (test-case (= 1.000 (floor 1.900)))
  (test-case (= 1.000 (floor 1.600)))
  (test-case (= 1.000 (floor 1.500)))
  (test-case (= 1.000 (floor 1.400)))
  (test-case (= 1.000 (floor 1.100)))
  (test-case (= 1.000 (floor 1.000)))
  (test-case (= 0.000 (floor 0.900)))
  (test-case (= 0.000 (floor 0.600)))
  (test-case (= 0.000 (floor 0.500)))
  (test-case (= 0.000 (floor 0.400)))
  (test-case (= 0.000 (floor 0.100)))
  (test-case (= -20 (ceiling -20)))
  (test-case (= -1 (ceiling -1)))
  (test-case (= 0 (ceiling 0)))
  (test-case (= 1 (ceiling 1)))
  (test-case (= 20 (ceiling 20)))
  (test-case (= -20.000 (ceiling -20.900)))
  (test-case (= -20.000 (ceiling -20.600)))
  (test-case (= -20.000 (ceiling -20.500)))
  (test-case (= -20.000 (ceiling -20.400)))
  (test-case (= -20.000 (ceiling -20.100)))
  (test-case (= -20 (ceiling -20)))
  (test-case (= -1.000 (ceiling -1.900)))
  (test-case (= -1.000 (ceiling -1.600)))
  (test-case (= -1.000 (ceiling -1.500)))
  (test-case (= -1.000 (ceiling -1.400)))
  (test-case (= -1.000 (ceiling -1.100)))
  (test-case (= -1.000 (ceiling -1.000)))
  (test-case (= -0.000 (ceiling -0.900)))
  (test-case (= -0.000 (ceiling -0.600)))
  (test-case (= -0.000 (ceiling -0.500)))
  (test-case (= -0.000 (ceiling -0.400)))
  (test-case (= -0.000 (ceiling -0.100)))
  (test-case (= -0.000 (ceiling -0.000)))
  (test-case (= 21.000 (ceiling 20.900)))
  (test-case (= 21.000 (ceiling 20.600)))
  (test-case (= 21.000 (ceiling 20.500)))
  (test-case (= 21.000 (ceiling 20.400)))
  (test-case (= 21.000 (ceiling 20.100)))
  (test-case (= 20 (ceiling 20)))
  (test-case (= 2.000 (ceiling 1.900)))
  (test-case (= 2.000 (ceiling 1.600)))
  (test-case (= 2.000 (ceiling 1.500)))
  (test-case (= 2.000 (ceiling 1.400)))
  (test-case (= 2.000 (ceiling 1.100)))
  (test-case (= 1.000 (ceiling 1.000)))
  (test-case (= 1.000 (ceiling 0.900)))
  (test-case (= 1.000 (ceiling 0.600)))
  (test-case (= 1.000 (ceiling 0.500)))
  (test-case (= 1.000 (ceiling 0.400)))
  (test-case (= 1.000 (ceiling 0.100))))


