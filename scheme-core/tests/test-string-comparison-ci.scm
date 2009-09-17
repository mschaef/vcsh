(use-package! "unit-test")

(define-test string-comparison-ci
  (test-case (eq? #t (string<-ci "alpha" "omega")))
  (test-case (eq? #f (string<-ci "alpha" "alpha")))
  (test-case (eq? #f (string<-ci "omega" "alpha")))

  (test-case (eq? #t (string<=-ci "alpha" "omega")))
  (test-case (eq? #t (string<=-ci "alpha" "alpha")))
  (test-case (eq? #f (string<=-ci "omega" "alpha")))

  (test-case (eq? #f (string=-ci "alpha" "omega")))
  (test-case (eq? #t (string=-ci "alpha" "alpha")))
  (test-case (eq? #f (string=-ci "omega" "alpha")))

  (test-case (eq? #f (string>-ci "alpha" "omega")))
  (test-case (eq? #f (string>-ci "alpha" "alpha")))
  (test-case (eq? #t (string>-ci "omega" "alpha")))

  (test-case (eq? #f (string>=-ci "alpha" "omega")))
  (test-case (eq? #t (string>=-ci "alpha" "alpha")))
  (test-case (eq? #t (string>=-ci "omega" "alpha")))

  (test-case (eq? #t (string!=-ci "alpha" "omega")))
  (test-case (eq? #f (string!=-ci "alpha" "alpha")))
  (test-case (eq? #t (string!=-ci "omega" "alpha")))

  (test-case (eq? #t (string<-ci "ALPHA" "OMEGA")))
  (test-case (eq? #f (string<-ci "ALPHA" "ALPHA")))
  (test-case (eq? #f (string<-ci "OMEGA" "ALPHA")))

  (test-case (eq? #t (string<=-ci "ALPHA" "OMEGA")))
  (test-case (eq? #t (string<=-ci "ALPHA" "ALPHA")))
  (test-case (eq? #f (string<=-ci "OMEGA" "ALPHA")))

  (test-case (eq? #f (string=-ci "ALPHA" "OMEGA")))
  (test-case (eq? #t (string=-ci "ALPHA" "ALPHA")))
  (test-case (eq? #f (string=-ci "OMEGA" "ALPHA")))

  (test-case (eq? #f (string>-ci "ALPHA" "OMEGA")))
  (test-case (eq? #f (string>-ci "ALPHA" "ALPHA")))
  (test-case (eq? #t (string>-ci "OMEGA" "ALPHA")))

  (test-case (eq? #f (string>=-ci "ALPHA" "OMEGA")))
  (test-case (eq? #t (string>=-ci "ALPHA" "ALPHA")))
  (test-case (eq? #t (string>=-ci "OMEGA" "ALPHA")))

  (test-case (eq? #t (string!=-ci "ALPHA" "OMEGA")))
  (test-case (eq? #f (string!=-ci "ALPHA" "ALPHA")))
  (test-case (eq? #t (string!=-ci "OMEGA" "ALPHA")))

  (test-case (eq? #t (string<-ci "alpha" "OMEGA")))
  (test-case (eq? #f (string<-ci "alpha" "ALPHA")))
  (test-case (eq? #f (string<-ci "omega" "ALPHA")))

  (test-case (eq? #t (string<=-ci "alpha" "OMEGA")))
  (test-case (eq? #t (string<=-ci "alpha" "ALPHA")))
  (test-case (eq? #f (string<=-ci "omega" "ALPHA")))

  (test-case (eq? #f (string=-ci "alpha" "OMEGA")))
  (test-case (eq? #t (string=-ci "alpha" "ALPHA")))
  (test-case (eq? #f (string=-ci "omega" "ALPHA")))

  (test-case (eq? #f (string>-ci "alpha" "OMEGA")))
  (test-case (eq? #f (string>-ci "alpha" "ALPHA")))
  (test-case (eq? #t (string>-ci "omega" "ALPHA")))

  (test-case (eq? #f (string>=-ci "alpha" "OMEGA")))
  (test-case (eq? #t (string>=-ci "alpha" "ALPHA")))
  (test-case (eq? #t (string>=-ci "omega" "ALPHA")))

  (test-case (eq? #t (string!=-ci "alpha" "OMEGA")))
  (test-case (eq? #f (string!=-ci "alpha" "ALPHA")))
  (test-case (eq? #t (string!=-ci "omega" "ALPHA")))

  (test-case (eq? #t (string<-ci "ALPHA" "omega")))
  (test-case (eq? #f (string<-ci "ALPHA" "alpha")))
  (test-case (eq? #f (string<-ci "OMEGA" "alpha")))

  (test-case (eq? #t (string<=-ci "ALPHA" "omega")))
  (test-case (eq? #t (string<=-ci "ALPHA" "alpha")))
  (test-case (eq? #f (string<=-ci "OMEGA" "alpha")))

  (test-case (eq? #f (string=-ci "ALPHA" "omega")))
  (test-case (eq? #t (string=-ci "ALPHA" "alpha")))
  (test-case (eq? #f (string=-ci "OMEGA" "alpha")))

  (test-case (eq? #f (string>-ci "ALPHA" "omega")))
  (test-case (eq? #f (string>-ci "ALPHA" "alpha")))
  (test-case (eq? #t (string>-ci "OMEGA" "alpha")))

  (test-case (eq? #f (string>=-ci "ALPHA" "omega")))
  (test-case (eq? #t (string>=-ci "ALPHA" "alpha")))
  (test-case (eq? #t (string>=-ci "OMEGA" "alpha")))

  (test-case (eq? #t (string!=-ci "ALPHA" "omega")))
  (test-case (eq? #f (string!=-ci "ALPHA" "alpha")))
  (test-case (eq? #t (string!=-ci "OMEGA" "alpha"))))
