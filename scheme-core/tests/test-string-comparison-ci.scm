(use-package! "unit-test")

(define-test string-comparison-ci
  (check (eq? #t (string<-ci "alpha" "omega")))
  (check (eq? #f (string<-ci "alpha" "alpha")))
  (check (eq? #f (string<-ci "omega" "alpha")))

  (check (eq? #t (string<=-ci "alpha" "omega")))
  (check (eq? #t (string<=-ci "alpha" "alpha")))
  (check (eq? #f (string<=-ci "omega" "alpha")))

  (check (eq? #f (string=-ci "alpha" "omega")))
  (check (eq? #t (string=-ci "alpha" "alpha")))
  (check (eq? #f (string=-ci "omega" "alpha")))

  (check (eq? #f (string>-ci "alpha" "omega")))
  (check (eq? #f (string>-ci "alpha" "alpha")))
  (check (eq? #t (string>-ci "omega" "alpha")))

  (check (eq? #f (string>=-ci "alpha" "omega")))
  (check (eq? #t (string>=-ci "alpha" "alpha")))
  (check (eq? #t (string>=-ci "omega" "alpha")))

  (check (eq? #t (string!=-ci "alpha" "omega")))
  (check (eq? #f (string!=-ci "alpha" "alpha")))
  (check (eq? #t (string!=-ci "omega" "alpha")))

  (check (eq? #t (string<-ci "ALPHA" "OMEGA")))
  (check (eq? #f (string<-ci "ALPHA" "ALPHA")))
  (check (eq? #f (string<-ci "OMEGA" "ALPHA")))

  (check (eq? #t (string<=-ci "ALPHA" "OMEGA")))
  (check (eq? #t (string<=-ci "ALPHA" "ALPHA")))
  (check (eq? #f (string<=-ci "OMEGA" "ALPHA")))

  (check (eq? #f (string=-ci "ALPHA" "OMEGA")))
  (check (eq? #t (string=-ci "ALPHA" "ALPHA")))
  (check (eq? #f (string=-ci "OMEGA" "ALPHA")))

  (check (eq? #f (string>-ci "ALPHA" "OMEGA")))
  (check (eq? #f (string>-ci "ALPHA" "ALPHA")))
  (check (eq? #t (string>-ci "OMEGA" "ALPHA")))

  (check (eq? #f (string>=-ci "ALPHA" "OMEGA")))
  (check (eq? #t (string>=-ci "ALPHA" "ALPHA")))
  (check (eq? #t (string>=-ci "OMEGA" "ALPHA")))

  (check (eq? #t (string!=-ci "ALPHA" "OMEGA")))
  (check (eq? #f (string!=-ci "ALPHA" "ALPHA")))
  (check (eq? #t (string!=-ci "OMEGA" "ALPHA")))

  (check (eq? #t (string<-ci "alpha" "OMEGA")))
  (check (eq? #f (string<-ci "alpha" "ALPHA")))
  (check (eq? #f (string<-ci "omega" "ALPHA")))

  (check (eq? #t (string<=-ci "alpha" "OMEGA")))
  (check (eq? #t (string<=-ci "alpha" "ALPHA")))
  (check (eq? #f (string<=-ci "omega" "ALPHA")))

  (check (eq? #f (string=-ci "alpha" "OMEGA")))
  (check (eq? #t (string=-ci "alpha" "ALPHA")))
  (check (eq? #f (string=-ci "omega" "ALPHA")))

  (check (eq? #f (string>-ci "alpha" "OMEGA")))
  (check (eq? #f (string>-ci "alpha" "ALPHA")))
  (check (eq? #t (string>-ci "omega" "ALPHA")))

  (check (eq? #f (string>=-ci "alpha" "OMEGA")))
  (check (eq? #t (string>=-ci "alpha" "ALPHA")))
  (check (eq? #t (string>=-ci "omega" "ALPHA")))

  (check (eq? #t (string!=-ci "alpha" "OMEGA")))
  (check (eq? #f (string!=-ci "alpha" "ALPHA")))
  (check (eq? #t (string!=-ci "omega" "ALPHA")))

  (check (eq? #t (string<-ci "ALPHA" "omega")))
  (check (eq? #f (string<-ci "ALPHA" "alpha")))
  (check (eq? #f (string<-ci "OMEGA" "alpha")))

  (check (eq? #t (string<=-ci "ALPHA" "omega")))
  (check (eq? #t (string<=-ci "ALPHA" "alpha")))
  (check (eq? #f (string<=-ci "OMEGA" "alpha")))

  (check (eq? #f (string=-ci "ALPHA" "omega")))
  (check (eq? #t (string=-ci "ALPHA" "alpha")))
  (check (eq? #f (string=-ci "OMEGA" "alpha")))

  (check (eq? #f (string>-ci "ALPHA" "omega")))
  (check (eq? #f (string>-ci "ALPHA" "alpha")))
  (check (eq? #t (string>-ci "OMEGA" "alpha")))

  (check (eq? #f (string>=-ci "ALPHA" "omega")))
  (check (eq? #t (string>=-ci "ALPHA" "alpha")))
  (check (eq? #t (string>=-ci "OMEGA" "alpha")))

  (check (eq? #t (string!=-ci "ALPHA" "omega")))
  (check (eq? #f (string!=-ci "ALPHA" "alpha")))
  (check (eq? #t (string!=-ci "OMEGA" "alpha"))))
