(use-package! "unit-test")

(define-test equal?-simple-hash
  (test-case (equal? #h(:eq) #h(:eq)))
  (test-case (equal? #h(:equal) #h(:equal)))
  (test-case (equal? #h(:eq a 1) #h(:eq a 1)))
  (test-case (equal? #h(:equal a 1) #h(:equal a 1)))
  (test-case (equal? #h(:eq a 1 b 12) #h(:eq a 1 b 12)))
  (test-case (equal? #h(:equal a 1 b 12) #h(:equal a 1 b 12)))
  (test-case (equal? #h(:equal 2943 a 2321 b) #h(:equal 2321 b 2943 a)))
  (test-case (equal? #h(:equal (h e l l o - w o r l d) 123
			   (f r o b o z z l e) 23
			   #(1 2 3 4 2 2 3) 23)
		 #h(:equal(f r o b o z z l e) 23
			  #(1 2 3 4 2 2 3) 23
			  (h e l l o - w o r l d) 123)))
  )

