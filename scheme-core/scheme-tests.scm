(define-package "unit-test-suite"
  (:requires "unit-test"
             "unit-test-utils")
  (:uses "scheme"
         "unit-test"
         "unit-test-utils"))

#.`(begin
     ,@(map #L(list 'include _)
            (qsort (directory "tests/test*.scm")
                   (lambda (x y) (> 0 (strcmp x y))))))

(define (run)
  (in-package! "unit-test-suite")
  (if (time (run-tests))
      0
      1))
