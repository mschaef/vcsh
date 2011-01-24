
(define-package "unit-test-suite"
  (:requires "unit-test")
  (:uses "scheme" "unit-test"))

#.`(begin
     ,@(map #L(list 'include _)
            (qsort (directory "test*.scm")
                   (lambda (x y) (> 0 (strcmp x y))))))

(define (run)
  (if (time (test))
      0
      1))