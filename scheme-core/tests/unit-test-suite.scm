
(define-package "unit-test-suite"
  (:requires "unit-test")
  (:uses "scheme" "unit-test"))

#.`(begin
     ,@(map #L(list 'include _) (directory "test*.scm")))

(define (run)
  (if (time (test))
      0
      1))