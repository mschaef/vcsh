(define-package "unit-test-test"
  (:uses "scheme"
         "unit-test"))

(define-test toplevel/error
  (+ 1 :invalid-argument))

(define-test toplevel/unhandled-abort
  (abort 'unhandled-abort))

(define-test toplevel/uncaught-throw
  (throw 'uncaught-throw))

(define-test test-case/error
  (test-case (+ 1 :invalid-argument)))

(define-test test-case/unhandled-abort
  (test-case (abort 'unhandled-abort)))

(define-test test-case/uncaught-throw
  (test-case (throw 'uncaught-throw)))

(define (run)
  (if (time (test)) 0 1))
