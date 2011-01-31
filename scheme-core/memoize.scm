;;;; memoize.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;;
;;; A function result cache facility.
;;; REVISIT: *memoize-result-caches*  should forget caches for functions that get finalized. (Which requires weak references.)

(define *memoize-result-caches* '())

(define *memoize-results?* #t)

(define (forget-all-memoized-results)
  (map hash-clear! *memoize-result-caches*))

(define (memoize f)
  "Returns a memoized version of the procedure <f>.  The memoized version
   of the function will remember its result for each time it is called with
   a given set of arguments. If the function is ever called with that set of
   arguments again, the memoized function will return the remembered value
   rather than recompute the value a second time."
  (let ((no-cached-result (gensym))
        (result-cache (make-hash)))
    (push! result-cache *memoize-result-caches*)
    (lambda arguments
      (if *memoize-results?*
          (let ((cached-result (hash-ref result-cache arguments no-cached-result)))
            (if (eq? cached-result no-cached-result)
                (let ((result (apply f arguments)))
                  (hash-set! result-cache arguments result)
                  result)
                cached-result))
          (apply f arguments)))))
