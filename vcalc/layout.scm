

(define-structure lseg
  (size :default 0)
  (allocation :default 0)
  (stretch :default 0)
  (shrink :default 0))

(define (measure-lsegs lsegs)
  "Given a list of lsegs, return the total size, stretch, and shrink."
  (let loop ((lsegs lsegs)
             (total-size 0)
             (total-stretch 0)
             (total-shrink 0))
    (if (null-list? lsegs)
        (values total-size total-stretch total-shrink)
        (let ((lseg (car lsegs)))
          (loop (cdr lsegs)
                (+ total-size (lseg-size lseg))
                (+ total-stretch (lseg-stretch lseg))
                (+ total-shrink (lseg-shrink lseg)))))))

(define (assign-lseg-allocations! lsegs target-size)
  (values-bind (measure-lsegs lsegs) (total-size total-stretch total-shrink)
    (let* ((initial-size-error (- total-size target-size))
           (total-adj (if (> initial-size-error 0) total-shrink total-stretch)))
      (define (lseg-scale-factor lseg)
        (cond ((= total-adj 0)
               0)
              ((> initial-size-error 0)
               (/ (lseg-shrink lseg) total-adj))
              (#t
               (/ (lseg-stretch lseg) total-adj))))
      (values lsegs
              (-  (fold (lambda (lseg total-allocation)
                          (let ((allocation (+ (lseg-size lseg) (* (lseg-scale-factor lseg)
                                                                   (- initial-size-error)))))
                            (set-lseg-allocation! lseg allocation)
                            (+ total-allocation allocation)))
                        0
                        lsegs)
                  target-size)))))