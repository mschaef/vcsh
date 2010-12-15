
(define-package "foplog"
  (:uses "scheme")
  (:exports "fop-histogram"
            "fop-pair-histogram"))

(define %foplog-reset #.(scheme::%subr-by-name "%foplog-reset" #L0(error "foplog support not found")))
(define %foplog-enable #.(scheme::%subr-by-name "%foplog-enable" #L0(error "foplog support not found")))
(define %foplog-snapshot #.(scheme::%subr-by-name "%foplog-snapshot" #L0(error "foplog support not found")))

(define (opcode->name op)
  (if (not (null? op))
      (mvbind (opcode args) (scheme::parse-fast-op op) opcode)
      #f))

(define (foplog)
  (map opcode->name (vector->list (%foplog-snapshot))))

(define (foplog-pairs log)
  (map-pair #L(cons (car _) (cadr _)) log))

(define (hist log)
  (let ((counts (make-hash :equal)))
    (dolist (entry log)
      (hash-set! counts entry (+ 1 (hash-ref counts entry 0))))
    (qsort (hash->a-list counts) > cdr)))

(define (fop-histogram)
  (hist (foplog)))

(define (fop-pair-histogram)
 (hist (foplog-pairs (foplog))))

