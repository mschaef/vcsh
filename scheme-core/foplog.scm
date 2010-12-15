
(define-package "foplog"
  (:uses "scheme")
  (:exports "fop-histogram"
            "fop-pair-histogram"))

(define %foplog #.(scheme::%subr-by-name "%foplog" #L0(error "foplog support not found")))

(define (opcode->name op)
  (if op
      (mvbind (opcode args) (scheme::parse-fast-op op) opcode)
      #f))

(define (foplog)
  (map opcode->name (vector->list (%foplog))))

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

