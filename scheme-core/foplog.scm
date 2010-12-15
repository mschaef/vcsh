
(define-package "foplog"
  (:uses "scheme")
  (:exports "foplog"
            "foplog-hist"
            "foplog-two-hist"))

(define %foplog #.(scheme::%subr-by-name "%foplog" #L0(error "foplog support not found")))

(define (opcode->name op)
  (if op
      (mvbind (opcode args) (scheme::parse-fast-op op) opcode)
      #f))

(define (foplog)
  (map opcode->name (vector->list (%foplog))))

(define (foplog-two-seqs log)
  (map-pair #L(cons (car _) (cadr _)) log))

(define (hist log)
  (let ((counts (make-hash :equal)))
    (dolist (entry log)
      (hash-set! counts entry (+ 1 (hash-ref counts entry 0))))
    (qsort (hash->a-list counts) > cdr)))

(define (foplog-hist)
  (hist (foplog)))

(define (foplog-two-hist)
 (hist (foplog-two-seqs (foplog))))

