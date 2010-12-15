
(define-package "foplog"
  (:uses "scheme")
  (:exports "foplog"
            "foplog-hist"
            "foplog-two-hist"))

(define %foplog #.(scheme::%subr-by-name "%foplog" #L0(error "foplog support not found")))

(define (opcode->name opcode)
  (if (= opcode -1)
      #f
      (aif (hash-ref scheme::*fast-op-opcodes* opcode #f)
           (scheme::fast-op-definition-name it)
           (format #f "<invalid:~a>" opcode))))

(define (foplog)
  (map opcode->name (vector->list (%foplog))))

(define (foplog-two-seqs log)
  (map-pair #L(cons (car _) (cadr _)) log))

(define (hist log)
  (let ((counts (make-hash :equal)))
    (dolist (entry log)
      (hash-set! counts entry (+ 1 (hash-ref counts entry 0))))
    counts))

(define (foplog-hist)
  (hist (foplog)))

(define (foplog-two-hist)
  (hist (foplog-two-seqs (foplog))))

