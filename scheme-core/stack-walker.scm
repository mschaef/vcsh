
(define (frame-ref frp ofs ref-type)
  (let ((raw-value (%memref (+ frp (* ofs (system-info :size-of-lref))))))
    (case ref-type
      ((:raw) raw-value)
      ((:lref) (%sysob raw-value))
      ((:lref-ptr) (%sysob (%memref raw-value)))
      (#t (error "Bad ref-type in frame-ref: ~s" ref-type)))))

(define (frame-lref frp ofs)
  (%sysob (frame-ref frp ofs)))

(define (frame-link frp)
  (let ((next-frp (frame-ref frp system::FOFS_LINK :raw)))
    (if (= next-frp 0)
        #f
      next-frp)))

(define (frame-type frp)
  (case (frame-ref frp system::FOFS_FTYPE :raw)
    ((#.system::FRAME_SUBR  ) 'system::FRAME_SUBR  )
    ((#.system::FRAME_EVAL  ) 'system::FRAME_EVAL  )
    ((#.system::FRAME_ESCAPE) 'system::FRAME_ESCAPE)
    ((#.system::FRAME_UNWIND) 'system::FRAME_UNWIND)
    (#t #f)))

(define (frame-decode frp)
  (let ((frame (make-hash :eq)))
    (hash-set! frame :frp frp)
    (let ((ftype (frame-type frp)))
      (hash-set! frame :frame-type ftype)
      (case ftype
        ((system::FRAME_SUBR)
         (hash-set! frame :subr         (frame-ref frp system::FOFS_SUBR_SUBR :lref)))
        ((system::FRAME_EVAL)
         (hash-set! frame :environment  (frame-ref frp system::FOFS_EVAL_ENV :lref))
         (hash-set! frame :initial-form (frame-ref frp system::FOFS_EVAL_IFORM :lref))
         (hash-set! frame :current-form (frame-ref frp system::FOFS_EVAL_FORM_PTR :lref-ptr)))
        ((system::FRAME_UNWIND)
         (hash-set! frame :after-thunk  (frame-ref frp system::FOFS_UNWIND_AFTER :lref)))
        ((system::FRAME_ESCAPE)
         (hash-set! frame :tag          (frame-ref frp system::FOFS_ESCAPE_TAG :lref))
         (hash-set! frame :escape-frp   (frame-ref frp system::FOFS_ESCAPE_FRAME :raw)))))
    frame))

(define (fold-frames kons knil frp)
  (let loop ((knil knil) (frp frp))
    (if frp
        (loop (kons frp knil) (frame-link frp))
        knil)))


(define (fold-decoded-frames kons knil frp)
  (fold-frames (lambda (frp rest)
                 (kons (frame-decode frp) rest))
               knil
               frp))

(define (capture-stack frp)
  (fold-decoded-frames (lambda (frame rest)
                         (cons frame rest))
                       ()
                       frp))