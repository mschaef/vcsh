
(define (frame-ref frp ofs)
  (%memref (+ frp (* ofs (system-info :size-of-lref)))))

(define (frame-lref frp ofs)
  (%sysob (frame-ref frp ofs)))

(define (frame-link frp)
  (let ((next-frp (frame-ref frp system::FOFS_LINK)))
    (if (= next-frp 0)
        #f
      next-frp)))

(define (frame-type frp)
  (case (frame-ref frp system::FOFS_FTYPE)
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
         (hash-set! frame :subr          (%sysob (frame-ref frp system::FOFS_SUBR_SUBR))))
        ((system::FRAME_EVAL)
         (hash-set! frame :environment   (%sysob (frame-ref frp system::FOFS_EVAL_ENV)))
         (hash-set! frame :initial-form  (%sysob (frame-ref frp system::FOFS_EVAL_IFORM)))
         (hash-set! frame :current-form  (%sysob (%memref (frame-ref frp system::FOFS_EVAL_FORM_PTR)))))
        ((system::FRAME_UNWIND)
         (hash-set! frame :after-thunk   (%sysob (frame-ref frp system::FOFS_UNWIND_AFTER))))
        ((system::FRAME_ESCAPE)
         (hash-set! frame :tag           (%sysob (frame-ref frp system::FOFS_ESCAPE_TAG)))
         (hash-set! frame :escape-frp    (frame-ref frp system::FOFS_ESCAPE_FRAME))
         )))
    frame))

(define (fold-frames kons knil frp)
  (let loop ((knil knil) (frp frp))
    (if frp
        (loop (kons frp knil) (frame-link frp))
        knil)))
