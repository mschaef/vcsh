;;;; standard-windows.scm
;;;; Mike Schaeffer
;;
;; vCalc standard window classes.

(define *lisp-windows* ())

(define (all-toplevel-windows)
  (remove #L(slot-ref _ 'parent) *lisp-windows*))

(define (shutdown-windows)
  (dolist (w (all-toplevel-windows))
    [w close]))

(eval-when (:load-toplevel)
  (add-hook-function! '*shutdown-hook* 'shutdown-windows))

(define-proto <lisp-window>
  'initial-title "LispWindow: No title specified."
  'window-type :window
  'parent #f
  'children ()
  'native-peer #f
  'surface #f)

(defmesg <lisp-window> (print-unreadably port)
  (format port "~s ~a~s (~s child)"
          @window-type 
          (if @parent "" "toplevel ")
          @initial-title 
          (length @children)
          (if (> (length @children) 1) "ren" "")))

(defmesg <lisp-window> (init)
  (push! self *lisp-windows*)
  (let ((parent @parent))
    (slot-set! self 'native-peer 
               (%make-window self (if parent @(parent native-peer) ())))
    (when parent
      [parent register-child! self])))

(defmesg <lisp-window> (show)
  (show-window @native-peer)
  self)

(defmesg <lisp-window> (hide)
  (hide-window @native-peer)
  self)

(defmesg <lisp-window> (create . args)
  (let ((ni (apply make-instance self args)))
    [ni init]
    ni))

(defmesg <lisp-window> (register-child! child)
  (when (memq child @children)
    (error "~s is already a child of ~s" child self))
  (slot-set! self 'children (cons child @children))
  self)

(defmesg <lisp-window> (unregister-child! child)
  (unless (memq child @children)
    (error "~s is not a child of ~s" child self))
  (slot-set! self 'children (delete child @children eq?))
  self)

(define *lisp-window-resize-delta* 32)

(defmesg <lisp-window> (on-move x y)
  (dolist (child @children)
    [child parent-repositioned]))

(defmesg <lisp-window> (on-size sx sy)
  (define (round-down x interval)   (* interval (quotient (inexact->exact x) interval)))
  (define (round-up x interval)     (* interval (quotient (+ interval (inexact->exact x)) interval)))

  (slot-set! self 'width sx)
  (slot-set! self 'height sy)

  (let* ((surface (or @surface
                      (let ((img (make-image sx sy)))
                        (slot-set! self 'surface img)
                        img)))
         (surface-size (measure-image surface)))
    (when (or (> sx (point-x surface-size))
              (> sy (point-y surface-size))
              (< sx (round-down (point-x surface-size) *lisp-window-resize-delta*))
              (< sy (round-down (point-y surface-size) *lisp-window-resize-delta*)))
      (slot-set! self 'surface 
                 (cross-image (round-up sx *lisp-window-resize-delta*)
                             (round-up sy *lisp-window-resize-delta*)))
      (awhen @native-peer
        (update-window it))))
  (dolist (child @children)
    [child parent-repositioned]))

(defmesg <lisp-window> (update)
  (update-window @native-peer))

(defmesg <lisp-window> (flush)
  (flush-window @native-peer))

(defmesg <lisp-window> (on-command command arg)
  )

(defmesg <lisp-window> (on-update)
  )

(defmesg <lisp-window> (on-destroy)
  (set! *lisp-windows* (delete self *lisp-windows* eq?))
  (slot-set! self 'native-peer #f))

(defmesg <lisp-window> (close)
  (close-window @native-peer))

(defmesg <lisp-window> (parent-repositioned)
  (parent-repositioned @native-peer))

(defmesg <lisp-window> (placement)
  (window-placement @native-peer))

(defmesg <lisp-window> (set-placement! placement)
  (set-window-placement! @native-peer placement))

(defmesg <lisp-window> (choose choose-list title description)
  (choose @native-peer choose-list title description))

(define-proto (<drawer> <lisp-window>)
  'initial-title "LispWindow: No title specified."
  'window-type :drawer)



