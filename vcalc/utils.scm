;;;; utils.scm
;;;; Mike Schaeffer
;;
;; vCalc Utility functions

(define point make-rectangular)
(define point? complex?)
(define point-x real-part)
(define point-y imag-part)

(define (cross-image sx sy)
  (let ((img (make-image (point sx sy))))
    (image-open! img)
    (set-foreground-color! (rgb-color 1 1 1) img)
    (draw-line (point 0 0) (point sx sy) img)
    (draw-line (point sx 0) (point 0 sy) img)
    (image-close! img)
    img))

(define (set-color! c :optional s)
  (set-foreground-color! c s)
  (set-background-color! c s))

(defmacro (with-open-image s . forms)
  `(dynamic-let ((*current-image* ,s))
     ,@forms))

(defmacro (with-image s . forms)
  `(with-open-image ,s
     (unwind-protect
      (lambda ()
	(image-open! *current-image*)
	,@forms)
      (lambda ()
	(image-close! *current-image*)))))

(defmacro (with-window-image w . forms)
  `(with-image (slot-ref ,w 'surface)  ,@forms))

(defmacro (with-new-image size . forms)
  (with-gensyms (surface-sym)
    `(let ((,surface-sym (image ,size)))
       (with-image ,surface-sym ,@forms)
       ,surface-sym)))

(define (display-to-string obj)
  (format #f "~a" obj))

(define (write-to-string obj)
  (format #f "~s" obj))