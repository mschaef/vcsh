;;;; character.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;
;; Character library procedures

;;; Comparison predicates
;;;
;;; !! - extend char comparison predicates to >=2 paramaters

;; TODO: char->integer throws a type error if given a non-character. This error is
;; also thrown by these predicates, which should not happen.

(define (char=? x y)
  (eq? x y))

(define (char<? x y)
  (< x y))

(define (char>? x y)
  (> x y))

(define (char<=? x y)
  (<= x y))

(define (char>=? x y)
  (>= x y))

(define (char-ci=? x y)
  (char=? (char-upcase x) (char-upcase y)))

(define (char-ci<? x y)
  (char<? (char-upcase x) (char-upcase y)))

(define (char-ci>? x y)
  (char>? (char-upcase x) (char-upcase y)))

(define (char-ci<=? x y)
   (char<=? (char-upcase x) (char-upcase y)))

(define (char-ci>=? x y)
   (char>=? (char-upcase x) (char-upcase y)))

;;; Character classification


(define (charset-vector . chars)
  "Return a vector with a boolean element for each valid character. Characters
   in the string(s) <chars> are set to #t, all others are #f."
  (let ((vec (make-vector (char->integer (hash-ref (system-info)
                                                   :most-positive-character))
                          #f)))
    (dolist (chars chars)
      (check string? chars)
      (dotimes (ii (length chars))
        (vector-set! vec (string-ref chars ii) #t)))
    vec))


(define (char-alphabetic? x)
 "Returns #t if <char> is an alphabetic letter, #f otherwise."
  (and (char? x)
       (vector-ref #.(charset-vector "abcdefghijklmnopqrstuvwxyz"
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                   x)))

(define (char-alphanumeric? x)
 "Returns #t if <char> is an alphabetic or numeric letter, #f otherwise."
  (and (char? x)
       (vector-ref #.(charset-vector "abcdefghijklmnopqrstuvwxyz"
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                     "0123456789")
                   x)))

(define (char-numeric? x)
 "Returns #t if <char> is an arabic numeral, #f otherwise."
  (and (char? x)
       (vector-ref #.(charset-vector "0123456789") x)))


(define (char-whitespace? x)
 "Returns #t if <char> is a whitespace character, #f otherwise."
  (and (char? x)
       (member x '(#\newline #\cr #\tab #\space))))

(define (char-upper-case? x)
 "Returns #t if <char> is a upper case alphabetic letter, #f
  otherwise."
  (and (char? x)
       (vector-ref #.(charset-vector "ABCDEFGHIJKLMNOPQRSTUVWXYZ") x)))

(define (char-lower-case? x)
 "Returns #t if <char> is a lower case alphabetic letter, #f
  otherwise."
  (and (char? x)
       (vector-ref #.(charset-vector "abcdefghijklmnopqrstuvwxyz") x)))

(define (char-octal? char)
 "Returns #t if <char> is a valid character for a octal number, #f
  otherwise."
  (and (char? char)
       (vector-ref #.(charset-vector "01234567") char)))

(define (char-hex? char)
 "Returns #t if <char> is a valid character for a hexadecimal number, #f
  otherwise."
  (and (char? char)
       (vector-ref #.(charset-vector "0123456789abcdefABCDEF") char)))

;;; Character case conversion

(define *uc-lc-offset* (- (char->integer #\a)
                          (char->integer #\A)))

(define (char-upcase x)
  (check char? x)
  (if (char-lower-case? x)
     (integer->char (- (char->integer x) *uc-lc-offset*))
   x))

(define (char-downcase x)
  (check char? x)
  (if (char-upper-case? x)
     (integer->char (+ (char->integer x) *uc-lc-offset*))
    x))




