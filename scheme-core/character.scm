
;;;; character.scm --
;;;;
;;;; Character library procedures.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;;; Character classification

(define (charset-vector . chars)
  "Return a vector that represents the set of characters specified by the input
   arguments <chars>. The output vector has an element, numbered by character
   ordinal, for each valid character. The vector value for each character is
   #t if the character is in the set and #f otherwise. The input argumnents <chars>
   can be specified as characters, strings, or other charset-vectors."
  (let ((vec (make-vector (char->integer (hash-ref (system-info) :most-positive-character))
                          #f)))
    (dolist (chars chars)
      (etypecase chars
        ((character)
         (vector-set! vec chars #t))
        ((string)
         (dotimes (ii (length chars))
           (vector-set! vec (string-ref chars ii) #t)))
        ((vector)
         (dotimes (ii (length chars))
           (when (vector-ref chars ii)
             (vector-set! vec ii #t))))))
    vec))


(define *charset-lowercase-alphabetic*
  '#.(charset-vector "abcdefghijklmnopqrstuvwxyz"))

(define *charset-uppercase-alphabetic*
  '#.(charset-vector "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define *charset-alphabetic*
  '#.(charset-vector *charset-lowercase-alphabetic*
                     *charset-uppercase-alphabetic*))

(define *charset-numeric*
  '#.(charset-vector "0123456789"))

(define *charset-alphanumeric*
  '#.(charset-vector *charset-alphabetic*
                     *charset-numeric*))

(define *charset-whitespace*
  '#.(charset-vector #\newline #\cr #\tab #\space))

(define *charset-hex*
  '#.(charset-vector "0123456789abcdefABCDEF"))

(define *charset-octal*
  '#.(charset-vector "01234567"))

(define (char-alphabetic? x)
 "Returns#t if <char> is an alphabetic letter, #f otherwise."
  (and (char? x)
       (vector-ref *charset-alphabetic* x)))

(define (char-alphanumeric? x)
 "Returns #t if <char> is an alphabetic or numeric letter, #f otherwise."
  (and (char? x)
       (vector-ref *charset-alphanumeric* x)))

(define (char-numeric? x)
 "Returns #t if <char> is an arabic numeral, #f otherwise."
  (and (char? x)
       (vector-ref *charset-numeric* x)))

(define (char-whitespace? x)
 "Returns #t if <char> is a whitespace character, #f otherwise."
  (and (char? x)
       (vector-ref *charset-whitespace* x)))

(define (char-upper-case? x)
 "Returns #t if <char> is a upper case alphabetic letter, #f
  otherwise."
  (and (char? x)
       (vector-ref *charset-uppercase-alphabetic* x)))

(define (char-lower-case? x)
 "Returns #t if <char> is a lower case alphabetic letter, #f
  otherwise."
  (and (char? x)
       (vector-ref *charset-lowercase-alphabetic* x)))

(define (char-octal? char)
 "Returns #t if <char> is a valid character for a octal number, #f
  otherwise."
  (and (char? char)
       (vector-ref *charset-octal* char)))

(define (char-hex? char)
 "Returns #t if <char> is a valid character for a hexadecimal number, #f
  otherwise."
  (and (char? char)
       (vector-ref *charset-hex* char)))

;;; Character case conversion

(define *uc-lc-offset* (- (char->integer #\a)
                          (char->integer #\A)))

(define (char-upcase x)
  (runtime-check char? x)
  (if (char-lower-case? x)
     (integer->char (- (char->integer x) *uc-lc-offset*))
   x))

(define (char-downcase x)
  (runtime-check char? x)
  (if (char-upper-case? x)
     (integer->char (+ (char->integer x) *uc-lc-offset*))
    x))

;;; Comparison predicates
;;;
;;; REVISIT - extend char comparison predicates to >=2 paramaters

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





