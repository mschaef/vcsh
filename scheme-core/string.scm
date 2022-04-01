
;;;; string.scm --
;;;;
;;;; String utility functions
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define (string>  string-1 string-2) (> (strcmp string-1 string-2) 0))
(define (string<  string-1 string-2) (< (strcmp string-1 string-2) 0))
(define (string>= string-1 string-2) (>= (strcmp string-1 string-2) 0))
(define (string<= string-1 string-2) (<= (strcmp string-1 string-2) 0))
(define (string=  string-1 string-2) (= (strcmp string-1 string-2) 0))
(define (string!= string-1 string-2) (not (= (strcmp string-1 string-2) 0)))

(define (string>-ci  string-1 string-2) (> (stricmp string-1 string-2) 0))
(define (string<-ci  string-1 string-2) (< (stricmp string-1 string-2) 0))
(define (string>=-ci string-1 string-2) (>= (stricmp string-1 string-2) 0))
(define (string<=-ci string-1 string-2) (<= (stricmp string-1 string-2) 0))
(define (string=-ci  string-1 string-2) (= (stricmp string-1 string-2) 0))
(define (string!=-ci string-1 string-2) (not (= (stricmp string-1 string-2) 0)))

(define (string-take string count)
  "Returns the <count> leftmost characters of <string>."
  (substring string 0 count))

(define (string-drop string count)
  "Returns all but the <count> leftmost characters of <string>."
  (substring string count))

(define (string-take-right string count)
  "Returns the <count> rightmost characters of <string>."
  (let ((len (length string)))
    (substring string (- len count) len)))

(define (string-drop-right string count)
  "Returns all but the <count> rightmost characters of <string>."
  (substring string 0 (- (length string) count)))

(define (make-string count :optional (obj #\space))
  "Create a string consisting of the display represntation of <obj>
   repeated <count> times."
  (let ((op (open-output-string)))
    (set-port-translate-mode! op #f)
    (if (or (char? obj) (string? obj))
        (repeat count (write-strings op obj))
        (repeat count (display obj op)))
    (get-output-string op)))

(define (pad-to-width obj width)
  "Return the display representation of <obj> padded or truncated
    to <width> characters. If padded, the result will be left justified
    with spaces. If truncated, the leftmost characters will be retained."
  (string-take (string-append (make-string 1 obj)
                              (make-string width #\space))
               width))

(define (string-empty? string)
  "Returns a boolean indicating if <string> is empty."
  (= 0 (string-length string)))

(define (string-fold kons knil str)
  (runtime-check string? str)
  (let loop ((ii 0) (result knil))
    (if (= ii (length str))
        result
        (loop (+ ii 1) (kons (string-ref str ii) result)))))

(define (string->list string)
 "Converts the string <string> into a list of characters orderd
  as they appear in the string."
 (reverse! (string-fold (lambda (char chars)
                          (cons char chars))
                        ()
                        string)))


(define (string-contains-one-of? search-strings string)
  "Returns #t if at least one of the strings in the list
   <search-strings> are in in <string>, #f otherwise."
  (any? #L(string-search _ string) search-strings))

(define (string-contains-all-of? search-strings string)
  "Returns #t if all of the strings in the list <search-strings>
   are in in <string>, #f otherwise."
  (every? #L(string-search _ string) search-strings))


(define (break-lines string line-length :optional (hard-right-margin? #f))
  "Breaks <string> into a list of lines of length <line-length>, chosing
   line break points at whitespace characters. Runs of text longer than
   <line-length> with no suitable breakpoints will result in output lines
   longer than <line-length>, unless <hard-right-margin?> is set.
   <hard-right-margin?> forces breakpoints to be taken in the middle of
   runs, if they are required to honor <line-length>."
  (let ((ip (open-input-string string)))
    (define (read-segment)
      (read-text-until-character ip (negate char-whitespace?))
      (read-text-until-character ip char-whitespace?))
    (let loop ((lines ())
               (current-line (open-output-string))
               (segment (read-segment)))
        (cond
         ((= 0 (length segment))
          (reverse (cons (get-output-string current-line) lines)))
         ((< (+ (length segment) 1 (length current-line)) line-length)
          (loop lines
                (begin
                  (unless (= 0 (length current-line))
                    (write-strings current-line #\space))
                  (write-strings current-line segment))
                (read-segment)))
         ;; at this point, we know the current line overflows, so we can flush it to the line list
         ((> (length current-line) 0)
          (loop (cons (get-output-string current-line) lines)
                (open-output-string)
                segment))
         ((not hard-right-margin?)
          (loop (cons segment lines)
                current-line
                (read-segment)))
         ((> line-length (length segment))
          (loop lines
                (write-strings current-line segment)
                (read-segment)))
         (#t
          (loop (cons (substring segment 0 line-length) lines)
                current-line
                (substring segment line-length)))))))


(define (string-begins-with? string . potential-prefixes) ;TESTTHIS
  "Returns the specific prefix if <string> begins with one of
    <potential-prefixes>."
  (let loop ((remaining potential-prefixes))
    (if (null? remaining)
        #f
      (if (aand (string-search (car remaining) string) (= 0 it))
          (car remaining)
          (loop (cdr remaining))))))

; REVISIT: <delim> can be a regex, but this requires a regex package :-)

; REVISIT: Should these be split-string-* or string-split-*?
; REVISIT: allow split-string-* to take lists of multiple delimiters
(define (split-string-once string delim) ; TESTTHIS
 "Splits <string> at the leftmost instance of the delimiter <delim>. Two
 values are returned: the text before the delimiter and the text after the
 delimiter. If the delimiter was not found, the text after the delimiter is
 returned as #f (The empty string can also be returned if the delimiter is
 at the end of the input string."
 (aif (string-search delim string)
      (values (substring string 0 it)
              (substring string (+ (if (char? delim) 1 (length delim)) it)))
      (values string #f)))

(define (split-string-once-from-right string delim) ; TESTTHIS
 "Splits <string> at the rightmost instance of the delimiter <delim>. Two
 values are returned: the text before the delimiter and the text after the
 delimiter. If the delimiter was not found, the text before the delimiter is
 returned as #f (The empty string can also be returned if the delimiter is
 at the beginning of the input string."
 (aif (string-search-from-right delim string)
      (values (substring string 0 it)
              (substring string (+ (if (char? delim) 1 (length delim)) it)))
      (values #f string)))

(define (split-string string delim) ; TESTTHIS ; REVISIT: Allocates two strings per token, when it can be one
 "Splits <string> at each delimiter <delim>, returning a list of all
  substrings between each delimiter. There is an implicit delimiter
  at the end of the string."
 (let recur ((string string))
   (mvbind (before after) (split-string-once string delim)
     (if after
         (cons before (recur after))
         (cons before)))))

(define (string-replace string old new)
 "Replace every occurrence of the string <old> within <string> with <new>."
 (runtime-check string? string)
 (runtime-check string? old)
 (runtime-check string? new)
 (let ((os (open-output-string)))
   (let loop ((loc 0))
        (acond  ((string-search old string loc)
                 (write-strings os (substring string loc it))
                 (write-strings os new)
                 (loop (+ it (length old))))
               (#t
                (get-output-string (write-strings os (substring string loc))))))))

(define (string-leftmost string num)
  "Returns a string consisting of the leftmost <num> characters of <string>. If <num>
   is negative, it is a count of characters from the right end of the string. If <string>
   does not contain at least (abs <num>) characters, an error is thrown."
  (if (> num 0)
      (substring string 0 num)
      (substring string 0 (+ (length string) num))))

(define (string-rightmost string num)
  "Returns a string consisting of the rightmost <num> characters of <string>. If <num>
   is negative, it is a count of characters from the left end of the string. If <string>
   does not contain at least (abs <num>) characters, an error is thrown."
  (if (> num 0)
      (substring string (- (length string) num))
      (substring string (- num))))

(define (normalize-whitespace string)
  "Normalizes the whitespace in <string>; All whitespace characters are converted
   to spaces, and all runs of multiple spaces are reduced to single spaces."
  (let ((in-whitespace? #t))
    (get-output-string (string-fold (lambda (ch op)
                                      (cond ((char-whitespace? ch)
                                             (unless in-whitespace?
                                               (set! in-whitespace? #t)
                                               (write-strings op #\space))
                                             op)
                                            (#t
                                             (set! in-whitespace? #f)
                                             (write-strings op ch))))
                                    (open-output-string)
                                    string))))

(define (text->boolean text)
  "Coerce a text value into a boolean.  All string values are taken to be
   true, aside from 'no', 'n', 'false', 'f', '0', and the empty string. <text>
   can also be a character, in which case it is coerced to a string."
  (let recur ((text text))
    (etypecase text
      ((character)
       (recur (make-string 1 text)))
      ((string)
       (let ((text (string-downcase (string-trim text))))
         (not (member text '("no" "n" "false" "f" "0" ""))))))))
