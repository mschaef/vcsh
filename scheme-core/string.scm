;;;; string.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;
;; String utility functions

(define (string-case-fold x)
  (string-downcase (string-upcase x)))

(define (string>  x y)
  (> (strcmp x y) 0))

(define (string<  x y)
  (< (strcmp x y) 0))

(define (string>= x y)
  (>= (strcmp x y) 0))

(define (string<= x y)
  (<= (strcmp x y) 0))

(define (string=  x y)
  (= (strcmp x y) 0))

(define (string!= x y)
  (not (= (strcmp x y) 0)))

(define (string>-ci  x y)
  (> (strcmp (string-case-fold x)  (string-case-fold y)) 0))

(define (string<-ci  x y)
  (< (strcmp (string-case-fold x) (string-case-fold y)) 0))

(define (string>=-ci x y)
  (>= (strcmp (string-case-fold x) (string-case-fold y)) 0))

(define (string<=-ci x y)
  (<= (strcmp (string-case-fold x) (string-case-fold y)) 0))

(define (string=-ci  x y)
  (= (strcmp (string-case-fold x) (string-case-fold y)) 0))

(define (string!=-ci x y)
  (not (= (strcmp (string-case-fold x) (string-case-fold y)) 0)))

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
    (repeat count (display obj op))
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

(define (->string obj)
  "Coerces <obj> into a string, retrning the display representation of <obj>
   if necessary. Symbols are returned as their print names."
  (typecase obj
    ((string) obj)
    ((symbol) (symbol-name obj))
    (#t (format #f "~a" obj))))

(define (->text spec)
  "Coerces <spec> into text. If <spec> is a string, it is returned
   as-is. If it is anything else, it is looked up in the global text
   dictionary."
 (if (string? spec)
     spec
     (get-text spec)))

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
                    (display #\space current-line))
                  (display segment current-line))
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
                (display segment current-line)
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

; TODO: <delim> can be a regex, but this requires a regex package :-)

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
 (mvbind (before after) (split-string-once string delim)
   (if after
       (cons before (split-string after delim))
       (cons before))))


(define *text-dictionary* (make-hash :eq))

(define (set-text! id string)
  "Updates the global text dictionary to associate the text <string>
   with <id>. <id> must be a symbol, <string> must be a string. If
   the <id> already has an association, it is overwritten."
  (check symbol? id)
  (check string? string)
  (hash-set! *text-dictionary* id string)
  string)

(define (get-text id :optional (default #f))
  "Retrieves the string associated with <id> from the global text dictionary.
   <id> must be a symbol. If there is no such id, <default> is returned
   if it was specified. If there is no <default>, an error is thrown."
  (check symbol? id)
  (aif (hash-ref *text-dictionary* id default)
       it
       (error "String ~s not found in global text dictionary." id)))

(defmacro (define-text . defs)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(p-list-fold (lambda (id string rest)
                      (cons `(set-text! ',id ,string) rest))
                    ()
                    defs)))


(define (string-replace string old new)
 "Replace every occurrence of the string <old> within <string> with <new>."
 (check string? string)
 (check string? old)
 (check string? new)
 (let ((os (open-output-string)))
   (let loop ((loc 0))
        (acond  ((string-search old string loc)
                 (display (substring string loc it) os)
                 (display new os)
                 (loop (+ it (length old))))
               (#t
                (get-output-string (display (substring string loc) os)))))))


(define (normalize-whitespace string)
  "Normalizes the whitespace in <string>; All whitespace characters are converted
   to spaces, and all runs of multiple spaces are reduced to single spaces."
  (let ((in-whitespace? #t))
    (get-output-string (string-fold (lambda (ch op)
                                      (cond ((char-whitespace? ch)
                                             (unless in-whitespace?
                                               (set! in-whitespace? #t)
                                               (display #\space op))
                                             op)
                                            (#t
                                             (set! in-whitespace? #f)
                                             (display ch op))))
                                    (open-output-string)
                                    string))))

(define (text->boolean text)
  "Coerce a text value into a boolean.  All string values are taken to be
   true, aside from 'no', 'n', 'false', 'f', '0', and the empty string. <text>
   can also be a character, in which case it is coerced to a string."
  (etypecase text
    ((character)
     (text->boolean (make-string 1 text)))
    ((string)
     (let ((text (string-downcase (string-trim text))))
       (not (member text '("no" "n" "false" "f" "0" "")))))))