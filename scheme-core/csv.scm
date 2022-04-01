
;;;; csv.scm --
;;;;
;;;; CSV file import/export.
;;;;
;;;; (C) Copyright 2001-2022 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;;
;;;; See the file "LICENSE" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

(define-package "csv"
  (:uses "scheme")
  (:exports "write-delimited"
            "write-csv"
            "read-delimited"
            "read-csv"
            "read-csv-file"
            "list->csv-string"
            "csv-string->list"))

(define (write-delimited xss item-delimiter line-delimiter port)
  "Write the nested list <xss> to the port <port> as delimited text. Sublists of <xss>
   are delimited with <line-delimiter>, sub-items within sublists are delimited
   with <item-delimiter>.  Atomic elements of <xss> itself are considered to be
   one element sublists. String items are displayed with surrounding double quotes,
   booleans are written with #t=1 and #f=0, and complex numbers are written as two
   items, the real part preceeding the imaginary part."
  (define (write-delimited-item x)
    (cond ((boolean? x)
           (display (if x "1" "0") port))
          ((or (exact? x) (real? x))
           (display x port))
          ((complex? x)
           (format port "~a~a~a" (real-part x) item-delimiter (imag-part x)))
          ((or  (char? x) (symbol? x))
           (format port "\"~a\"" x))
          ((string? x)
           (write x port))
          (#t
           (display "#BAD_VALUE" port))))

  (define (write-delimited-line xs)
    (define (next-item xs need-seperator?)
      (unless (null? xs)
        (when need-seperator?
          (display item-delimiter  port))
        (write-delimited-item (car xs))
        (next-item (cdr xs) #t)))
    (if (atom? xs)
        (write-delimited-item xs)
        (next-item xs #f))
    (display line-delimiter port))

  (define (write-delimited-lines xss)
    (unless (null? xss)
      (write-delimited-line (car xss))
      (write-delimited-lines (cdr xss))))

  (write-delimited-lines xss))

(define (write-csv xss port)
  "Use write-delimited to write the nested lists <xss> to port <port>
   in the form of a CSV file."
  (write-delimited xss "," #\newline port))

(define (list->csv-string xss)
  (let ((p (open-output-string)))
    (write-csv xss p)
    (get-output-string p)))

(forward read-csv)

(define (csv-string->list string)
  (let ((p (open-input-string string)))
    (read-csv p)))

(define (read-delimited port item-delimiter line-delimiter literal-delimiter)

  (define (remove-surrounding-quotes s)
    "If <s> is a string, and has quotes in the first and last positions,
   then this function returns the <s> without theh quotes. Otherwise,
   <s> is returned unchanged."
    (if (and (string? s)
             (>= (length s) 2)
             (eq? (string-ref s 0) #\")
             (eq? (string-ref s (- (length s) 1)) #\"))
        (substring s 1 (- (length s) 1))
        s))

  (define (parse-delimited-item s)
    (aif (string->number s)
         it
         (remove-surrounding-quotes (string-trim s))))

  (let ((delimiters (list item-delimiter line-delimiter)))

    (define (delimited-port-tokens port)
      (define (read-token)
        (let ((read-buffer (open-output-string)))
          (let read-next-char ((in-literal? #f))
            (let ((ch (peek-char port)))
              (if (or (eof-object? ch)
                      (and (not in-literal?) (member ch delimiters)))
                  (get-output-string read-buffer)
                  (begin
                    (display (read-char port) read-buffer)
                    (read-next-char (not (eq? in-literal?
                                              (eq? ch literal-delimiter))))))))))
      (let read-next ((tokens ()))
        (if (port-at-end? port)
            tokens
            (let ((token (parse-delimited-item (read-token))))
              (read-next (cons (cons token (read-char port)) tokens))))))

    (define (split-tokens tokens)
      (let  next-token ((remaining tokens) (current-line ()) (lines ()))
        (cond ((null? remaining)
               (cons current-line lines))
              ((eq? (cdar remaining) line-delimiter)
               (next-token (cdr remaining) (cons (caar remaining)) (cons current-line lines)))
              (#t
               (next-token (cdr remaining) (cons (caar remaining) current-line) lines)))))

    (remove null?
            (split-tokens
             (delimited-port-tokens port)))))


(define (read-csv port) ; REVISIT: requires loading the entire CSV file at once.
  (read-delimited port #\, #\newline '#\" port))

(define (read-csv-file filename)
  (with-port p (open-file filename)
    (read-csv p)))
