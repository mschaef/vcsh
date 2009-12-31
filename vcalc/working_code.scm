; working_code.scm 
;
; Place to work with code for vcalc before integrating it into the
; main build.








(define-vcalc-command (import-csv-file)
  "Imports a CSV file and pushes the contents on the top of the stack."
  (awhen (choose-file *current-window* #t '(("Comma Seperated Data Files" . "*.csv") #t))
    (list (read-csv-file it))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Basic primitives


(define (string-accept-to-null string)
  "Returns the leftmost characters of <string>, up to but
   not including the first #\nul. If there is no #\nul,
   this function returns all of <string>"
  (aif (string-first-character string "\0")
       (substring string 0 it)
       string))

(define (copy-object-to-clipboard obj)
  (dynamic-let ((*seperator-mode* :none))
    (cond ((number? obj)
           (set-clipboard-data :CF_TEXT (vc-object->f-text obj)
                               "s-expression" (write-to-string obj)))
          ((list? obj)
           (set-clipboard-data "Csv" (list->csv-string obj)
                               "s-expression" (write-to-string obj)))
          (#t
           (set-clipboard-data "s-expression" (write-to-string obj))))))

(define (get-object-from-clipboard)
  (aif (get-clipboard-data "s-expression" "Csv" :CF_TEXT)
       (let ((format (car it))
             (data (string-accept-to-null (cdr it))))
         (cond ((equal? format "s-expression")
                (vc-read data))
               ((equal? format "Csv")
                (csv-string->list data))
               ((equal? format :CF_TEXT)
                (vc-read data))
               (#t (values))))
       (values)))

(define-vcalc-command (stack-cut x)
  "Cuts the top object from the stack and places it on the clipboard."
  (copy-object-to-clipboard x)
  (values))

(define-vcalc-command (stack-copy x)
  "Copies the top object from the stack to the clipboard."
  (copy-object-to-clipboard x)
  (values x))

(define-vcalc-command (stack-paste)
  "Pastes the object from the clipaboard onto the stack."
  (get-object-from-clipboard))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Online help logic (tip of the day and the keyboard map
;;; window)


(define (show-console-help-text)
  (display ";;; Welcome to the vCalc console.\n")
  (display ";;;\n")
  (display ";;; This window lets you evaluate arbitrary Scheme expressions.\n")
  (display ";;; Enter the expression you wish to evaluate and press <control+Enter>\n")
  (display ";;;\n")
  (display ";;; Sample Expressions:\n")
  (display ";;;    Hide the console window: (hide-console)\n")
  (display ";;;    Online help: (apropos \"map\")\n")
  (display ";;;    Add numbers: (+ 2 3)\n")
  (display ";;;\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu operations

(define (do-config w)
  (define (get-current-config-vector)
    (vector *angle-mode*
	    *seperator-mode*
	    *number-format-mode*
	    *number-precision*
	    *interest-accrual-mode*
	    *default-base*))
  (define (set-current-config-vector vec)
    (awhen (vector-ref vec 0)
      (set! *angle-mode* it))
    (awhen (vector-ref vec 1)
      (set! *seperator-mode* it))
    (awhen (vector-ref vec 2)
      (set! *number-format-mode* it))
    (awhen (vector-ref vec 3)
      (set! *number-precision* it))
    (awhen (vector-ref vec 4)
      (set! *interest-accrual-mode* it))
    (awhen (vector-ref vec 5)
      (set! *default-base* it))
      (update-window *current-window*))
  (let ((current-configuration (get-current-config-vector)))
    (set-current-config-vector (aif (show-config-dialog w
							current-configuration
							set-current-config-vector)
				    it
				    current-configuration))))

"vCalc Save Files (*.vcx)|*.vcx|All Files (*.*)|*.*||"



; !!! - read-line
; !!! - split
; !!! - string mapping/folding/iteration


(define (unique-eq xs)
  "Compute the set of unique, as distinguished by eq?, objects in the list <xs>"
  (let ((objects (make-hash :eq)))
     (for-each (lambda (x) (hash-set! objects x #f)) xs)
     (map car (hash->list objects))))

(define (unique-equal xs)
  "Compute the set of unique, as distinguished by equal?, objects in the list <xs>"
  (let ((objects (make-hash :equal)))
     (for-each (lambda (x) (hash-set! objects x #f)) xs)
     (map car (hash->list objects))))

(define (sort-numbers numbers) (qsort numbers < identity))

(define ir*-tenors '(1 M 2 M 3 M 6 M 9 M 12 M 18 M 2 Y 3 Y 4 Y 5 Y 7 Y 10 Y 15 Y 20 Y 25 Y 30 Y))

(define (tenors->days n unit . rest)
  (cons (* n (case unit ((M) 30) ((Y) 365))) (if (null? rest) '() (apply tenors->days rest))))

(define (count-chars str ch)
  "Count the numbers of times the character <ch> appears in the string <str>"
  (let ((is (open-input-string str)))
     (define (loop c)
       (if (port-at-end? is) 
          c
          (loop (if (eq? (read-char is) ch) (+ c 1) c))))
      (loop 0)))

(define (join strings sep)
  (let ((os (open-output-string)))
    (define (loop remaining need-sep?)
       (cond ((null? remaining)
               (get-output-string os))
             (#t
              (when need-sep? (display sep os))
              (display (car remaining) os)
              (loop (cdr remaining) #t))))
    (loop strings #f)))

(define (comma->bar if of)
  (with-file

; !!! - read-line
; !!! - split
; !!! - string mapping/folding/iteration

; !!! - unique on a given key-function
(define (unique-eq xs)
  "Compute the set of unique, as distinguished by eq?, objects in the list <xs>"
  (let ((objects (make-hash :eq)))
     (for-each (lambda (x) (hash-set! objects x #f)) xs)
     (map car (hash->list objects))))
(define (unique-equal xs)
  "Compute the set of unique, as distinguished by equal?, objects in the list <xs>"
  (let ((objects (make-hash :equal)))
     (for-each (lambda (x) (hash-set! objects x #f)) xs)
     (map car (hash->list objects))))
(define (sort-numbers numbers) (qsort numbers < identity))
(define ir*-tenors '(1 M 2 M 3 M 6 M 9 M 12 M 18 M 2 Y 3 Y 4 Y 5 Y 7 Y 10 Y 15 Y 20 Y 
25 Y 30 Y))
(define (tenors->days n unit . rest)
  (cons (* n (case unit ((M) 30) ((Y) 365))) (if (null? rest) '() (apply tenors->days 
rest))))
(define (count-chars str ch)
  "Count the numbers of times the character <ch> appears in the string <str>"
  (let ((is (open-input-string str)))
     (define (loop c)
       (if (port-at-end? is) 
          c
          (loop (if (eq? (read-char is) ch) (+ c 1) c))))
      (loop 0)))
(define (join strings sep)
  (let ((os (open-output-string)))
    (define (loop remaining need-sep?)
       (cond ((null? remaining)
               (get-output-string os))
             (#t
              (when need-sep? (display sep os))
              (display (car remaining) os)
              (loop (cdr remaining) #t))))
    (loop strings #f)))

(define (count xs x)
  (define (loop xs c)
    (cond ((null? xs) c)
          ((atom? xs) (error "Bad argument, ~a." xs))
          ((equal? x (car xs)) (loop (cdr xs) (+ c 1)))
          (#t (loop (cdr xs) c))))
  (loop xs 0))
(define (make-string obj count)
  "Create a string consisting of the display represntation of <obj>
   repeated <count> times."
  (let ((op (open-output-string)))
     (repeat count (display obj op))
     (get-output-string op)))
(define (pad-to-width obj width)
   "Return the display representation of <obj> padded or truncated
    to <width> characters. If padded, the result will be left justified
    with spaces. If truncated, the leftmost characters will be retained."
    (substring (string-append (make-string obj 1)
                              (make-string #\space width))
               0 width))
(define (transpose xs)
  (cond ((null? (car xs)) '())
        (#t
            (cons (map car xs)
                  (transpose (map cdr xs))))))
(define (group-by key-func xs)
  (let ((groups (make-hash :equal)))
     (define (push-item! x key)
       (unless (hash-has? groups key)
         (hash-set! groups key '()))
       (hash-set! groups key (cons x (hash-ref groups key))))
     (for-each (lambda (x) (push-item! x (key-func x))) xs)
     (map cdr (hash->list groups))))
     
(define (partition pred? xs)
  (define (loop rest in out)
     (cond ((null? rest) (list in out))
           ((pred? (car rest)) (loop (cdr rest) (cons (car rest) in) out))
           (#t (loop (cdr rest) in (cons (car rest) out)))))
  (loop xs '() '()))
  
  
  (define (table->instances table)
    (let ((base-instance  (make-instance '() 'class-name :imported-row))
           (slot-names (map (lambda (header-item) (intern! header-item "keyword")) (car table))))
       (for-each (lambda (slot) (slot-set! base-instance slot #f)) slot-names)
       (map 
         (lambda (row)
           (let ((instance (make-instance base-instance)))
              (for-each (lambda (slot-name slot-value)
                           (slot-set! instance slot-name slot-value))
                         slot-names row)
               instance))
         (cdr table))))
      
(define (table* xs) 
  (for-each (lambda (x) (format #t "~a\n" x)) xs) #f)

 
(table
  (qsort
     (filter (lambda (sym) 
                (and (symbol-bound? sym)
                     (primitive? (symbol-value sym))))
         (all-package-symbols "vcalc"))
     (lambda (x y) (> (strcmp x y) 0))
     symbol-name ))

;;; FAST INSTANCE
(define (make-fi)
  (let ((slot-map (make-hash :eq)))
    (hash-set! slot-map :%count  0)
    (hash-set! slot-map :proto   0)
    (hash-set! slot-map :parent  1)
    (cons :fi (vector slot-map #f #f))))
(define (clone-fi fi)
  (cons :fi (vector-copy (cdr fi))))

; hash-copy
; symmetric hash->list list->hash


(define *default-font* '((:face . "Arial")
                         (:size . 12)
                         (:bold . #f)
                         (:italic . #f)
                         (:underline . #f)
                         (:color . (0 0 0))))

(define (cft ft)
  (compile-formatted-text ft *default-font*))

(define (intern-using hash object)
  "Intern <object> into hash-table <hash>, and return the interned
   value.  This guarantees that all objects identical by the hash
   table's comparison function map into the same object."
  (aif (hash-ref hash object) 
       it
       (begin
          (hash-set! hash object object)
          object)))

(define (intern-using hash object)
  "Intern <object> into hash-table <hash>, and return the interned
   value.  This guarantees that all objects identical by the hash
   table's comparison function map into the same object."
          (hash-set! hash object object)
          object))

(define *fonts* (make-hash :equal))
(define *colors* (make-hash :equal))

(define (attrs->font attrs)
    (list :font (cdr (assoc :face attrs))
                (cdr (assoc :size attrs))
                (cdr (assoc :bold attrs))
                (cdr (assoc :italic attrs))
                (cdr (assoc :underline attrs))))

(define (attrs->color attrs)
    (list :color (cdr (assoc :color attrs))))

(define (compile-formatted-text f-text attrs) 
   (define (next f-text attrs need-font? need-color? accum)
      (cond ((string? (car f-text))
             (cond (need-font?
                    (next f-text attrs #f need-color? (cons (cons :%font-change (attrs->font attrs)) accum)))
                   (need-color?
                    (next f-text attrs need-font? #f (cons (cons :%color-change (attrs->color attrs)) accum)))
                   (#t
                    (next (cdr f-text) attrs need-font? need-color? (cons (car f-text) accum)))))
            ((pair? (car f-text))
              (case (caar f-text)
                ((:face :bold :italic :underline :size)
                 (next (cdr f-text) (alist-cons (caar f-text) (cadar f-text) attrs) #t need-color? accum))
                ((:color)
                 (next (cdr f-text) (alist-cons (caar f-text) (cdar f-text) attrs) need-font? #t accum))
                ((:adjust-size)
                 (next (cdr f-text) (alist-cons :size (+ (cdr (assoc :size attrs)) (cadar f-text))
                                                 attrs) #t need-color? accum))
                ((:block)
                 (next (cdr f-text) attrs #t #t (append! (next (cdar f-text) attrs #f #f '()) accum)))
                (#t (error "Invalid formatting instruction [ ~a ]" (caar f-text)))))
             ((null? f-text) accum)
            (#t (error "Not formatted text [ ~a ]" f-text))))
   (reverse (next f-text attrs #t #t '())))
                               



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; gmr_analysis.scm - utility code for diffing two GMR feeds' CCDL,
;;;;; CMDL, and IRDL records

;;;; Work in the scheme package to get the more efficient math...

(in-package! "scheme")

; ...But we need vcalc's delimited file loader and file chooser.
(import! 'vcalc::read-delimited)
(import! 'vcalc::read-csv)
(import! 'vcalc::choose-file)
(import! 'vcalc::*stack-window*)

;;;; A simple query tool

(define *unbound-column* (gensym "unbound"))

(define (bound? value)
  (not (eq? value *unbound-column*)))

(define (row-assoc key list)
  (aif (assoc key list) (cdr it) *unbound-column*))

(define (column-symbol? symbol)
  (if (and (symbol? symbol)
           (char=? #\? (string-ref (symbol-name symbol) 0)))
      (intern! (substring (symbol-name symbol) 1) (symbol-package symbol))
      #f))

(define (expand-row-clause where-clause)
  (cond ((symbol? where-clause)
         (aif (column-symbol? where-clause)
              `(row-assoc ',it *current-record*)
              where-clause))
        ((list? where-clause)
         `(,(car where-clause) ,@(map expand-row-clause (cdr where-clause))))
        (#t where-clause)))

(defmacro (select columns list-form)
  `(map (lambda (*current-record*)
          (list ,@(map (lambda (column)
                         `(cons ',(aif (column-symbol? column) it column)
                                ,(expand-row-clause column)))
                       columns)))
        ,list-form))

(defmacro (where clause-form list-form)
  `(filter (lambda (*current-record*)
             ,(expand-row-clause clause-form))
           ,list-form))


(define (match-column-names row column-names)
  (define (unnamed-column-symbol id) (intern! (format #f "column-~a" id)))
  (define (loop row column-names id)
    (cond ((and (null? row) (null? column-names))
           '())
          ((null? row)
           (cons (cons (car column-names) '()) (loop '() (cdr column-names) (+ 1 id))))
          ((null? column-names)
           (cons (cons (unnamed-column-symbol id) (car row)) (loop (cdr row) '() (+ 1 id))))
          (#t
           (cons (cons (car column-names) (car row)) (loop (cdr row) (cdr column-names) (+ 1 id))))))
  (loop row column-names 1))

(define (table-values-only table)
  (map (cut map cdr <>) table))

;;;; The GMR Loader

(define (read-gmr-feed port)
  "Reads a GMR Trade Data feed from port <port>. Returns a list of a-lists, one for each
   row in the feed in the order in which it appears. The a-list keys are the column
   names from 'GMR_FEED 7-0 Release'"
  (define gmr-record-layouts
    '(("L" ; Standard records
       record-type org-unit legal-vehicle unique-id market-value vol-sd-size step-size fs-amt
       prod-code subprod-code opt-exercise-type opt-payoff-type currency-conversion-type fs-use-only
       smile-adjustment-method fs-type curr-1 curr-2 yield-curve sub-yield-curve issuer-type tenor-unit
       underlying-tenor-begin underlying-tenor-end vol-tenor-begin vol-tenor-end rate-basis
       equity-commodity-symbol equity-commodity-basket-id moneyness industry-code risk-rating
       issuer-domicile issuer-name debt-security-id base-yield-curve country of credit subrecord-count
       filler2 weight spot-sd-size commodity-group commodity-family commodity-product mkt-factor-domicile
       coupon-frequency compound-frequency vega-calc-method price-to-face-value-ratio distance-to-current-coupon
       collateral-type dividend-type)
      ("G" ; Grid initial records have the same layout as standard records
       "L")
      ("D" ; Short records
       record-type fs-type spot-shift vol-shift time-shift delta-pv delta gamma vega tenor-unit
       tenor-begin tenor-end component-index component-loading)
      ))
  (define (match-gmr-column-names row)
    (define (gmr-record-layout record-type)
      (aif (assoc record-type gmr-record-layouts)
           (if (string? (cadr it))
               (gmr-record-layout (cadr it))
               (cdr it))
           '()))
    (if (list? row)
        (match-column-names row (gmr-record-layout (car row)))
        row))
  (map match-gmr-column-names (read-delimited port #\| #\newline '(#\"))))

(define (read-feed)
  (aif (vcalc::choose-file vcalc::*stack-window* #t '(#t))
       (with-port p (open-input-file it)
             (read-gmr-feed p))
       '()))

;;;; The mapping loader

(define endur<->comsys-mapping (make-hash))

(define (read-mapping)
  (awhen (vcalc::choose-file vcalc::*stack-window* #t '(#t))
       (with-port p (open-input-file it)
                  (hash-clear! endur<->comsys-mapping)
                  (dolist (mapping (read-csv p))
                          (let ((endur-id (car mapping))
                                (comsys-id (cadr mapping)))
                            (hash-set! endur<->comsys-mapping `(:endur ,endur-id) `(:comsys ,comsys-id))
                            (hash-set! endur<->comsys-mapping `(:comsys ,comsys-id) `(:endur ,endur-id)))))))


(define (simplify-feed feed source-system)
  (select ((list `(,source-system ?unique-id)
                 ?equity-commodity-symbol
                 ?fs-type
                 ?underlying-tenor-end)
           (list (hash-ref endur<->comsys-mapping `(,source-system ?unique-id) :unmapped)
                 ?equity-commodity-symbol
                 ?fs-type
                 ?underlying-tenor-end)
           ?fs-amt)
          (where (member ?fs-type '("CCDL" "CMDL" "IRDL"))
                 feed)))

;a == (car *stack*) == gmr feed

(define (random-atom)
  (case (random 4)
    ((0) "foobarbaz")
    ((1) 12345)
    ((2) 1.234)
    ((3) #t)))

(define (make-2d-list rows cols)
  (let ((xss '()))
    (repeat rows
            (let ((xs '()))
              (repeat cols
                      (push! (random-atom) xs))
              (push! xs xss)))
    xss))

(define a (make-2d-list 100 100))



(with-port p (open-output-file "test2.sx")
           (gc) (port-bandwidth p (repeat 100 (write a p)))) ; 3.8MB/s

(with-port p (open-output-string)
           (gc) (port-bandwidth p (repeat 100 (write a p)))) ; 3.8MB/s

(with-port p (open-null-port)
           (gc) (port-bandwidth p (repeat 100 (write a p)))) ; 4.8MB/s
(with-port p (open-output-file "test2.csv")
           (gc) (port-bandwidth p (repeat 100 (write-csv a p)))) ; 860k/s

(with-port p (open-output-string)
           (gc) (port-bandwidth p (repeat 100 (write-csv a p)))) ; 880k/s

(with-port p (open-null-port)
           (gc) (port-bandwidth p (repeat 100 (write-csv a p)))) ; 926k/s


(with-port p (open-input-file "test2.sx")
  (gc) (port-bandwidth p 1 (repeat 100 (read p)) 1)) ; 3.5MB/s

(with-port p (open-input-file "test2.csv")
  (gc) (port-bandwidth p 1 (repeat 1 (read-csv p)) 1)) ; 87k/s



(define a (write-to-string (car *stack*)))


(define (wc filename)
  (with-port p (open-input-file filename)
             (let ((line-no 0))
               (while (not (port-at-end? p))
                      (read-line p)
                      (incr! line-no))
               line-no)))


(define (factorial/n-over-factorial/k n k)
  "Computes (/ (factorial <n>) (factorial <k>)) for
   (<= k n), taking care to avoid overflow."

  (define (loop n k) (if (= n k) 1 (* n (loop (- n 1) k))))

  (cond ((not (exact? n)) (error "Wrong type, expected exact: ~a" n))
        ((not (exact? k)) (error "Wrong type, expected exact: ~a" k))
        ((or (< n 0)) (error "Domain error, n<0: ~a" n))
        ((or (< k 0)) (error "Domain error, k<0: ~a" k))
        ((> k n) (error "Domain error, n<k: ~a ~a" n k))
        (#t (loop n k))))

(define (permutations number number-chosen)
  "Compute the possible number of permutations of <number-chosen>
   objects selected from a pool of <number> possibilities. This
   counts seperately each ordering of the set of chosen objects."
  (cond ((> number-chosen number) #f)
        (#t
         (factorial/n-over-factorial/k number (- number number-chosen)))))

(define (combinations number number-chosen)
  "Compute the possible number of combinations of <number-chosen>
   objects selected from a pool of <number> possibilities.  This
   does not count different orderings of the same set of chosen
   objects."
  (cond ((> number-chosen number) #f)
        (#t
         (scheme:quotient (factorial/n-over-factorial/k number (- number number-chosen))
                          (factorial number-chosen)))))







(define-vcalc-command (swap x y)
  "Swaps top two arguments on the stack.")

(define-method (swap (x fixnum flonum)
                     (y fixnum flonum))
  (+ x y))
