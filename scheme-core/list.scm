;;;; list.scm
;;;; October 19th, 2007
;;;; Mike Schaeffer
;;
;; List utilities.

;; Parts of the list utilities are basically straight out of the
;; reference implementation of SRFI-1. The copyright of this code
;; is as follows:

;; SRFI-1 list-processing library                      -*- Scheme -*-
;; Reference implementation
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;     -Olin


;; This version is materially slower, but allows for possibly somewhat nicer
;; error messages.
;;
;; (define (null-list? xs :optional (error-list #f))
;;   "Determines if <xs> represents the end of a list, throwing an error
;;    if <xs> is an atom. This predicate is for detecting the end of a traversal
;;    over a list that cannot be dotted. <error-list> is the list to be displayed
;;    in the error message, which defaults to <xs> if no <error-list> is provided."
;;   (cond ((pair? xs) #f)
;;         ((null? xs) #t)
;;         (#t (error "Invalid list: ~s" (if error-list error-list xs)))))

(define (null-list? xs) ; Heaviliy SRFI-1
  "Determines if <xs> represents the end of a list, throwing an error
   if <xs> is an atom. This predicate is for detecting the end of a traversal
   over a list that cannot be dotted. <error-list> is the list to be displayed
   in the error message, which defaults to <xs> if no <error-list> is provided."
  (cond ((pair? xs) #f)
        ((null? xs) #t)
        (#t (error "Invalid list: ~s" xs))))

(define (end-of-list? xs)
  "An alias for null-list?." ;; REVISIT: Consolidate the two?
  (null-list? xs))

(define (->list x)
  "Ensures that <x> is a list by wrapping it in a list if it's not a pair."
  (if (pair? x) x (cons x)))

(define (list . xs)
  "Returns a list of all arguments."
  xs)

(define (list-ref xs index)
  (check (and exact? (>= 0)) index)
  (let loop ((pos xs) (ii 0))
    (cond ((null-list? pos)
           (error "List index out of range: ~s" index))
          ((= ii index)
           (car pos))
          (#t
           (loop (cdr pos) (+ ii 1))))))

(define (list-set! xs index val)
  (check (and exact? (>= 0)) index)
  (let loop ((pos xs) (ii 0))
    (cond ((null-list? pos)
           (error "List index out of range: ~s" index))
          ((= ii index)
           (set-car! pos val)
           xs)
          (#t
           (loop (cdr pos) (+ ii 1))))))

(define (first xs) (car xs))
(define (rest xs) (cdr xs))
(define (second xs) (cadr xs))
(define (third xs) (caddr xs))
(define (fourth xs) (cadddr xs))
(define (fifth xs) (cadddr (cdr xs)))

(define (length=0? xs) (null? xs))
(define (length=1? xs) (= (length xs) 1))
(define (length=2? xs) (= (length xs) 2))
(define (length=3? xs) (= (length xs) 3))
(define (length=4? xs) (= (length xs) 4))

(define (atom? x) (not (pair? x)))

(define (last xs)
  "Returns the last element of <xs>."
  (car (last-pair xs)))

(define (caar   x) (car (car x)))
(define (cddr   x) (cdr (cdr x)))
(define (cadr   x) (car (cdr x)))
(define (cdar   x) (cdr (car x)))
(define (caaar  x) (car (car (car x))))
(define (caddr  x) (car (cdr (cdr x))))
(define (caadr  x) (car (car (cdr x))))
(define (cadar  x) (car (cdr (car x))))
(define (cdaar  x) (cdr (car (car x))))
(define (cdddr  x) (cdr (cdr (cdr x))))
(define (cdadr  x) (cdr (car (cdr x))))
(define (cddar  x) (cdr (cdr (car x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))


(define (unsafe-list? xs)
  (while (pair? xs)
    (set! xs (cdr xs)))
  (null? xs))

(define (blithe-cdr x)
  (if (pair? x) (cdr x) '()))

(define (nth-cdr xs n)
  "Returns the result of calling cdr <n> times on <xs>. <n> must be a
   non-negative exact number. If it is not, an error is thrown."
  (check (and exact? (>= 0)) n)
  (let loop ((xs xs) (n n))
    (if (or (= 0 n) (null? xs))
        xs
        (loop (cdr xs) (- n 1)))))

(define (nth xs n)
  "Returns the <n>th element of the list <xs>. <n> must be a
   non-negative exact number. If it is not, an error is thrown."
  (car (nth-cdr xs n)))

(define (list? xs)
  (if (pair? xs)
      (let ((f (cdr xs))
            (s xs))
        (while (and (not (eq? f s))
                    (pair? s))
               (set! f (blithe-cdr (blithe-cdr f)))
               (set! s (cdr s)))
        (null? s))
      (null? xs)))

(define (dotted-list? xs)
  (if (pair? xs)
      (let ((f (cdr xs))
            (s xs))
        (while (and (not (eq? f s))
                    (pair? s))
               (set! f (blithe-cdr (blithe-cdr f)))
               (set! s (cdr s)))
        (not (null? s)))
      #f))


(defmacro (%a-list-search-let let-form a-list-search-form a-list bindings . body-forms)
  (define (binding-form binding)
    (let ((variable (first binding))
          (key (second binding))
          (default-value (third binding)))
      `(,variable (aif (,a-list-search-form ,key ,a-list) (cdr it) ,default-value))))
  (dolist (binding bindings)
    (unless (and (list? binding)
                 (symbol? (car binding))
                 (or (= 3 (length binding))
                     (= 2 (length binding))))
      (error "Invalid binding in assoc-let: ~a" binding)))
  `(,let-form ,(map binding-form bindings) ,@body-forms))

(defmacro (assoc-let a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assoc to search <a-list>."
  `(%a-list-search-let let assoc ,a-list ,bindings ,@body-forms))

(defmacro (ass-let a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses ass to search <a-list>."
  `(%a-list-search-let let ass ,a-list ,bindings ,@body-forms))

(defmacro (assq-let a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assq to search <a-list>."
  `(%a-list-search-let let assq ,a-list ,bindings ,@body-forms))

(defmacro (assv-let a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assv to search <a-list>."
  `(%a-list-search-let let assv ,a-list ,bindings ,@body-forms))

(defmacro (assoc-let* a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assoc to search <a-list>."
  `(%a-list-search-let let* assoc ,a-list ,bindings ,@body-forms))

(defmacro (ass-let* a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses ass to search <a-list>."
  `(%a-list-search-let let* ass ,a-list ,bindings ,@body-forms))

(defmacro (assq-let* a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assq to search <a-list>."
  `(%a-list-search-let let* assq ,a-list ,bindings ,@body-forms))

(defmacro (assv-let* a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assv to search <a-list>."
  `(%a-list-search-let let* assv ,a-list ,bindings ,@body-forms))

(defmacro (assoc-letrec a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assoc to search <a-list>."
  `(%a-list-search-let letrec assoc ,a-list ,bindings ,@body-forms))

(defmacro (ass-letrec a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses ass to search <a-list>."
  `(%a-list-search-let letrec ass ,a-list ,bindings ,@body-forms))

(defmacro (assq-letrec a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assq to search <a-list>."
  `(%a-list-search-let letrec assq ,a-list ,bindings ,@body-forms))

(defmacro (assv-letrec a-list bindings . body-forms)
  "Creates a set of local bindings to values in <a-list>. Each element of <bindings>
   is a list of the form (<variable> <assoc-key> <default-value>). Default value forms
   are not evaluated unless their values are needed. Uses assv to search <a-list>."
  `(%a-list-search-let letrec assv ,a-list ,bindings ,@body-forms))

;;; A high-speed queue.
;;
;; This doesn't check errors and uses macroexpansion to avoid
;; funcall overhead for the sake of speed. It's intended to be
;; a quick way to build lists head to tail.

(defmacro (%make-q) ;; REVISIT: the naming convention for the queue and 'q' functions is not consistent
  "Constructs a primitive queue object."
  `(let ((q-cons (cons)))
     (set-car! q-cons q-cons)
     q-cons))

(defmacro (%q-items q)
  "Returns a list of the queue items in the queue <q>."
  `(cdr ,q))

(defmacro (%q-enqueue! x q)
  "Enqueues a new item <x> into the queue <q>."
  `(let ((new-q-cons (cons ,x))
         (q ,q))
     (set-cdr! (car q) new-q-cons)
     (set-car! q new-q-cons)
     q))

(defmacro (%q-enqueue-list! xs q)
  "Enqueues a new list of items <xs> into the queue <q>, destructively altering <xs>."
  `(let ((new-q-entries ,xs)
         (q ,q))
     (unless (null? new-q-entries)
       (set-cdr! (car q) new-q-entries)
       (set-car! q (last-pair new-q-entries)))
     q))

(defmacro (%q-empty? q)
  "Returns a boolean indicating if the queue <q> is empty."
  `(null? (cdr ,q)))

(defmacro (%q-dequeue! q)
  "Removes the next element from the queue <q>. Returns () if <q>
   is empty."
  `(let ((q ,q))
     (cond ((%q-empty? q)
            ())
           (#t
            (let ((value (cadr q)))
              (when (null? (cddr q))
                (set-car! q q))
              (set-cdr! q (cddr q))
              value)))))

;;; This is the slower, safer version of the above.

(define (make-queue) ;; REVISIT: This should use a structure
  "Constructs a queue object."
  (cons :queue (%make-q)))

(define (queue? q)
  "Returns <q> if it is a queue, #f otherwise."
  (if (and (pair? q) (eq? :queue (car q)))
      q
      #f))

(define (q-items q)
  "Returns a list of the queue items in the queue <q>."
  (check queue? q)
  (%q-items (cdr q)))

(define (q-enqueue! x q)
  "Enqueues a new item <x> into the queue <q>."
  (check queue? q)
  (%q-enqueue! x (cdr q)))

(define (q-enqueue-list! xs q)
  "Enqueues a new list of items <xs> into the queue <q>, destructively altering <xs>."
  (check queue? q)
  (%q-enqueue-list! xs (cdr q)))

(define (q-dequeue! q)
  "Removes the next element from the queue <q>. Signals an error if <q> is
   empty."
  (check queue? q)
  (when (%q-empty? (cdr q))
    (error "Cannot dequeue element from empty queue."))
  (%q-dequeue! (cdr q)))

(define (q-empty? q)
  "Returns a boolean indicating if the queue <q> is empty."
  (check queue? q)
  (%q-empty? (cdr q)))

(define (member x xs)
  "Checks if <x> is a member of the list <xs> based on the equality
   predicate equal?. If <x> is not found returns #f, otherwise returns
   the first sublist of <xs> whose car is <x>."
  (cond ((null-list? xs) #f)
        ((equal? x (car xs)) xs)
        (#t (member x (cdr xs)))))

(define (memv x xs)
  "Checks if <x> is a member of the list <xs>, based on the equality
   predicate eqv?. If <x> is not found returns #f, otherwise returns
   the first sublist of <xs> whose car is <x>."
  (cond ((null-list? xs)  #f)
        ((eqv? x (car xs)) xs)
        (#t (memv x (cdr xs)))))

(define (memq x xs)
  "Checks if <x> is a member of the list <xs>, based on the equality
   predicate eq?. If <x> is not found, returns #f, otherwise returns
   the first sublist of <xs> whose car is <x>."
  (cond ((null-list? xs)  #f)
        ((eq? x (car xs)) xs)
        (#t (memq x (cdr xs)))))

(define (list-index pred? xs)
  "Returns the 0-based index of the first element of <xs> that
   satifsies <pred?>. Returns #f if not found."
  (let loop ((ii 0) (xs xs))
    (cond ((null-list? xs) #f)
          ((pred? (car xs)) ii)
          (#t (loop (+ ii 1) (cdr xs))))))

(define (member-index x xs)
  "Returns the 0-based index of the first element of <xs> that
   is equal? to <x>. Returns #f if not found."
  (list-index #L(equal? _ x) xs))

(define (memv-index x xs)
  "Returns the 0-based index of the first element of <xs> that
   is eqv? to <x>. Returns #f if not found."
  (list-index #L(eqv? _ x) xs))

(define (memq-index x xs)
  "Returns the 0-based index of the first element of <xs> that
   is eq? to <x>. Returns #f if not found."
  (list-index #L(eq? _ x) xs))

(define (reverse xs)
  "Reverses the elements in the list <xs>. The original list <xs>
   remains unaltered."
  (let loop ((new ()) (xs xs))
    (cond ((null-list? xs)
           new)
          (#t
           (loop (cons (car xs) new) (cdr xs))))))

(define (reverse! xs)
  "Reverses the elements in the list <xs>. The original list <xs>
   is desctructively altered."
  (let loop ((new ()) (xs xs))
    (cond ((null-list? xs)
           new)
          (#t
           (let ((next (cdr xs)))
             (set-cdr! xs new)
             (loop xs next))))))

(define (qsort xs less? :optional (key identity))
  "Sorts the list <xs> into the order imposed by the comparison
   predicate <less?>. For any two elements of <xs>, x and y,
   x will appear before y in the result list if x and y satisfy
   (less? x y). <less?> should never return different values on
   subsequent calls with the same arguments. <key> is an optional
   accessor function used to access the sort key for list elements. If
   specified, the ordering function for two elements x and y is actually
   (less? (key x) (key y)). If <key> is passed as a symbol, it is taken
   to be a slot name suitable for use with slot-ref."
  (let ((key (if (symbol? key) #L(slot-ref _ key) key)))
    (define (sort-step xs)
      (if (null? xs)
          ()
          (let* ((pivot-index (random (length xs)))
                 (pivot-value (list-ref xs pivot-index)))
            (let loop ((less ()) (greater ()) (index 0) (xs xs))
              (cond ((= index pivot-index)
                     (loop less greater (+ 1 index) (cdr xs)))
                    ((null? xs)
                     (nconc (sort-step less) (cons pivot-value (sort-step greater))))
                    ((less? (key (car xs)) (key pivot-value))
                     (loop (cons (car xs) less) greater (+ index 1) (cdr xs)))
                    (#t
                     (loop less (cons (car xs) greater) (+ index 1) (cdr xs))))))))
    (check list? xs)
    (check procedure? less?)
    (check procedure? key)
    (sort-step xs)))

(define (nconc xs ys)
  "Destructively appends <xs> and <ys>."
  (cond ((null? xs)
         ys)
        (#t
         (set-cdr! (last-pair xs) ys)
         xs)))

(define (last-pair xs)
  "Finds the last pair of the list <xs>."
  (check (or pair? null?) xs)
  (let loop ((xs xs))
    (let ((tail (cdr xs)))
      (if (pair? tail)
          (loop tail)
          xs))))

(define (list-combinations lists)
  "Given a list of sublists, return a list of every combination of single
   elements from each sublist. "
  (if (null? (cdr lists))
      (map list (car lists))
      (let ((cdr-combinations (list-combinations (cdr lists))))
        (append-map (lambda (list-item)
                      (map (lambda (cdr-combination)
                             (cons list-item cdr-combination))
                           cdr-combinations))
                    (car lists)))))

(define (circular-list . xs)
  "Construct a circular list from the argument list."
  (let ((xs (list-copy xs)))
    (unless (null? xs)
      (set-cdr! (last-pair xs) xs))
    xs))

(define (butlast xs)
  "Make a duplicate copy of <xs> containing all but the final list element."
  (let loop ((xs xs) (accum ()))
    (cond ((null-list? (cdr xs))
           (reverse! accum))
          (#t
           (loop (cdr xs) (cons (car xs) accum))))))

(define (append-map! f . xss)
  "Apply the function <f> to the lists in <xss>. The first list in <xss>
   provides the first argument to <f>, the second list the second argument,
   and so on. The shortest list in <xss> determines the number of applications
   of <f>.  Each application of <f> must result in either a list or null, which
   are then destructively append!'ed together to make the return value of append-map."
  (let ((items (%make-q)))
    (dolist (map-results (apply map f xss))
      (%q-enqueue-list! map-results items))
    (%q-items items)))


(define (append-map f . xss)
  "Apply the function <f> to the lists in <xss>. The first list in <xss>
   provides the first argument to <f>, the second list the second argument,
   and so on. The shortest list in <xss> determines the number of applications
   of <f>.  Each application of <f> must result in either a list or null, which
   are then non-destructively append'ed together to make the return value of append-map."
  (let ((items (%make-q)))
    (dolist (map-results (apply map f xss))
      (%q-enqueue-list! (list-copy map-results) items))
    (%q-items items)))


(define (drop xs n)
  "Returns all but the first <n> elements of <xs>."
  (cond ((zero? n)  xs)
        ((null? xs) ())
        (#t         (drop (cdr xs) (- n 1)))))

(define (take xs n)
  "Returns a new list consisting of the first <n> elements of
   the list <xs>. Throws an error if there are not that many
   elements in the list"
  (check integer? n)
  (let recur ((remaining-xs xs) (still-need n))
    (cond ((zero? still-need)   ())
          ((null? remaining-xs) (error "Cannot take ~s items from ~s" n xs))
          (#t                   (cons (car remaining-xs)
                                      (recur (cdr remaining-xs) (- still-need 1)))))))

(define (take-up-to xs n)
  "Returns a new list consisting of the first <n> elements of
   the list <xs>. Does not throw an error if there are not that
   many  elements in the list"
  (check integer? n)
  (let recur ((remaining-xs xs) (still-need n))
    (cond ((or (zero? still-need)
               (null? remaining-xs))
           ())
          (#t                   (cons (car remaining-xs)
                                      (recur (cdr remaining-xs) (- still-need 1)))))))

(define (take! xs n)
  "Returns a list consisting of the first <n> elements of the list
   <xs>. <xs> is destructively altered to return this list."
  (check integer? n)
  (if (zero? n)
      '()
      (begin
        (set-cdr! (drop xs (- n 1)) '())
        xs)))

(define (take-while pred? xs)
  "Returns a new list consisting of every item in <xs> up until
   the first item that does not satisfy <pred?>."
  (check procedure? pred?)
  (let recur ((xs xs))
    (if (null-list? xs)
        '()
        (let ((x (car xs)))
          (if (pred? x)
              (cons x (recur (cdr xs)))
              '())))))

(define (take-until-dot xs)
  "Returns a new list consisting of every item in <xs>, aside from any
   atom in the last cdr."
  (let recur ((xs xs))
    (if (or (null? xs) (atom? xs))
        ()
        (cons (car xs) (recur (cdr xs))))))

(define (take-while! pred? xs)
  "Returns a new list consisting of every item in <xs> up until
   the first item that does not satisfy <pred?>. <xs> is desctructively
   altered to return this list."
  (check procedure? pred?)
  (if (or (null-list? xs)
          (not (pred? (car xs))))
      '()
      (begin
        (let loop ((prev xs) (rest (cdr xs)))
          (if (pair? rest)
              (let ((x (car rest)))
                (if (pred? x)
                    (loop rest (cdr rest))
                    (set-cdr! prev '())))))
        xs)))

(define (take-until-dot! xs)
  "Returns a new list consisting of every item in <xs>, aside from any
   atom in the last cdr. <xs> is desctructively altered to return this
   list."
  (if (or (null? xs)
          (atom? xs))
      ()
      (let loop ((prev xs) (rest (cdr xs)))
        (cond ((atom? rest)
               (set-cdr! prev ())
               xs)
              ((pair? rest)
               (loop rest (cdr rest)))
              (#t
               xs)))))


(define (drop-while pred? xs)
  "Returns the tail of <xs>, starting with the first item that does
   not satisfy <pred?>."
  (check procedure? pred?)
  (let loop ((xs xs))
    (if (null-list? xs)
        '()
        (if (pred? (car xs))
            (loop (cdr xs))
            xs))))

(define (filter pred xs)
  "Returns a list of all objects in the list <xs> that satisfy predicate <pred>."
  (let ((filtered-xs '()))
    (dolist (x xs)
      (when (pred x)
        (push! x filtered-xs)))
    (reverse! filtered-xs)))

(define (remove pred lis)
  "Removed all object from list <lis> that satisfy predicate <pred>."
  (filter (lambda (x) (not (pred x))) lis))

(define (delete x xs :optional (test? equal?)) ; TEST
  "Returns <xs> with all instances of <x> deleted. <test?> is the equality predicate
   used to identity instances of <x>."
  (filter (lambda (y) (not (test? x y))) xs))

(UNIMPLEMENTED delete! "Copy from non-destructive (delete...)") ; REVISIT

(define (span pred? xs)
  "Returns two lists, the longest initial prefix of <xs> in which every
   item satifies <pred?>, and the rest of the list."
  (check procedure? pred?)
  (let recur ((xs xs))
    (if (null-list? xs)
        (values '() '())
        (let ((x (car xs)))
          (if (pred? x)
              (values-bind (recur (cdr xs)) (prefix suffix)
                (values (cons x prefix) suffix))
              (values '() xs))))))

(define (span! pred? xs)
  "Returns two lists, the longest initial prefix of <xs> in which every
   item satifies <pred?>, and the rest of the list. <xs> is destructively
   altered."
  (check procedure? pred?)
  (if (or (null-list? xs) (not (pred? (car xs)))) (values '() xs)
      (let ((suffix (let lp ((prev xs) (rest (cdr xs)))
                      (if (null-list? rest) rest
                          (let ((x (car rest)))
                            (if (pred? x) (lp rest (cdr rest))
                                (begin (set-cdr! prev '())
                                       rest)))))))
        (values xs suffix))))


(define (break pred? xs)
  "Returns two lists, the longest initial prefix of <xs> in which every
   item does not satifie <pred?>, and the rest of the list."
  (span  (lambda (x) (not (pred? x))) xs))

(define (break! pred? xs)
  "Returns two lists, the longest initial prefix of <xs> in which every
   item does not satifie <pred?>, and the rest of the list. <xs> is
   destructively altered."
  (span! (lambda (x) (not (pred? x))) xs))

(define (insert-ordered lis x :optional (lt? <) (selector identity))
  "Inserts <x> into <lis>, maintaining the order enforced by <lt?>. <selector> is
   a predicate that selects the subpart of list objects to be compared. <selector>
   can also be a symbol, in which case it is assumed to be a slot name."
  (let ((selector (if (symbol? selector) #L(slot-ref _ selector) selector)))
    (let loop ((lis lis))
      (cond ((null-list? lis)
             (cons x))
            ((lt? (selector x) (selector (car lis)))
             (cons x lis))
            (#t
             (cons (car lis) (loop (cdr lis))))))))

;; !! define insert-ordered!

(define (find pred? xs :optional (default #f))
  "Returns the first element of <xs> that satisfies <pred?>. If no element
   satisfies <pred?>, the function returns <default>."
  (let ((tail (find-tail pred? xs)))
    (if (not (null? tail))
        (car tail)
        default)))

(define (find-tail pred? xs)
  "Returns the sublist of <xs> beginning with the first element that
   satisfies <pred?>. If there's no such element, returns ()."
  (if (null? xs)
      '()
      (if (pred? (car xs))
          xs
          (find-tail pred? (cdr xs)))))

(define (exists? y ys)
  "Determine if <y> exists, determined by eq?, in <ys>. Returns <y>, if so, #f otherwise."
  (find (lambda (x) (eq? x y)) ys))


(define (alist key value . rest)
  "Create an alist from an arbitrary (non-zero) number of <key>/<value>
   pairs passed as arguments."
  (cons (cons key value)
        (if (null? rest)
             ()
             (apply alist rest))))

(define (minimal-alist elts :optional (in-list? assoc))
  "Non-destructively computes the 'minimal' representation of
   the alist <elts>. The minimal representation has no duplicate
   keys, leaving only the 'current' bindings.  <in-list?> determines
   if a key is a duplicate. No guarantees are made about the order
   of the result list."
  (let loop ((new ()) (elts elts))
    (cond ((null? elts)
           new)
          ((or (atom? elts) (atom? (car elts)))
           (error "Malformed alist: ~s" elts))
          ((in-list? (caar elts) new)
           (loop new (cdr elts)))
          (#t
           (loop (cons (car elts) new) elts)))))


(define (alist-cons key value alist)
  "Returns the a-list created by adding the <key>,<value> pair to the
   begining <alist>."
  (cons (cons key value) alist))

(define (alist-copy alist)
  "Makes a duplicate copy of the backbone of <alist>."
  (map (lambda (elt) (cons (car elt) (cdr elt))) alist))

(define (alist-delete key alist :optional (test? equal?))
  "Returns <alist> with all instances of <key> deleted. <test?> is the equality predicate
   used to identity instances of <key>."
  (filter (lambda (elt) (not (test?? key (car elt)))) alist))

;; !! split-at
;; !! split-at!
;; !! n-arity fold
;; !! reduce
;; !! reduce-next

(define (fold-right kons knil l)
  "The right-associative version of the fundamental list iterator.
   fold-right applies <kons> to each element of <l> and the result
   of a call to fold-right on the rest of the list. fold-right with
   a null list returns <knil>. fold-right is not tail recursive."
  (if (null? l)
      knil
      (kons (car l) (fold-right kons knil (cdr l)))))

(define (pair-fold-right kons knil l)
  "The right-associative version of the fundamental list iterator.
   fold-right applies <kons> to each sublist of <l> and the result
   of a call to fold-right on the rest of the list. fold-right with
   a null list returns <knil>. fold-right is not tail recursive."
  (if (null? l)
      knil
      (kons l (pair-fold-right kons knil (cdr l)))))

(define (fold kons knil l)
  "The fundamental list iterator. fold applies <kons> to each element
   of <l> and the result of a recursive call to fold on the remainder
   of <l>. If <l> is null, the return value is <knil>. fold is tail
   recursive."
  (if (null? l)
      knil
      (fold kons (kons (car l) knil) (cdr l))))

(define (pair-fold kons knil l)
  "The fundamental list iterator. fold applies <kons> to each sublist
   of <l> and the result of a recursive call to fold on the remainder
   of <l>. If <l> is null, the return value is <knil>. fold is tail
   recursive."
  (if (null? l)
      knil
      (let ((tail (cdr l)))
        (pair-fold kons (kons l knil) tail))))

(defmacro (push! item stack)
  `(set! ,stack (cons ,item ,stack)))

(defmacro (pop! stack)
  (with-gensyms (tos-sym)
    `(let ((,tos-sym (car ,stack)))
       (set! ,stack (cdr ,stack))
       ,tos-sym)))


(define (iseq from to)
  "Returns an ordered list of the whole numbers in the range [<from>, <to>]."
  (let loop ((current to) (accum ()))
    (if (< current from)
        accum
        (loop (- current 1) (cons current accum)))))

(define (flatten xs)
  "Flattens the list <xs> by removing nested list structure.  The
   items of the resultant list appear in the same order as they
   do in <xs>, but all in the same list."
  (cond ((null? xs) '())
        ((atom? xs) (cons xs))
        ((list? xs) (append-map! flatten xs))
        ((pair? xs) (cons (car xs) (flatten (cdr xs))))
        (#t (error "Invalid parameter to flatten: ~s" xs))))

(define (partition pred? xs)
  (let loop ((rest xs) (in ()) (out ()))
    (cond ((null? rest) (list in out))
          ((pred? (car rest)) (loop (cdr rest) (cons (car rest) in) out))
          (#t (loop (cdr rest) in (cons (car rest) out))))))

(define (recursive-list-copy xs)
  "Copies the list <xs>, recursively copying nested lists satisfying list?."
  (cond ((list? xs)
         (map recursive-list-copy xs))
        (#t
         xs)))


(define (iota count :optional (start 0) (step 1))
  (check (>= 0) count)
  (let loop ((count count) (val (+ start (* (- count 1) step))) (ans ()))
    (if (<= count 0)
        ans
        (loop (- count 1) (- val step) (cons val ans)))))

(defmacro (a-list-set! a-list key value)
  "Destructively updates the value bound to <key> in the association
    list <a-list>. The new value is <value>."
  `(begin
     (aif (assoc ,key ,a-list)
          (set-cdr! it ,value)
          (push! (cons ,key ,value) ,a-list))
     ,a-list))


(define (random-list-element xs)
  "Randomly selects an element of the list <xs>."
  (list-ref xs (random (length xs))))

(define (random-subsequence xs count)
  "Given a list or vector of items <xs>, return a random selection of <count>
   elements from that sequence."
  (check (or list? vector?) xs)
  (let* ((chosen-key (gensym "chosen"))
         (xs-vec (if (list? xs) (list->vector xs) (vector-copy xs))))
    (check (< (length xs)) count)
    (let choose-next ((chosen ())
                      (number-left count))
      (if (= number-left 0)
          (if (list? xs) chosen (list->vector chosen))
          (let* ((idx (random (length xs-vec)))
                 (choice (vector-ref xs-vec idx)))
            (if (eq? choice chosen-key)
                (choose-next chosen number-left)
                (begin
                  (vector-set! xs-vec idx chosen-key)
                  (choose-next (cons choice chosen) (- number-left 1)))))))))


(define (any? pred? xs)
  "Searches for the first value in <xs> that satifies <pred?>, and
    returns the value returned by <pred?>. If <pred?> is never
    satisfied, then return #f."
  (cond ((null-list? xs) #f)
        (#t
         (aif (pred? (car xs))
              it
              (any? pred? (cdr xs))))))

(define (any-not? pred? xs)
  "Searches for the first value in <xs> that does not satisfy <pred>,
   and returns that value.  If <pred?> is always satisfied, then return #f."
  (cond ((null-list? xs) #f)
        (#t
         (aif (not (pred? (car xs)))
              (car xs)
              (any-not? pred? (cdr xs))))))

(define (every? pred? xs)
  "Checks to see that every element in <xs> satisfies <pred?>. If so,
    then it returns the return value from the last call to <pred?>. If
    not, returns #f."
  (cond ((null-list? xs) #t)
        ((null? (cdr xs)) (pred? (car xs)))
        ((pred? (car xs)) (every? pred? (cdr xs)))
        (#t #f)))

(define (cons* x . xs)
  "Constructs a list from (cons <x> <xs>), with the last list element
   going into the cdr of the last list cell."
  (if (null? xs)
      x
      (cons x (apply cons* xs))))

;;; REVISIT: Add unify?

;; REVISIT: need configurable equal?/eq? predicate for match? ?
;; REVISIT: Add ?? ?? key/value pattern var for matching 'extra slots'

(define (match? pat form)
  "Determines if <form> matches pattern <pat>.  A form matches a pattern if
   it can be equal? to the pattern with a consistent set of pattern variable
   bindings.  Pattern variables are variables in the pattern that can be bound to
   arbitrary values, as long as each instance of the same pattern variable is bound
   to the same thing. Pattern variables are denotes as symbols with #\\? as the
   first character of their name. Pattern variables with a symbol-name of \"??\"
   are special pattern variables that do not establish a binding; These pattern
   variables are universal and can match anything anywere.  Returns an a-list
   ((<var> . <val>) ... ) if <form> matches, and #f otherwise."
  (define (pattern-var? var)
    (and (symbol? var)
         (char=? (string-ref (symbol-name var) 0) #\?)))
  (define (universal-pattern-var? var)
    (and (pattern-var? var)
         (equal? (symbol-name var) "??")))
  (define (maybe-extend-env? env var val)
    (aif (assq var env)
         (if (equal? val (cdr it))
             env
             #f)
         (alist-cons var val env)))
  (let recur ((pat pat) (form form) (env ()))
    (cond ((not env)
           #f)
          ((pattern-var? pat)
           (if (universal-pattern-var? pat)
               env
               (maybe-extend-env? env pat form)))
          ((and (pair? pat) (pair? form))
           (let ((nenv (recur (car pat) (car form) env)))
             (recur (cdr pat) (cdr form) nenv)))
          ((and (structure? pat) (structure? form)
                (eq? (structure-type pat) (structure-type form)))
           (let loop ((slots (structure-slots pat)) (nenv env))
             (if (or (not env) (null? slots))
                 nenv
                 (loop (cdr slots) (recur (structure-slot-by-name pat (car slots))
                                          (structure-slot-by-name form (car slots))
                                          nenv)))))
          ((and (hash? pat) (hash? form))
           (let ((pat-keys (hash-keys pat)))
             (if (and (set-equivalent? pat-keys (hash-keys form) (hash-type pat))
                      (eq? (hash-type pat) (hash-type form)))
                 (let loop ((keys pat-keys) (nenv env))
                   (cond ((null? keys) nenv)
                         ((pattern-var? (car keys))
                          (error "Match pattern variables not allowed in hash keys: ~s" pat))
                         ((hash-has? form (car keys))
                          (loop (cdr keys)
                                (recur (hash-ref pat (car keys))
                                       (hash-ref form (car keys))
                                       nenv)))
                         (#t
                          #f)))
                 #f)))
          ((and (vector? pat) (vector? form)
                (= (length pat) (length form)))
           (let loop ((ii 0) (nenv env))
             (if (or (not nenv) (>= ii (length pat)))
                 nenv
                 (loop (+ ii 1) (recur (vector-ref pat ii) (vector-ref form ii) nenv)))))
          ((and (atom? pat) (atom? form)
                (equal? pat form))
           env)
          (#t
           #f))))


(define (p-list-fold kons knil l)
  "The fundamental list iterator, for Common Lisp style property lists. (Lists
   of the form (<key-1> <value-1> ... <key-n> <value-n>). p-list-fold applies
   <kons> to each key/value pair of <l> and the result of a recursive
   call to p-list-fold on the remainder of <l>. If <l> is null, the return
   value is <knil>. p-list-fold is tail recursive. For lists with an odd
   number of elements, the last value is taken to be NIL."
  (if (null? l)
      knil
    (p-list-fold kons (kons (car l) (cadr l) knil) (cddr l))))

(define (p-list->a-list p-list)
  (reverse! (p-list-fold (lambda (name value xs)
                           (cons (cons name value) xs))
                         () p-list)))

(define (vector-index pred? vec)
  "Returns the 0-based index of the first element of <vec> that
   satifsies <pred?>. Returns #f if not found."
  (let loop ((ii 0))
    (cond ((>= ii (length vec))   #f)
          ((pred? (vector-ref vec ii)) ii)
          (#t (loop (+ ii 1))))))


(define (duplicates xs :optional (same-elt? eq?))
  "Compute the set of duplicate elements in <xs>. An element is considered
   the same as another if <same-elt?> returns true when called on both. This
   function runs in O(n^2) time with respect to the length of <xs>."
  (let loop ((remaining xs) (dups ()) (seen ()))
       (cond ((null-list? remaining xs) dups)
             ((not (find #L(same-elt? _ (car remaining)) seen))
              (loop (cdr remaining) dups (cons (car remaining) seen)))
             ((find #L(same-elt? _ (car remaining)) dups)
              (loop (cdr remaining) dups seen))
             (#t
              (loop (cdr remaining) (cons (car remaining) dups) seen)))))

(define (duplicates? xs :optional (same-elt? eq?))
  "Compute the set of duplicate elements in <xs>, returning #f if there are none.
   An element is considered the same as another if <same-elt?> returns true when
   called on both. This function runs in O(n^2) time with respect to the length
   of <xs>."
  (let ((dups (duplicates xs same-elt?)))
    (if (null? dups) #f dups)))

;; REVISIT: delete-duplicates depends on delete returning a value eq? to its argument
;; if nothing is deleted. This currently does not hold true

;; (define (delete-duplicates xs :optional (same-elt? eq?)) ; Heavily from SRFI-1
;;   "Returns <xs> with all duplicates removed. An element is considered
;;    the same as another if <same-elt?> returns true when called on both. This
;;    function runs in O(n^2) time with respect to the length of <xs>. <xs>
;;    is itself unchanged, although it can possibly share structure with the
;;    return value."
;;   (let recur ((remaining xs))
;;     (watch-environment)
;;     (if (null-list? remaining xs)
;;         remaining
;;         (let* ((x (car remaining))
;;                (tail (cdr remaining))
;;                (new-tail (recur (delete x tail same-elt?))))
;;           (if (eq? tail new-tail)
;;               xs
;;               (cons x new-tail))))))

;; (define (delete-duplicates! xs :optional (same-elt? eq?)) ; Heavily from SRFI-1
;;   "Returns <xs> with all duplicates destructively removed. An element
;;    is considered the same as another if <same-elt?> returns true when
;;    called on both. This function runs in O(n^2) time with respect to the
;;    length of <xs>."
;;   (let recur ((remaining xs))
;;     (if (null-list? remaining xs)
;;         remaining
;;         (let* ((x (car remaining))
;;                (tail (cdr remaining))
;;                (new-tail (recur (delete! x tail same-elt?))))
;;           (if (eq? tail new-tail)
;;               xs
;;               (cons x new-tail))))))

(defmacro (dbind binding value . code)
  (define (find-dbind-binding-forms binding value)
    ;; REVISIT: no dbind specific typechecking. Does this matter?
    (cond ((symbol? binding)
           `((,binding ,value)))
          ((pair? binding)
           `(,@(find-dbind-binding-forms (car binding) `(car ,value))
             ;; REVISIT: this generates inefficient code with repeated calls to cdr
             ,@(find-dbind-binding-forms (cdr binding) `(cdr ,value))))
          ((vector? binding)
           (let loop ((ii 0))
             (if (= ii (length binding))
                 ()
                 `(,@(find-dbind-binding-forms (vector-ref binding ii) `(vector-ref ,value ,ii))
                   ,@(loop (+ ii 1))))))
          ((null? binding)
           ())
          (#t
           (error "Invalid dbind binding: ~s" binding))))
  (with-gensyms(value-sym)
   `(let ((,value-sym ,value))
      (let (,@(find-dbind-binding-forms binding value-sym))
        ,@code))))