
;;;; list.scm --
;;;;
;;;; Lists. Portions of this code come from the reference implementation
;;;; of SRFI-1.
;;;;
;;;; (C) Copyright 2001-2011 East Coast Toolworks Inc.
;;;; (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
;;;; (C) Portions Copyright 1998, 1999 by Olin Shivers.
;;;;
;;;; See the file "license.terms" for information on usage and
;;;; redistribution of this file, and for a DISCLAIMER OF ALL
;;;; WARRANTIES.

;; SRFI-1 list-processing library                      -*- Scheme -*-
;; Reference implementation
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;     -Olin

;; TODO: split-at, split-at!, n-arity fold, reduce, reduce-next, insert-ordered!

;;;; The core of list processing

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
  "Ensures that <x> is a list by wrapping it in a list if it's not
   a pair or null."
  (if (or (pair? x)
          (null? x))
      x
      (cons x)))

(define (list . xs)
  "Returns a list of all arguments."
  xs)

(define (atom? x)
  (not (pair? x)))

(define (last-pair xs)
  "Finds the last pair of the list <xs>."
  (let loop ((xs xs))
    (if (and (pair? xs) (pair? (cdr xs)))
        (loop (cdr xs))
        xs)))

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

(define (cons* x . xs)
  "Constructs a list from (cons <x> <xs>), with the last list element
   going into the cdr of the last list cell."
  (if (null? xs)
      x
      (cons x (apply cons* xs))))

;;;; List predicates

(define (blithe-cdr x)
  (if (pair? x) (cdr x) '()))

(define (unsafe-list? xs)
  (while (pair? xs)
    (set! xs (cdr xs)))
  (null? xs))

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

;;;; List indexing

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


;;;; Fast Queues

(defmacro (%make-q) ;; REVISIT: the naming convention for the queue and 'q' functions is not consistent
  "Constructs a primitive queue object."
  `(let ((q-cons (cons)))
     (set-car! q-cons q-cons)
     q-cons))

(defmacro (%q-items q)
  "Returns a list of the queue items in the queue <q>."
  `(cdr ,q))

(defmacro (%q-enqueue-cell! x q)
  "Enqueues a new cell <x> into the queue <q>."
  `(let ((new-q-cons ,x)
         (q ,q))
     (set-cdr! (car q) new-q-cons)
     (set-car! q new-q-cons)
     q))

(defmacro (%q-enqueue! x q)
  "Enqueues a new item <x> into the queue <q>."
  `(%q-enqueue-cell! (cons ,x) ,q))

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

;;;; Slow Queues

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

(define (make-list count :optional (initial ()))
  (check (and exact? (>= 0)) count)
  (let loop ((ii count) (accum ()))
    (if (= ii 0)
        accum
        (loop (- ii 1) (cons initial accum)))))

;;;; Append/Copy

(define (list-copy xs)
  "Return a duplicate copy of the list <xs>. Each cons cell in the backbone
   of the list is copied to form the new list."
  (let ((items (%make-q)))
    (let loop ((xs xs))
      (cond ((pair? xs)
             (%q-enqueue! (car xs) items)
             (loop (cdr xs)))
            ((atom? xs)
             (%q-enqueue-cell! xs items))))
    (%q-items items)))

(define (append . xss)
  (let ((items (%make-q)))
    (dolist (xs xss)
      (%q-enqueue-list! (list-copy xs) items))
    (%q-items items)))

(define (nconc xs ys)
  "Destructively appends <xs> and <ys>."
  (cond ((null? xs)
         ys)
        (#t
         (set-cdr! (last-pair xs) ys)
         xs)))

(define (append! . xss)
  (let ((items (%make-q)))
    (dolist (xs xss)
      (%q-enqueue-list! xs items))
    (%q-items items)))

(define (circular-list . xs)
  "Construct a circular list from the argument list."
  (let ((xs (list-copy xs)))
    (unless (null? xs)
      (set-cdr! (last-pair xs) xs))
    xs))

;;;; List element predicates

(define (every? pred? xs)
  "Checks to see that every element in <xs> satisfies <pred?>. If so,
    then it returns the return value from the last call to <pred?>. If
    not, returns #f."
  (let loop ((xs xs))
    (cond ((null-list? xs) #t)
          ((null? (cdr xs)) (pred? (car xs)))
          ((pred? (car xs)) (loop (cdr xs)))
          (#t #f))))

(define (any? pred? xs)
  "Searches for the first value in <xs> that satifies <pred?>, and
    returns the value returned by <pred?>. If <pred?> is never
    satisfied, then return #f."
  (let loop ((xs xs))
    (cond ((null-list? xs) #f)
          (#t
           (aif (pred? (car xs))
                it
                (loop (cdr xs)))))))

(define (any-not? pred? xs)
  "Searches for the first value in <xs> that does not satisfy <pred>,
   and returns that value.  If <pred?> is always satisfied, then return #f."
  (let loop ((xs xs))
    (cond ((null-list? xs) #f)
          (#t
           (aif (not (pred? (car xs)))
                (car xs)
                (loop (cdr xs)))))))

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

;;;; Map amd fold

(define map) ; forward

(define (cars+cdrs xs)
  (values (map car xs)
          (map cdr xs)))

(define (for-each fn . xss)
  (case (length xss)
    ((0) #f)
    ((1)
     (let loop ((xs (car xss)))
          (when (pair? xs)
            (fn (car xs))
                 (loop (cdr xs)))))
    (#t
     (let loop ((xss xss))
          (when (every? pair? xss)
            (mvbind (cars cdrs) (cars+cdrs xss)
              (apply fn cars)
              (loop cdrs)))))))


(define (map fn . xss)
  (case (length xss)
    ((0) ())
    ((1) (reverse! (let loop ((xs (car xss)) (accum ()))
                     (if (null? xs)
                         accum
                         (loop (cdr xs) (cons (fn (car xs)) accum))))))
    (#t  (reverse! (let loop ((xss xss) (accum ()))
                     (if (every? pair? xss)
                         (mvbind (cars cdrs) (cars+cdrs xss)
                           (loop cdrs (cons (apply fn cars) accum)))
                         accum))))))


(define (map-pair fn . xss)
  (case (length xss)
    ((0) ())
    ((1) (reverse! (let loop ((xs (car xss)) (accum ()))
                     (if (null? xs)
                         accum
                         (loop (cdr xs) (cons (fn xs) accum))))))
    (#t  (reverse! (let loop ((xss xss) (accum ()))
                     (if (every? pair? xss)
                         (mvbind (cars cdrs) (cars+cdrs xss)
                           (loop cdrs (cons (apply fn xss) accum)))
                         accum))))))


(define (fold-right kons knil l)
  "The right-associative version of the fundamental list iterator.
   fold-right applies <kons> to each element of <l> and the result
   of a call to fold-right on the rest of the list. fold-right with
   a null list returns <knil>. fold-right is not tail recursive."
  (let recur ((l l))
    (if (null? l)
        knil
        (kons (car l) (recur (cdr l))))))

(define (pair-fold-right kons knil l)
  "The right-associative version of the fundamental list iterator.
   fold-right applies <kons> to each sublist of <l> and the result
   of a call to fold-right on the rest of the list. fold-right with
   a null list returns <knil>. fold-right is not tail recursive."
  (let recur ((l l))
    (if (null? l)
        knil
        (kons l (recur (cdr l))))))

(define (fold kons knil l)
  "The fundamental list iterator. fold applies <kons> to each element
   of <l> and the result of a recursive call to fold on the remainder
   of <l>. If <l> is null, the return value is <knil>. fold is tail
   recursive."
  (let loop ((knil knil) (l l))
    (if (null? l)
        knil
        (loop (kons (car l) knil) (cdr l)))))

(define (pair-fold kons knil l)
  "The fundamental list iterator. fold applies <kons> to each sublist
   of <l> and the result of a recursive call to fold on the remainder
   of <l>. If <l> is null, the return value is <knil>. fold is tail
   recursive."
  (let loop ((knil knil) (l l))
    (if (null? l)
        knil
        (loop (kons l knil) (cdr l)))))

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

;;;; Search

(define (member x xs)
  "Checks if <x> is a member of the list <xs> based on the equality
   predicate equal?. If <x> is not found returns #f, otherwise returns
   the first sublist of <xs> whose car is <x>."
  (let loop ((xs xs))
    (cond ((null-list? xs) #f)
          ((equal? x (car xs)) xs)
          (#t (loop (cdr xs))))))

(define (memv x xs)
  "Checks if <x> is a member of the list <xs>, based on the equality
   predicate eqv?. If <x> is not found returns #f, otherwise returns
   the first sublist of <xs> whose car is <x>."
  (let loop ((xs xs))
    (cond ((null-list? xs)  #f)
          ((eqv? x (car xs)) xs)
          (#t (loop (cdr xs))))))

(define (memq x xs)
  "Checks if <x> is a member of the list <xs>, based on the equality
   predicate eq?. If <x> is not found, returns #f, otherwise returns
   the first sublist of <xs> whose car is <x>."
  (let loop ((xs xs))
    (cond ((null-list? xs)  #f)
          ((eq? x (car xs)) xs)
          (#t (loop (cdr xs))))))

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

(define (find-tail pred? xs)
  "Returns the sublist of <xs> beginning with the first element that
   satisfies <pred?>. If there's no such element, returns ()."
  (let loop ((xs xs))
    (cond ((null? xs)       ())
          ((pred? (car xs)) xs)
          (#t               (loop (cdr xs))))))

(define (find pred? xs :optional (default #f))
  "Returns the first element of <xs> that satisfies <pred?>. If no element
   satisfies <pred?>, the function returns <default>."
  (let ((tail (find-tail pred? xs)))
    (if (not (null? tail))
        (car tail)
        default)))

(define (exists? y ys)
  "Determine if <y> exists, determined by eq?, in <ys>. Returns <y>, if so, #f otherwise."
  (find (lambda (x) (eq? x y)) ys))

;;;; Sorting

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

(define (unique xs :keyword (:count count #f) (:equivalence equivalence :eq))
 (let ((counts (make-hash equivalence)))
   (define (update x)
     (hash-set! counts x (+ 1 (hash-ref counts x 0))))
   (dolist (x xs)
     (update x))
   (if count
       (hash->a-list counts)
       (hash-keys counts))))

(define (radix-sort xs less? :optional (key identity))
 (let ((buckets (make-hash :eq)))
   (dolist (x xs)
     (hash-push! buckets (key x) x))
   (append-map! cdr (qsort (hash->a-list buckets) less? car))))

(define (butlast xs)
  "Make a duplicate copy of <xs> containing all but the final list element."
  (let loop ((xs xs) (accum ()))
    (cond ((null-list? (cdr xs))
           (reverse! accum))
          (#t
           (loop (cdr xs) (cons (car xs) accum))))))



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

;;;; Take and drop

(define (drop xs n)
  "Returns all but the first <n> elements of <xs>."
  (let loop ((xs xs) (n n))
    (cond ((= n 0)    xs)
          ((null? xs) ())
          (#t         (loop (cdr xs) (- n 1))))))

(define (take xs n)
  "Returns a new list consisting of the first <n> elements of
   the list <xs>. Throws an error if there are not that many
   elements in the list"
  (check integer? n)
  (let loop ((remaining-xs xs) (accum ()) (still-need n))
    (cond ((= still-need 0)     (reverse! accum))
          ((null? remaining-xs) (error "Cannot take ~s items from ~s" n xs))
          (#t                   (loop (cdr remaining-xs)
                                      (cons (car remaining-xs) accum)
                                      (- still-need 1))))))

(define (take-up-to xs n)
  "Returns a new list consisting of the first <n> elements of
   the list <xs>. Throws an error if there are not that many
   elements in the list"
  (check integer? n)
  (let loop ((remaining-xs xs) (accum ()) (still-need n))
    (cond ((or (= still-need 0)
               (null? remaining-xs))  (reverse! accum))
          (#t                         (loop (cdr remaining-xs)
                                            (cons (car remaining-xs) accum)
                                            (- still-need 1))))))

(define (take! xs n)
  "Returns a list consisting of the first <n> elements of the list
   <xs>. <xs> is destructively altered to return this list."
  (check integer? n)
  (if (= n 0)
      '()
      (begin
        (set-cdr! (drop xs (- n 1)) '())
        xs)))

;; TODO: take-until
;; TODO: take-until!

(define (take-while pred? xs)
  "Returns a new list consisting of every item in <xs> up until
   the first item that does not satisfy <pred?>."
  (check procedure? pred?)
  (let loop ((xs xs) (accum ()))
    (if (null-list? xs)
        (reverse! accum)
        (let ((x (car xs)))
          (if (pred? x)
              (loop (cdr xs) (cons x accum))
            (reverse! accum))))))

(define (take-until-dot xs)
  "Returns a new list consisting of every item in <xs>, aside from any
   atom in the last cdr."
  (let loop ((xs xs) (accum ()))
    (if (or (null? xs) (atom? xs))
        (reverse! accum)
        (loop (cdr xs) 
              (cons (car xs) accum)))))

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


;;;; List filtering

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

(define (delq elem xs)
  (let recur ((xs xs))
    (cond ((null? xs)
           ())
          ((pair? xs)
           (if (eq? elem (car xs))
               (recur (cdr xs))
               (begin
                 (set-cdr! xs (recur (cdr xs)))
                 xs)))
          (#t
           (error "Improper list: ~s" xs)))))

;;;; List partitioning

(define (span pred? xs)
  "Returns two lists, the longest initial prefix of <xs> in which every
   item satifies <pred?>, and the rest of the list."
  (check procedure? pred?)
  (let recur ((xs xs))
    (if (null-list? xs)
        (values '() '())
        (let ((x (car xs)))
          (if (pred? x)
              (mvbind (prefix suffix) (recur (cdr xs))
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
   item does not satify <pred?>, and the rest of the list."
  (span  (lambda (x) (not (pred? x))) xs))

(define (break! pred? xs)
  "Returns two lists, the longest initial prefix of <xs> in which every
   item does not satify <pred?>, and the rest of the list. <xs> is
   destructively altered."
  (span! (lambda (x) (not (pred? x))) xs))


(define (partition pred? xs)
  (let loop ((rest xs) (in ()) (out ()))
    (cond ((null? rest) (list in out))
          ((pred? (car rest)) (loop (cdr rest) (cons (car rest) in) out))
          (#t (loop (cdr rest) in (cons (car rest) out))))))

;;;; Association Lists

(define (ass key a-list equality-predicate?)
  (let loop ((pos a-list))
    (cond ((null? pos)
           #f)
          ((pair? pos)
           (if (and (pair? (car pos))
                    (equality-predicate? key (caar pos)))
               (car pos)
               (loop (cdr pos))))
          (#t
           (error "improper a-list at ~s: ~s" pos a-list)))))

(define (assq key a-list)
  (ass key a-list eq?))

(define (assv key a-list)
  (ass key a-list eqv?))

(define (assoc key a-list)
  (ass key a-list equal?))

(define (alist . k/vs)
  "Create an alist from an arbitrary (non-zero) number of <key>/<value>
   pairs passed as arguments."
  (let recur ((k/vs k/vs))
    (if (null? k/vs)
        ()
        (cons (cons (car k/vs) (cadr k/vs))
              (recur (cddr k/vs))))))

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
  (filter (lambda (elt) (not (test? key (car elt)))) alist))

(defmacro (a-list-set! a-list key value)
  "Destructively updates the value bound to <key> in the association
    list <a-list>. The new value is <value>."
  `(begin
     (aif (assoc ,key ,a-list)
          (set-cdr! it ,value)
          (push! (cons ,key ,value) ,a-list))
     ,a-list))


;;;; Stack operations

(defmacro (push! item stack)
  `(set! ,stack (cons ,item ,stack)))

(defmacro (pop! stack)
  (with-gensyms (tos-sym)
    `(let ((,tos-sym (car ,stack)))
       (set! ,stack (cdr ,stack))
       ,tos-sym)))


;;;; List sequence generation

(define (iseq from to)
  "Returns an ordered list of the whole numbers in the range [<from>, <to>]."
  (let loop ((current to) (accum ()))
    (if (< current from)
        accum
        (loop (- current 1) (cons current accum)))))

(define (iota count :optional (start 0) (step 1))
  (check (>= 0) count)
  (let loop ((count count) (val (+ start (* (- count 1) step))) (ans ()))
    (if (<= count 0)
        ans
        (loop (- count 1) (- val step) (cons val ans)))))

;;;; List flatten/recursive copy

(define (flatten xs)
  "Flattens the list <xs> by removing nested list structure.  The
   items of the resultant list appear in the same order as they
   do in <xs>, but all in the same list."
  (let recur ((xs xs))
    (cond ((null? xs) '())
          ((atom? xs) (cons xs))
          ((list? xs) (append-map! recur xs))
          ((pair? xs) (cons (car xs) (recur (cdr xs))))
          (#t (error "Invalid parameter to flatten: ~s" xs)))))

(define (recursive-list-copy xs)
  "Copies the list <xs>, recursively copying nested lists satisfying list?."
  (let recur ((xs xs))
    (if (list? xs)
        (map recur xs)
        xs)))

;;;; Random list

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

;;;; Property list

(define (p-list-fold kons knil l)
  "The fundamental list iterator, for Common Lisp style property lists. (Lists
   of the form (<key-1> <value-1> ... <key-n> <value-n>). p-list-fold applies
   <kons> to each key/value pair of <l> and the result of a recursive
   call to p-list-fold on the remainder of <l>. If <l> is null, the return
   value is <knil>. p-list-fold is tail recursive. For lists with an odd
   number of elements, the last value is taken to be NIL."
  (let loop ((knil knil) (l l))
    (if (null? l)
        knil
        (loop (kons (car l) (cadr l) knil)
              (cddr l)))))

(define (p-list->a-list p-list)
  (reverse! (p-list-fold (lambda (name value xs)
                           (cons (cons name value) xs))
                         () p-list)))

;;;; Duplicate list element detection

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


;;;; Vector search (REVISIT: Move)

(define (vector-index pred? vec)
  "Returns the 0-based index of the first element of <vec> that
   satifsies <pred?>. Returns #f if not found."
  (let loop ((ii 0))
    (cond ((>= ii (length vec))   #f)
          ((pred? (vector-ref vec ii)) ii)
          (#t (loop (+ ii 1))))))
