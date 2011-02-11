(define (columns->instance columns :optional (default-value #f)) ; REVISIT: specify base columns
  "Given a list of column names, <columns>, return an instance with the slots
   listed in <slots>, each bound to <default-value>."
 (let ((instance (make-instance)))
   (dolist (column columns)
     (slot-set! instance column default-value))
   instance))

(define (data->instance columns data-values :optional (base-instance #f))
  "Given a list of column names, <columns>, return an instance with the slots
   listed in <columns>, each bound to the corresponding value in <data-values>."
 (let ((instance (make-instance base-instance)))
   (iterate/r ((:list column columns)
               (:list data-value data-values)) ()
              (slot-set! instance column data-value)
              instance)))

(define (table->instances table)
  "Given a data table (a list of lists, where the first list is a list of field names
   for each subsequent list), return a list of instances with a slot for each field."
 (let* ((header (map intern-keyword! (first table)))
        (base (columns->instance header)))
   (map #L(data->instance header _ base) (rest table))))
