
(define (signature-list->english type-names)
  "Given a list of symbols, <type-names>, return an English string
   listing each element."
  (define (vowel? letter)
    (member letter '(#\a #\e #\i #\o #\u #\y #\A #\E #\I #\O #\U #\Y)))
  (define (type-name-connective type-name)
    (cond ((eq? type-name #t) "a")
          ((vowel? (string-ref (symbol-name type-name) 0)) "an")
          (#t "a")))  (let ((need-commas? (> (length type-names) 2))
        (need-and? (> (length type-names) 1))
        (op (open-output-string)))
    (let loop ((names type-names) (pos 0))
      (cond ((null? names) (get-output-string op))
            (#t
             (unless (or (symbol? (car names)) (eq? (car names) #t))
               (error "Type names must be symbols or #t: ~a" (car names)))
             (when (and (> pos 0) need-commas?) (display ", " op))

             (when (null? (cdr names))
               (when need-and?
                 (when (= pos 1) (display " " op))
                 (display "and " op)))
             (format op "~a ~a" (type-name-connective (car names)) (car names))
             (loop (cdr names) (+ 1 pos)))))))
